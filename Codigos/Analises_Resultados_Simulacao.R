library(readxl)
library(dplyr)
library(purrr)
library(ggplot2)
library(scales)

# Pasta contendo as nove planilhas
pasta = "." 

cenarios = c("A1", "A2", "A3",
              "B1", "B2", "B3",
              "C1", "C2", "C3")

arquivos = file.path(
  pasta,
  paste0("resultados_simulacoes_", cenarios, ".xlsx")
)

dados = purrr::map2_dfr(
  arquivos, cenarios,
  ~ readxl::read_excel(.x, sheet = .y) |>
      dplyr::mutate(Cenario = .y)
)

dados

# Filtrar DPMM e identificar as 12 configuracoes
dados_dp = dados |>
  filter(Metodo == "Processo_Dirichlet") |>
  mutate(
    Priori_alpha = case_when(
      near(a_alpha, 1) & near(b_alpha, 1)   ~ "Gamma(1,1)",
      near(a_alpha, 2) & near(b_alpha, 4)   ~ "Gamma(2,4)",
      near(a_alpha, 1) & near(b_alpha, 0.1) ~ "Gamma(1,0.1)",
      TRUE ~ NA_character_
    ),
    G0 = sub("^(G0-[1-4]).*$", "\\1", Config_G0)
  )

# ARI medio e desvio-padrao entre replicas
resumo_ari = dados_dp |>
  group_by(Cenario, Priori_alpha, G0) |>
  summarise(
    ARI_medio = mean(ARI, na.rm = TRUE),
    ARI_dp = sd(ARI, na.rm = TRUE),
    n_validos = sum(!is.na(ARI)),
    .groups = "drop"
  ) |>
  mutate(
    Cenario = factor(Cenario, levels = cenarios),
    Priori_alpha = factor(
      Priori_alpha,
      levels = c("Gamma(1,1)", "Gamma(2,4)", "Gamma(1,0.1)")
    ),
    # Inverter a ordem para G0-1 aparecer em cima no grafico
    G0 = factor(G0, levels = paste0("G0-", 4:1)),
    Rotulo = sub("\\.", ",", sprintf("%.2f", ARI_medio)),
    Cor_texto = if_else(ARI_medio >= 0.65, "white", "black")
  )

# Mapa de calor do ARI medio
grafico_ari = ggplot(
  resumo_ari,
  aes(x = Cenario, y = G0, fill = ARI_medio)
) +
  geom_tile(color = "white", linewidth = 0.55) +
  geom_vline(
    xintercept = c(3.5, 6.5),
    color = "grey45",
    linewidth = 0.4
  ) +
  geom_text(
    aes(label = Rotulo, color = Cor_texto),
    size = 2.8,
    fontface = "bold"
  ) +
  scale_color_identity() +
  scale_fill_gradient(
    low = "#F4F7FD",
    high = "royalblue",
    limits = c(min(0, min(resumo_ari$ARI_medio, na.rm = TRUE)), 1),
    name = "ARI médio",
    guide = guide_colorbar(
      title.position = "top",
      barwidth = grid::unit(7, "cm"),
      barheight = grid::unit(0.25, "cm")
    )
  ) +
  facet_grid(
    rows = vars(Priori_alpha),
    switch = "y"
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid = element_blank(),
    panel.spacing.y = grid::unit(0.20, "cm"),
    axis.text.x = element_text(
      face = "bold", size = 9
    ),
    axis.text.y = element_text(
      face = "bold", size = 8.5
    ),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.y.left = element_text(
      angle = 0,
      face = "bold",
      size = 8
    ),
    legend.position = "bottom",
    plot.margin = margin(3, 4, 2, 3, "mm")
  )

print(grafico_ari)

ggsave(
  file.path(pasta, "mapa_calor_ari_dpmm.pdf"),
  plot = grafico_ari,
  width = 7,
  height = 5.9,
  units = "in",
  bg = "white"
)


dados_dp = dados |>
  filter(Metodo == "Processo_Dirichlet") |>
  mutate(
    alpha_id = case_when(
      near(a_alpha, 1) & near(b_alpha, 1)   ~ 1L,
      near(a_alpha, 2) & near(b_alpha, 4)   ~ 2L,
      near(a_alpha, 1) & near(b_alpha, 0.1) ~ 3L,
      TRUE ~ NA_integer_
    ),
    g0_id = suppressWarnings(as.integer(
      sub("^G0-([1-4]).*$", "\\1", Config_G0)
    )),
    Modelo = paste0("DP", 4L * (alpha_id - 1L) + g0_id),
    # Particao representativa de Dahl, nao a media a posteriori
    K_para_grafico = as.numeric(K_Dahl_Livre)
  )

dados_classicos = dados |>
  filter(Metodo %in% c("KMeans", "GMM")) |>
  mutate(
    Modelo = if_else(Metodo == "KMeans", "K-means", "GMM"),
    K_para_grafico = as.numeric(K_estimado)
  )

dados_graficos = bind_rows(dados_classicos, dados_dp)

ordem_metodos = c("K-means", "GMM", paste0("DP", 1:12))
dados_graficos = dados_graficos |>
  mutate(
    Cenario = factor(Cenario, levels = cenarios),
    Modelo = factor(Modelo, levels = ordem_metodos)
  )



# Mapa de calor do desvio assinado de K
resumo_k = dados_graficos |>
  group_by(Cenario, Modelo) |>
  summarise(
    K_verdadeiro = first(K_verdadeiro),
    K_medio = mean(K_para_grafico, na.rm = TRUE),
    n_validos = sum(!is.na(K_para_grafico)),
    .groups = "drop"
  ) |>
  mutate(
    Desvio_K = K_medio - K_verdadeiro,
    # Inverte os niveis para K-means aparecer na primeira linha
    Modelo = factor(as.character(Modelo),
                    levels = rev(ordem_metodos)),
    Rotulo = gsub("\\.", ",", sprintf("%+.2f", Desvio_K)),
    Rotulo = if_else(abs(Desvio_K) < 0.005, "0,00", Rotulo)
  )

limite = max(1, ceiling(max(abs(resumo_k$Desvio_K), na.rm = TRUE)))

resumo_k = resumo_k |>
  mutate(
    Cor_texto = if_else(abs(Desvio_K) >= 0.53 * limite,
                        "white", "#26323C")
  )

# Grafico de mapa de calor do desvio assinado de k
grafico_k = ggplot(
  resumo_k,
  aes(x = Cenario, y = Modelo, fill = Desvio_K)
) +
  geom_tile(color = "white", linewidth = 0.55) +
  geom_vline(xintercept = c(3.5, 6.5),
             color = "grey50", linewidth = 0.45) +
  # Divisoes: K-means/GMM, DP1-4, DP5-8, DP9-12
  geom_hline(yintercept = c(4.5, 8.5, 12.5),
             color = "grey55", linewidth = 0.45) +
  geom_text(
    aes(label = Rotulo, color = Cor_texto),
    size = 2.5, fontface = "bold",
    show.legend = FALSE
  ) +
  scale_color_identity() +
  scale_fill_gradient2(
    low = "royalblue",
    mid = "#FAFAFA",
    high = "#C65D3B",
    midpoint = 0,
    limits = c(-limite, limite),
    breaks = c(-limite, 0, limite),
    labels = function(z) gsub("\\.", ",", sprintf("%+.0f", z)),
    name = "Desvio medio de K",
    guide = guide_colorbar(
      title.position = "top",
      barwidth = grid::unit(8, "cm"),
      barheight = grid::unit(0.30, "cm")
    )
  ) +
  labs(x = "Cenario", y = NULL) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(face = "bold", size = 9),
    axis.text.y = element_text(face = "bold", size = 8.7),
    legend.position = "bottom",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    plot.margin = margin(3, 4, 2, 3, "mm")
  )

print(grafico_k)

ggsave(
  file.path(pasta, "mapa_calor_desvio_assinado_K.pdf"),
  plot = grafico_k, width = 7, height = 7.3,
  units = "in", bg = "white"
)

# Grafico do Tempo Computacional

dados_tempo = dados_graficos |>
  filter(is.finite(Tempo_segundos), Tempo_segundos > 0) |>
  mutate(
    Grupo = case_when(
      as.character(Modelo) == "K-means" ~ "K-means",
      as.character(Modelo) == "GMM"     ~ "GMM",
      TRUE                             ~ "DPMM"
    ),
    Grupo = factor(Grupo, levels = c("K-means", "GMM", "DPMM"))
  )

tempos_por_modelo = dados_tempo |>
  group_by(Cenario, Grupo, Modelo) |>
  summarise(
    Tempo_config = mean(Tempo_segundos, na.rm = TRUE),
    .groups = "drop"
  )

resumo_tempo = tempos_por_modelo |>
  group_by(Cenario, Grupo) |>
  summarise(
    Tempo_medio = mean(Tempo_config, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    Rotulo = if_else(
      Grupo == "K-means",
      sprintf("%.3f", Tempo_medio),
      sprintf("%.2f", Tempo_medio)
    ),
    Rotulo = gsub("\\.", ",", Rotulo)
  )

faixa_dp = tempos_por_modelo |>
  filter(Grupo == "DPMM") |>
  group_by(Cenario) |>
  summarise(
    Min_config = min(Tempo_config, na.rm = TRUE),
    Max_config = max(Tempo_config, na.rm = TRUE),
    .groups = "drop"
  )


max_tempo = max(
  resumo_tempo$Tempo_medio,
  faixa_dp$Max_config,
  na.rm = TRUE
)

posicao_kmeans = -0.045 * max_tempo

limite_inferior = -0.10 * max_tempo
limite_superior = 1.12 * max_tempo

# Grafico de tempo
grafico_tempo = ggplot(
  resumo_tempo,
  aes(
    x = Cenario,
    y = Tempo_medio,
    color = Grupo,
    group = Grupo
  )
) +
  
  geom_linerange(
    data = faixa_dp,
    aes(
      x = Cenario,
      ymin = Min_config,
      ymax = Max_config
    ),
    inherit.aes = FALSE,
    linewidth = 0.8,
    color = "royalblue",
    alpha = 0.4
  ) +
  
  geom_line(linewidth = 0.85) +
  
  geom_point(size = 2.5) +

  geom_hline(
    yintercept = 0,
    color = "grey55",
    linewidth = 0.35
  ) +
  
geom_text(
  data = filter(
    resumo_tempo,
    Grupo == "K-means"
  ),
  aes(
    x = Cenario,
    y = posicao_kmeans,
    label = Rotulo
  ),
  inherit.aes = FALSE,
  color = "#C65D3B",
  size = 3,
  fontface = "bold",
  vjust = 0.5
) +
  
geom_text(
  data = filter(
    resumo_tempo,
    Grupo == "GMM"
  ),
  aes(label = Rotulo),
  vjust = -1.2,
  size = 3,
  fontface = "bold",
  show.legend = FALSE
) +
  
geom_text(
  data = filter(
    resumo_tempo,
    Grupo == "DPMM"
  ),
  aes(label = Rotulo),
  vjust = -1.2,
  size = 3,
  fontface = "bold",
  show.legend = FALSE
) +
  
  # Cores dos métodos
  scale_color_manual(
    values = c(
      "K-means" = "#C65D3B",
      "GMM" = "#7030A0",
      "DPMM" = "royalblue"
    ),
    breaks = c("K-means", "GMM", "DPMM")
  ) +
  
  scale_y_continuous(
    breaks = function(lims) {
      valores = pretty(
        c(0, max_tempo),
        n = 6
      )
      valores[valores >= 0 &
                valores <= limite_superior]
    },
    labels = scales::label_number(
      decimal.mark = ",",
      big.mark = "."
    ),
    expand = expansion(mult = 0)
  ) +
  
  coord_cartesian(
    ylim = c(
      limite_inferior,
      limite_superior
    ),
    clip = "off"
  ) +
  
  labs(
    x = "Cenário",
    y = "Tempo médio de execução (segundos)",
    color = NULL
  ) +
  
  theme_classic(base_size = 11) +
  
  theme(
    # Eixos X e Y
    axis.line = element_line(
      color = "black",
      linewidth = 0.5
    ),
    
    axis.ticks = element_line(
      color = "black",
      linewidth = 0.4
    ),
    
    axis.text.x = element_text(
      face = "bold",
      size = 9,
      color = "black"
    ),
    
    axis.text.y = element_text(
      size = 9,
      color = "black"
    ),
    
    axis.title = element_text(
      size = 11,
      color = "black"
    ),
    
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    legend.position = "bottom",
    legend.text = element_text(size = 9),
    legend.title = element_blank(),
    
    plot.margin = margin(
      5, 10, 5, 5, "mm"
    )
  )

print(grafico_tempo)

ggsave(
  file.path(
    pasta,
    "tempo_computacional_metodos.pdf"
  ),
  plot = grafico_tempo,
  width = 7,
  height = 5,
  units = "in",
  bg = "white"
)
