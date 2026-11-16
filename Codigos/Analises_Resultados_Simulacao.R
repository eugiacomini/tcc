# ============================================================
# Mapa de calor do ARI medio - estudo de simulacao DPMM
# Nove cenarios (A1 a C3), tres prioris de alpha e quatro G0
# ============================================================

# Execute uma unica vez, caso necessario:
# install.packages(c("readxl", "dplyr", "purrr", "ggplot2", "scales"))

library(readxl)
library(dplyr)
library(purrr)
library(ggplot2)
library(scales)

# 1. Pasta contendo as nove planilhas
# Substitua pelo caminho da sua pasta, se necessario.
pasta <- "." 

cenarios <- c("A1", "A2", "A3",
              "B1", "B2", "B3",
              "C1", "C2", "C3")

arquivos <- file.path(
  pasta,
  paste0("resultados_simulacoes_dpmm_", cenarios, ".xlsx")
)

if (!all(file.exists(arquivos))) {
  stop("Planilhas nao encontradas: ",
       paste(basename(arquivos[!file.exists(arquivos)]), collapse = ", "))
}

# 2. Cada arquivo tem uma aba com o nome do cenario
dados <- purrr::map2_dfr(
  arquivos, cenarios,
  ~ readxl::read_excel(.x, sheet = .y) |>
      dplyr::mutate(Cenario = .y)
)

# 3. Filtrar DPMM e identificar as 12 configuracoes
dados_dp <- dados |>
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

if (anyNA(dados_dp$Priori_alpha) ||
    any(!dados_dp$G0 %in% paste0("G0-", 1:4))) {
  stop("Ha configuracoes de alpha ou G0 nao reconhecidas.")
}

# 4. ARI medio e desvio-padrao entre replicas
resumo_ari <- dados_dp |>
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

if (nrow(resumo_ari) != 108) {
  stop("Esperadas 108 celulas: 9 cenarios x 3 prioris x 4 G0.")
}
if (any(resumo_ari$n_validos != 20)) {
  warning("Algumas combinacoes nao tem exatamente 20 valores validos.")
}

# 5. Escala adequada mesmo se houver algum ARI negativo
min_ari <- min(0, min(resumo_ari$ARI_medio, na.rm = TRUE))

grafico_ari <- ggplot(
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
    limits = c(min_ari, 1),
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
  width = 6.8,
  height = 5.9,
  units = "in",
  bg = "white"
)




















# ============================================================
# SIMULACOES DPMM - DOIS GRAFICOS PARA A MONOGRAFIA
# 1) Mapa de calor: desvio assinado medio de K
# 2) Tempo computacional medio (escala logaritmica)
#
# Planilhas esperadas:
# resultados_simulacoes_dpmm_A1.xlsx ... _C3.xlsx
# Cada arquivo contem uma aba com o nome do cenario.
#
# Instale, se necessario:
# install.packages(c("readxl", "dplyr", "purrr",
#                    "ggplot2", "scales"))
# ============================================================

library(readxl)
library(dplyr)
library(purrr)
library(ggplot2)
library(scales)

# Altere para a pasta em que estao os arquivos .xlsx:
pasta <- "."

cenarios <- c("A1", "A2", "A3",
              "B1", "B2", "B3",
              "C1", "C2", "C3")

arquivos <- file.path(
  pasta, paste0("resultados_simulacoes_dpmm_", cenarios, ".xlsx")
)

if (!all(file.exists(arquivos))) {
  stop("Arquivos nao encontrados: ",
       paste(basename(arquivos[!file.exists(arquivos)]), collapse = ", "))
}

# ------------------------------------------------------------
# 0. Leitura e identificacao das 12 especificacoes do DPMM
# ------------------------------------------------------------

dados <- map2_dfr(
  arquivos, cenarios,
  ~ read_excel(.x, sheet = .y) |>
    mutate(Cenario = .y)
)

dados_classicos <- dados |>
  filter(Metodo %in% c("KMeans", "GMM")) |>
  mutate(
    Modelo = if_else(Metodo == "KMeans", "K-means", "GMM"),
    K_para_grafico = as.numeric(K_estimado)
  )

dados_dp <- dados |>
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

if (anyNA(dados_dp$alpha_id) ||
    anyNA(dados_dp$g0_id) ||
    !setequal(unique(dados_dp$Modelo), paste0("DP", 1:12))) {
  stop("Falha ao identificar as 12 configuracoes do DPMM.")
}

if (any(
  dados_dp$K_para_grafico != dados_dp$K_estimado,
  na.rm = TRUE
)) {
  warning("Alguns K_Dahl_Livre diferem de K_estimado.")
}

dados_graficos <- bind_rows(dados_classicos, dados_dp)

ordem_metodos <- c("K-means", "GMM", paste0("DP", 1:12))
dados_graficos <- dados_graficos |>
  mutate(
    Cenario = factor(Cenario, levels = cenarios),
    Modelo = factor(Modelo, levels = ordem_metodos)
  )

# ------------------------------------------------------------
# 1. MAPA DE CALOR DO DESVIO ASSINADO DE K
# D = media(K estimado nas 20 replicas) - K verdadeiro
# Positivo: superestimacao / Negativo: subestimacao
# ------------------------------------------------------------

resumo_k <- dados_graficos |>
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

if (nrow(resumo_k) != 126L || any(resumo_k$n_validos != 20L)) {
  stop("Esperados 14 metodos x 9 cenarios com 20 replicas validas.")
}

# Escala divergente SIMETRICA, centrada em zero
limite <- max(1, ceiling(max(abs(resumo_k$Desvio_K), na.rm = TRUE)))

resumo_k <- resumo_k |>
  mutate(
    Cor_texto = if_else(abs(Desvio_K) >= 0.53 * limite,
                        "white", "#26323C")
  )

grafico_k <- ggplot(
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
  plot = grafico_k, width = 6.8, height = 7.3,
  units = "in", bg = "white"
)

# ------------------------------------------------------------
# 2. GRAFICO DO TEMPO COMPUTACIONAL
# Sao mostradas 3 series:
#   K-means -> media das 20 replicas;
#   GMM     -> media das 20 replicas;
#   DPMM    -> media das 12 configuracoes x 20 replicas.
# Os segmentos verticais no DPMM representam os extremos
# das MEDIAS de tempo das 12 configuracoes em cada cenario.
# ------------------------------------------------------------

if (any(is.na(dados_graficos$Tempo_segundos) |
        !is.finite(dados_graficos$Tempo_segundos) |
        dados_graficos$Tempo_segundos <= 0)) {
  warning("Tempos ausentes, nao finitos ou <= 0 foram excluidos ",
          "do grafico em escala logaritmica.")
}

dados_tempo <- dados_graficos |>
  filter(is.finite(Tempo_segundos), Tempo_segundos > 0) |>
  mutate(
    Grupo = case_when(
      as.character(Modelo) == "K-means" ~ "K-means",
      as.character(Modelo) == "GMM"     ~ "GMM",
      TRUE                             ~ "DPMM"
    ),
    Grupo = factor(Grupo, levels = c("K-means", "GMM", "DPMM"))
  )


# ============================================================
# GRÁFICO DO TEMPO COMPUTACIONAL
# ============================================================

library(ggplot2)
library(dplyr)
library(scales)

# Preparar os rótulos
resumo_tempo <- resumo_tempo |>
  mutate(
    Rotulo = if_else(
      Grupo == "K-means",
      sprintf("%.3f", Tempo_medio),
      sprintf("%.2f", Tempo_medio)
    ),
    Rotulo = gsub("\\.", ",", Rotulo)
  )

# Maior tempo observado nas médias e nos intervalos
max_tempo <- max(
  resumo_tempo$Tempo_medio,
  faixa_dp$Max_config,
  na.rm = TRUE
)

# Posição dos rótulos do K-means
posicao_kmeans <- -0.045 * max_tempo

# Limites do gráfico
limite_inferior <- -0.10 * max_tempo
limite_superior <- 1.12 * max_tempo

# ============================================================
# CONSTRUÇÃO DO GRÁFICO
# ============================================================

grafico_tempo <- ggplot(
  resumo_tempo,
  aes(
    x = Cenario,
    y = Tempo_medio,
    color = Grupo,
    group = Grupo
  )
) +
  
  # Intervalos dos tempos médios das configurações DPMM
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
  
  # Linhas dos métodos
  geom_line(linewidth = 0.85) +
  
  # Pontos dos tempos médios
  geom_point(size = 2.5) +
  
  # Linha horizontal indicando tempo zero
  geom_hline(
    yintercept = 0,
    color = "grey55",
    linewidth = 0.35
  ) +
  
  # ----------------------------------------
# RÓTULOS K-MEANS: ABAIXO DOS PONTOS
# ----------------------------------------
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
  
  # ----------------------------------------
# RÓTULOS GMM: ACIMA DOS PONTOS
# ----------------------------------------
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
  
  # ----------------------------------------
# RÓTULOS DPMM: ACIMA DOS PONTOS
# ----------------------------------------
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
  
  # Eixo Y em escala linear
  scale_y_continuous(
    breaks = function(lims) {
      valores <- pretty(
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
  
  # Espaço inferior reservado aos rótulos
  coord_cartesian(
    ylim = c(
      limite_inferior,
      limite_superior
    ),
    clip = "off"
  ) +
  
  # Títulos dos eixos
  labs(
    x = "Cenário",
    y = "Tempo médio de execução (segundos)",
    color = NULL
  ) +
  
  # Fundo branco, sem grades
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
    
    # Remover completamente o grid
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Legenda
    legend.position = "bottom",
    legend.text = element_text(size = 9),
    legend.title = element_blank(),
    
    # Margens
    plot.margin = margin(
      5, 10, 5, 5, "mm"
    )
  )

# Visualizar
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
