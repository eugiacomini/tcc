load("Ambientes/meu_ambiente.RData")

pacotes = c(
  "readxl", "dplyr", "ggplot2", "plotly", "gridExtra",
  "mclust", "cluster", "reshape2", "corrplot", "clusterCrit",
  "htmlwidgets","mvtnorm","openxlsx", "qgraph", "tibble"
)

# Instalar apenas os pacotes ausentes
pacotes_faltantes = pacotes[
  !vapply(
    pacotes,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(pacotes_faltantes) > 0) {
  install.packages(pacotes_faltantes)
}

for (p in pacotes) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
  library(p, character.only = TRUE)
}

seed_base = 811819

# Inserir base lol_2025
dados_originais = as.data.frame(lol_2025)

# Padronizar nomes de colunas
names(dados_originais) = make.names(
  names(dados_originais),
  unique = TRUE
)

colunas = setdiff(names(dados_originais), "Jogador")

dados_originais[colunas] = lapply(dados_originais[colunas], function(x) {
  if (is.numeric(x)) return(x)
  
  x = trimws(as.character(x))
  x[x %in% c("", "-", "NA", "N/A")] = NA
  x = gsub(",", ".", x, fixed = TRUE)
  
  suppressWarnings(as.numeric(x))
})

dados_originais[colunas] = lapply(dados_originais[colunas], function(x) {
  x[is.na(x)] = 0
  x
})

dados = dados_originais %>%
  filter(
    !is.na(Numero.Vitorias),
    Numero.Vitorias >= 10
  ) %>%
  mutate(
    Numero.Partidas = as.integer(Numero.Partidas),
    Numero.Vitorias = as.integer(Numero.Vitorias)
  ) %>%
  as.data.frame()

variaveis_numericas = names(dados)[
  sapply(dados, is.numeric)
]

estatisticas_descritivas = do.call(
  rbind,
  lapply(variaveis_numericas, function(v) {
    
    x = dados[[v]]
    
    data.frame(
      Variavel = v,
      Media = mean(x),
      DP = sd(x),
      Mediana = median(x),
      Minimo = min(x),
      Maximo = max(x),
      Perc_Zeros = 100 * mean(x == 0)
    )
  })
)

# Arredondar resultados
estatisticas_descritivas[-1] = lapply(
  estatisticas_descritivas[-1],
  function(x) round(x, 2)
)

print(estatisticas_descritivas)

# Verificar variaveis com variancia nula
desvios = sapply(
  dados[, variaveis_numericas, drop = FALSE],
  sd
)

variaveis_validas = names(desvios)[
  is.finite(desvios) & desvios > 0
]

variaveis_removidas = setdiff(
  variaveis_numericas,
  variaveis_validas
)

if (length(variaveis_removidas) > 0) {
  cat(
    "\nVariaveis com variancia nula removidas:\n",
    paste(variaveis_removidas, collapse = ", "),
    "\n"
  )
}

X_descritiva = scale(
  dados[, variaveis_validas, drop = FALSE]
)

X_descritiva = as.matrix(X_descritiva)

summary(as.vector(X_descritiva))

# Converter matriz padronizada para formato longo
dados_boxplot = reshape2::melt(
  as.data.frame(X_descritiva),
  variable.name = "Variavel",
  value.name = "Valor_Padronizado"
)

# Boxplots de todas as variaveis

grafico_boxplot = ggplot(
  dados_boxplot,
  aes(
    x = Variavel,
    y = Valor_Padronizado
  )
) +
  geom_boxplot(
    fill = "royalblue",
    alpha = 0.7,
    outlier.color = "#C65D3B",
    outlier.size = 0.8,
    outlier.alpha = 0.5
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray40"
  ) +
  coord_flip(ylim = c(-5, 5)) +
  labs(
    x = "Variáveis",
    y = "Valor padronizado (z-score)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.line = element_line(
      color = "black",
      linewidth = 0.6
    ),
    axis.ticks = element_line(
      color = "black",
      linewidth = 0.5
    ),
    axis.text.x = element_text(
      size = 10,
      color = "black"
    ),
    axis.text.y = element_text(
      size = 9,
      color = "black"
    ),
    axis.title = element_text(
      size = 12,
      #face = "bold",
      color = "black"
    ),
    panel.grid.major.y = element_blank()
  )

print(grafico_boxplot)

cat("Jogadores após o filtro:", nrow(dados), "\n")

matriz_cor = as.matrix(cor(
  dados[, colunas, drop = FALSE],
  method = "pearson",
  use = "pairwise.complete.obs"
))

matriz_cor = (matriz_cor + t(matriz_cor)) / 2

matriz_cor[matriz_cor > 1] = 1
matriz_cor[matriz_cor < -1] = -1

diag(matriz_cor) = 1

nomes_originais = colnames(matriz_cor)

# Substituir nomes por v1, v2, v3...
nomes_variaveis = paste0(
  "v",
  seq_len(ncol(matriz_cor))
)

# Tabela de identificação
tabela_variaveis = data.frame(
  Codigo = nomes_variaveis,
  Variavel = nomes_originais
)

# Substituir nomes na matriz
colnames(matriz_cor) = nomes_variaveis
rownames(matriz_cor) = nomes_variaveis

# Matriz de correlação
dist_cor = as.dist(1 - matriz_cor)

hc = hclust(
  dist_cor,
  method = "complete"
)

ordem = colnames(matriz_cor)[hc$order]

matriz_cor_ord = matriz_cor[ordem,ordem,drop = FALSE]

dados_heatmap = reshape2::melt(matriz_cor_ord)

names(dados_heatmap) = c(
  "Variavel_1",
  "Variavel_2",
  "Correlacao"
)

dados_heatmap$Variavel_1 = factor(
  dados_heatmap$Variavel_1,
  levels = ordem
)

dados_heatmap$Variavel_2 = factor(
  dados_heatmap$Variavel_2,
  levels = rev(ordem)
)

dados_heatmap_plot = dados_heatmap %>%
  mutate(
    indice_x = match(as.character(Variavel_1), ordem),
    indice_y = match(as.character(Variavel_2), ordem)
  ) %>%
  filter(indice_x >= indice_y)

heatmap_correlacao = ggplot(
  dados_heatmap_plot,
  aes(
    x = Variavel_1,
    y = Variavel_2,
    fill = Correlacao
  )
) +
  
  geom_tile(
    color = "white",
    linewidth = 0.25
  ) +
  
  scale_fill_gradient2(
    low = "#C65D3B",
    mid = "#FFFFFF",
    high = "royalblue",
    midpoint = 0,
    limits = c(-1, 1),
    breaks = c(-1, -0.5, 0, 0.5, 1),
    name = "Correlação"
  ) +

  scale_x_discrete(
    limits = ordem,
    position = "top",
    breaks = NULL,
    expand = expansion(add = 0)
  ) +
  
  scale_y_discrete(
    limits = rev(ordem),
    position = "left",
    expand = expansion(add = 0)
  ) +
  
  coord_fixed() +
  
  labs(
    x = NULL,
    y = NULL
  ) +
  
  theme_minimal(base_size = 11) +
  
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    
    panel.grid = element_blank(),
    
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    plot.margin = margin(0, 0, 0, 0)
  )

ggsave(
  filename = "heatmap.pdf",
  plot = heatmap_correlacao,
  device = "pdf",
  width = 7,
  height = 5,
  units = "in",
  bg = "white"
)

print(heatmap_correlacao)

matriz_qgraph = matriz_cor

diag(matriz_qgraph) = 0

set.seed(seed_base)

qgraph(
  matriz_qgraph,
  
  layout = "spring",
  
  labels = nomes_variaveis,
  
  color = "#FFFFFF",
  border.color = "black",
  
  posCol = "royalblue",
  negCol = "#C65D3B",
  
  edge.width = 1,
  fade = TRUE,
  
  vsize = 3,
  label.cex = 3,
  label.color = "black",
  
  theme = "classic"
)

print(tabela_variaveis)

# Selecionar variáveis mantidas
colunas = c(
  "v2",  # Numero.Vitorias
  "v3",  # KDA
  "v4",  # Media.Kills
  "v5",  # Media.Deaths
  "v6",  # Media.Assistances
  "v7",  # CSM
  "v9",  # DPM
  "v10", # VSPM
  "v12", # Avg.WCPM
  "v13", # Avg.VWPM
  "v14", # GD.15
  "v15", # CSD.15
  "v16", # XPD.15
  "v17", # Pentakills
  "v18", # Abates.Solo
  "v19", # KP.
  "v23", # FB..
  "v24"  # FB.Victim
)


tabela_final = tabela_variaveis %>%
  filter(Codigo %in% colunas)

print(tabela_final, row.names = FALSE)

dados_features = dados[,tabela_final$Variavel,drop = FALSE]

dados_features

# Padronização das variáveis
X_padronizado = as.matrix(scale(dados_features))

# Transformação de Cholesky
cholesky_transformation = function(Y) {
  Y = as.matrix(Y)
  S = stats::cov(Y)
  L = t(chol(S))
  Y_centered = sweep(Y, 2, colMeans(Y), "-")
  t(solve(L, t(Y_centered)))
}

X = cholesky_transformation(X_padronizado)

# Verificar a transformação
print(round(colMeans(X), 6))
print(round(cov(X), 4))

cat("Variáveis utilizadas no clustering:\n")
print(colnames(X))

cat("Variáveis utilizadas no clustering:\n")
print(colnames(X))


# Agrupamento

k_max = 30

# GMM — GAUSSIAN MIXTURE MODEL
set.seed(seed_base)

modelo_gmm = tryCatch(
  
  mclust::Mclust(
    X,
    G = 1:k_max,
    verbose = FALSE
  ),
  
  error = function(e) {
    
    message("GMM padrão falhou: ", e$message)
    message("Tentando modelos mais simples.")
    
    tryCatch(
      
      mclust::Mclust(
        X,
        G = 1:k_max,
        modelNames = c("EII", "VII", "EEI", "VEI"),
        verbose = FALSE
      ),
      
      error = function(e2) {
        stop(
          "Não foi possível ajustar o GMM: ",
          e2$message
        )
      }
      
    )
  }
)

cluster_gmm = modelo_gmm$classification

incerteza_gmm = modelo_gmm$uncertainty

cat("Melhor G para GMM:", modelo_gmm$G, "\n")

# Listar jogadores por cluster GMM
jogadores_por_cluster_gmm = split(
  dados$Jogador,
  cluster_gmm
)

for (i in names(jogadores_por_cluster_gmm)) {
  
  cat("\nCluster", i, ":\n")
  
  cat(
    paste(jogadores_por_cluster_gmm[[i]], collapse = ", "),
    "\n"
  )
}

pca_gmm = prcomp(
  X_padronizado,
  center = FALSE,
  scale. = FALSE
)

variancia_gmm = 100 * pca_gmm$sdev^2 /
  sum(pca_gmm$sdev^2)

tabela_pca_gmm = data.frame(
  Jogador = dados$Jogador,
  PC1 = pca_gmm$x[, 1],
  PC2 = pca_gmm$x[, 2],
  PC3 = pca_gmm$x[, 3],
  Cluster = factor(cluster_gmm),
  Incerteza = incerteza_gmm
)

paleta_base = c(
  "#F0E442", # 01 Amarelo
  "#003B5C", # 02 Azul-escuro
  "#56B4E9", # 03 Azul-claro
  "#009E73", # 04 Verde
  "#D55E00", # 05 Vermelho-alaranjado
  "#CC79A7", # 06 Rosa
  "#E69F00", # 07 Laranja
  "#0072B2", # 08 Azul
  "#332288", # 09 Roxo-escuro
  "#88CCEE", # 10 Azul-celeste
  "#44AA99", # 11 Verde-água
  "#117733", # 12 Verde-escuro
  "#999933", # 13 Oliva
  "#DDCC77", # 14 Dourado-claro
  "#CC6677", # 15 Rosa-avermelhado
  "#882255", # 16 Vinho
  "#AA4499", # 17 Magenta
  "#661100", # 18 Marrom
  "#6699CC", # 19 Azul-médio
  "#AA4466", # 20 Rosa-escuro
  "#4477AA", # 21 Azul-acinzentado
  "#228833", # 22 Verde-forte
  "#EE6677", # 23 Coral
  "#BBBBBB"  # 24 Cinza
)

simbolos_base = c(
  "circle",
  "square",
  "diamond",
  "cross",
  "x",
  "circle-open",
  "square-open",
  "diamond-open"
)

niveis = levels(tabela_pca_gmm$Cluster)
n_clusters = length(niveis)

# Atribuição automática
cores = rep(
  paleta_base,
  length.out = n_clusters
)

simbolos = rep(
  simbolos_base,
  length.out = n_clusters
)

names(cores) = niveis
names(simbolos) = niveis

p_pca_gmm = plot_ly()

for (cl in niveis) {
  
  dados_cluster = subset(
    tabela_pca_gmm,
    Cluster == cl
  )
  
  dados_cluster$Hover = paste0(
    "<b>Jogador:</b> ", dados_cluster$Jogador,
    "<br><b>Cluster:</b> ", cl,
    "<br><b>Incerteza:</b> ",
    round(dados_cluster$Incerteza, 4)
  )
  
  p_pca_gmm = p_pca_gmm %>%
    add_trace(
      data = dados_cluster,
      
      x = ~PC1,
      y = ~PC2,
      z = ~PC3,
      
      type = "scatter3d",
      mode = "markers",
      
      name = paste("Cluster", cl),
      
      text = ~Hover,
      hoverinfo = "text",
      
      marker = list(
        color = unname(cores[cl]),
        symbol = unname(simbolos[cl]),
        size = 5,
        opacity = 0.9,
        line = list(
          color = "#333333",
          width = 0.5
        )
      )
    )
}

p_pca_gmm = p_pca_gmm %>%
  layout(
    title = paste0(
      "PCA 3D — GMM (G = ",
      modelo_gmm$G,
      ")"
    ),
    
    scene = list(
      xaxis = list(
        title = paste0(
          "PC1 (",
          round(variancia_gmm[1], 1),
          "%)"
        )
      ),
      
      yaxis = list(
        title = paste0(
          "PC2 (",
          round(variancia_gmm[2], 1),
          "%)"
        )
      ),
      
      zaxis = list(
        title = paste0(
          "PC3 (",
          round(variancia_gmm[3], 1),
          "%)"
        )
      )
    ),
    
    legend = list(
      title = list(text = "Clusters")
    )
  )

p_pca_gmm

# Salvar HTML interativo
htmlwidgets::saveWidget(
  p_pca_gmm,
  "PCA_3D_GMM.html",
  selfcontained = TRUE
)



# K-Means
distancia_X = dist(X)

wss = numeric(k_max)
silhouette_media = rep(NA_real_, k_max)
modelos_kmeans = vector("list", k_max)

for (k in seq_len(k_max)) {
  
  modelo = kmeans(X,centers = k,nstart = 50,iter.max = 100)
  
  modelos_kmeans[[k]] = modelo
  wss[k] = modelo$tot.withinss
  
  if (k >= 2) {
    
    sil = cluster::silhouette(
      modelo$cluster,
      distancia_X
    )
    
    silhouette_media[k] = mean(sil[, 3])
  }
}

resultados_kmeans = data.frame(
  k = seq_len(k_max),
  WSS = wss,
  silhouette_media = silhouette_media
)

resultados_kmeans

tema_kmeans = theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15,
      hjust = 0.5,
      color = "#243447"
    ),
    plot.subtitle = element_text(
      size = 10,
      hjust = 0.5,
      color = "gray40"
    ),
    axis.title = element_text(
      face = "bold",
      size = 11,
      color = "#34495E"
    ),
    axis.text = element_text(
      size = 10,
      color = "gray30"
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      color = "#E8EDF2",
      linewidth = 0.4
    ),
    plot.margin = margin(15, 15, 15, 15)
  )

p_cotovelo = ggplot(
  resultados_kmeans,
  aes(x = k, y = WSS)
) +
  geom_line(
    color = "#003B5C",
    linewidth = 1.2
  ) +
  geom_point(
    color = "#003B5C",
    fill = "white",
    shape = 21,
    size = 3.5,
    stroke = 1.3
  ) +
  scale_x_continuous(
    breaks = seq_len(k_max)
  ) +
  labs(
    title = "Método do Cotovelo",
    subtitle = "Soma dos quadrados intracluster",
    x = "Número de clusters (k)",
    y = "WSS"
  ) +
  tema_kmeans

p_silhouette = ggplot(
  subset(resultados_kmeans, k >= 2),
  aes(x = k, y = silhouette_media)
) +
  geom_line(
    color = "#F0E442",
    linewidth = 1.2
  ) +
  geom_point(
    color = "#F0E442",
    fill = "white",
    shape = 21,
    size = 3.5,
    stroke = 1.3
  ) +
  scale_x_continuous(
    breaks = 2:k_max
  ) +
  labs(
    title = "Método da Silhouette",
    subtitle = "Coeficiente médio de silhouette",
    x = "Número de clusters (k)",
    y = "Silhouette média"
  ) +
  tema_kmeans

p_final = arrangeGrob(
  p_cotovelo,
  p_silhouette,
  ncol = 2
)

# Exibir
grid::grid.newpage()
grid::grid.draw(p_final)

# Escolher k pela maior silhouette média válida
indices_validos = which(
  seq_len(k_max) >= 2 &
    is.finite(silhouette_media)
)

k_otimo = indices_validos[
  which.max(silhouette_media[indices_validos])
]

kmeans_final = modelos_kmeans[[k_otimo]]
cluster_kmeans = kmeans_final$cluster

cat("Melhor k para K-means:", k_otimo, "\n")

# Listar jogadores por cluster K-Means
jogadores_por_cluster_kmeans = split(
  dados$Jogador,
  cluster_kmeans
)

for (i in names(jogadores_por_cluster_kmeans)) {
  
  cat("\nCluster", i, ":\n")
  
  cat(
    paste(jogadores_por_cluster_kmeans[[i]], collapse = ", "),
    "\n"
  )
}


# PCA sobre os dados padronizados (sem Cholesky)
pca_kmeans = prcomp(
  X_padronizado,
  center = FALSE,
  scale. = FALSE
)

# Percentual da variância explicada
variancia_kmeans = 100 * pca_kmeans$sdev^2 /
  sum(pca_kmeans$sdev^2)

# Tabela para visualização
tabela_pca_kmeans = data.frame(
  Jogador = dados$Jogador,
  PC1 = pca_kmeans$x[, 1],
  PC2 = pca_kmeans$x[, 2],
  PC3 = pca_kmeans$x[, 3],
  Cluster = factor(cluster_kmeans)
)

# Identificar os clusters
niveis_kmeans = levels(tabela_pca_kmeans$Cluster)
n_clusters_kmeans = length(niveis_kmeans)

# Atribuição automática de cores e símbolos
cores_kmeans = rep(
  paleta_base,
  length.out = n_clusters_kmeans
)

simbolos_kmeans = rep(
  simbolos_base,
  length.out = n_clusters_kmeans
)

names(cores_kmeans) = niveis_kmeans
names(simbolos_kmeans) = niveis_kmeans

# Criar gráfico 3D
p_pca_kmeans = plot_ly()

for (cl in niveis_kmeans) {
  
  dados_cluster = subset(
    tabela_pca_kmeans,
    Cluster == cl
  )
  
  dados_cluster$Hover = paste0(
    "<b>Jogador:</b> ", dados_cluster$Jogador,
    "<br><b>Cluster:</b> ", cl
  )
  
  p_pca_kmeans = p_pca_kmeans %>%
    add_trace(
      data = dados_cluster,
      
      x = ~PC1,
      y = ~PC2,
      z = ~PC3,
      
      type = "scatter3d",
      mode = "markers",
      
      name = paste("Cluster", cl),
      
      text = ~Hover,
      hoverinfo = "text",
      
      marker = list(
        color = unname(cores_kmeans[cl]),
        symbol = unname(simbolos_kmeans[cl]),
        size = 5,
        opacity = 0.9,
        line = list(
          color = "#333333",
          width = 0.5
        )
      )
    )
}

# Layout do gráfico
p_pca_kmeans = p_pca_kmeans %>%
  layout(
    title = paste0(
      "PCA 3D — K-means (K = ",
      k_otimo,
      ")"
    ),
    
    scene = list(
      xaxis = list(
        title = paste0(
          "PC1 (",
          round(variancia_kmeans[1], 1),
          "%)"
        )
      ),
      
      yaxis = list(
        title = paste0(
          "PC2 (",
          round(variancia_kmeans[2], 1),
          "%)"
        )
      ),
      
      zaxis = list(
        title = paste0(
          "PC3 (",
          round(variancia_kmeans[3], 1),
          "%)"
        )
      )
    ),
    
    legend = list(
      title = list(text = "Clusters")
    )
  )

p_pca_kmeans

# Salvar HTML interativo
htmlwidgets::saveWidget(
  widget = p_pca_kmeans,
  file = "PCA_3D_Kmeans.html",
  selfcontained = TRUE
)


# DPMM - Dirichlet Process Mixture Model
# Comparacao de prioris de alpha e configuracoes de G0
# Total: 3 prioris de alpha x 5 configuracoes de G0 = 15 ajustes

# Arquivo contendo clustering_dpmm() e dpmm_particao_dahl()
source("Funcoes_DPMM.R")

storage.mode(X) = "double"

n = nrow(X)
p = ncol(X)

# Configuracoes de priori de alpha
alpha_priors = data.frame(
  Prior_alpha = c(
    "Gamma(1,1)",
    "Gamma(2,4)",
    "Gamma(1,0.1)"
  ),
  a_alpha = c(1, 2, 1),
  b_alpha = c(1, 4, 0.1)
)

# Média da distribuição a priori
alpha_priors$Media_prior = with(
  alpha_priors,
  a_alpha / b_alpha
)

# Desvio-padrão da distribuição a priori
alpha_priors$DP_prior = with(
  alpha_priors,
  sqrt(a_alpha) / b_alpha
)

print(alpha_priors)


# Configuracoes de G0
# G0 Normal-Inverse-Gamma independente por dimensao.
# mu0: centro da priori
# lambda0: precisao da priori para a media
# alfa0 e beta0: parametros da priori da variancia

g0_configs = data.frame(
  Config_G0 = c(
    "G0-1_original",
    "G0-2_lambda_fraco",
    "G0-3_lambda_forte",
    "G0-4_beta_fraco",
    "G0-5_beta_forte"),
  mu0 = rep(0, 5),
  lambda0 = c(1,0.1,10,1,1),
  alfa0 = rep(2, 5),
  beta0 = c(1,1,1,0.5,2)
)

print(g0_configs)


# Parâmetros do MCMC

n_final_samples = 2000
burnin = 1000
saltos = 2

n_coclustering_samples = 500

alpha_init = 1
K_init = 3

# Função para calcular o indice de silhueta da matriz C

calcular_silhouette_coclustering = function(clusters, C) {
  
  clusters = as.integer(factor(clusters))
  
  K = length(unique(clusters))
  n = length(clusters)
  
  if (K < 2 || K >= n) {
    return(NA_real_)
  }
  
  D = 1 - C
  
  D = (D + t(D)) / 2
  D[D < 0] = 0
  D[D > 1] = 1
  diag(D) = 0
  
  resultado = tryCatch({
    
    sil = cluster::silhouette(
      clusters,
      stats::as.dist(D)
    )
    
    mean(sil[, "sil_width"], na.rm = TRUE)
    
  }, error = function(e) NA_real_)
  
  resultado
}

# Executando o MCMC

resultados = list()
particoes = list()
matrizes_coclustering = list()
modelos = list()

contador = 0L

total_ajustes = nrow(alpha_priors) * nrow(g0_configs)

for (ia in seq_len(nrow(alpha_priors))) {
  
  for (ig in seq_len(nrow(g0_configs))) {
    
    contador = contador + 1L
    
    alpha = alpha_priors[ia, ]
    g0 = g0_configs[ig, ]
    
    nome = paste(
      g0$Config_G0,
      alpha$Prior_alpha,
      sep = "__"
    )
    
    cat(
      "\n========================================\n",
      "Ajuste ", contador, " de ", total_ajustes, "\n",
      nome, "\n",
      "========================================\n",
      sep = ""
    )
    
    set.seed(seed_base + contador)
    
    inicio = Sys.time()
    
    ajuste = tryCatch({
      
      clustering_dpmm(X = X,
                      
                      prioris = list(
                        mu0 = g0$mu0,
                        lambda0 = g0$lambda0,
                        alfa0 = g0$alfa0,
                        beta0 = g0$beta0
                      ),
                      
                      hiperparametros = list(
                        a_alpha = alpha$a_alpha,
                        b_alpha = alpha$b_alpha
                      ),
                      
                      n_final_samples = n_final_samples,
                      burnin = burnin,
                      saltos = saltos,
                      
                      seed = seed_base + contador,
                      verbose = FALSE,
                      
                      calcular_coclustering = TRUE,
                      n_coclustering_samples = n_coclustering_samples,
                      
                      alpha_init = alpha_init,
                      K_init = K_init
      )
      
    }, error = function(e) {
      
      message("Erro no ajuste: ", conditionMessage(e))
      NULL
      
    })
    
    tempo = as.numeric(
      difftime(Sys.time(), inicio, units = "secs")
    )
    
    if (is.null(ajuste)) {
      
      resultados[[nome]] = data.frame(
        Config_G0 = g0$Config_G0,
        Prior_alpha = alpha$Prior_alpha,
        Status = "Erro",
        Erro = "Falha no ajuste",
        Tempo_segundos = tempo
      )
      
      next
    }
    
    grupos = ajuste$groups_samples
    K_samples = ajuste$K_samples
    C = ajuste$coclustering
    
    if (is.null(K_samples) || length(K_samples) == 0) {
      K_samples = apply(
        grupos,
        1,
        function(z) length(unique(z))
      )
    }
    
    K_samples = as.integer(K_samples)
    
    particao_dahl = dpmm_particao_dahl(
      groups_samples = grupos,
      C = C,
      indices_draws = ajuste$indices_coclustering
    )
    
    clusters_dahl = particao_dahl$clusters
    
    K_dahl = length(unique(clusters_dahl))
    
    silhouette_C = calcular_silhouette_coclustering(
      clusters = clusters_dahl,
      C = C
    )
    
    # Resultados
    
    tab_K = table(K_samples)
    
    K_moda = as.integer(
      names(tab_K)[which.max(tab_K)]
    )
    
    resultados[[nome]] = data.frame(
      
      Config_G0 = g0$Config_G0,
      
      Prior_alpha = alpha$Prior_alpha,
      
      a_alpha = alpha$a_alpha,
      
      b_alpha = alpha$b_alpha,
      
      mu0 = g0$mu0,
      
      lambda0 = g0$lambda0,
      
      alfa0 = g0$alfa0,
      
      beta0 = g0$beta0,
      
      K_Dahl = K_dahl,
      
      K_posterior_media = mean(K_samples),
      
      K_posterior_mediana = median(K_samples),
      
      K_posterior_moda = K_moda,
      
      Dahl_loss = particao_dahl$dahl_loss,
      
      Dahl_draw_index = particao_dahl$dahl_draw_index,
      
      Silhouette_coclustering = silhouette_C,
      
      Tempo_segundos = tempo,
      
      N_amostras_posteriores = length(K_samples),
      
      Status = "Concluido",
      
      stringsAsFactors = FALSE
    )
    
    # Guardar particao e matriz C.
    particoes[[nome]] = data.frame(
      Jogador = if ("Jogador" %in% names(dados)) {
        dados$Jogador
      } else {
        seq_len(nrow(X))
      },
      Cluster_DPMM = clusters_dahl
    )
    
    matrizes_coclustering[[nome]] = C
    
    modelos[[nome]] = ajuste
    
    cat(
      "K Dahl:", K_dahl,
      "| K posterior medio:", round(mean(K_samples), 2),
      "| Silhouette:", round(silhouette_C, 3),
      "| Tempo:", round(tempo, 2), "segundos\n"
    )
  }
}


# Organizar resultados

resultados_dpmm = dplyr::bind_rows(resultados)

resultados_dpmm = resultados_dpmm %>%
  arrange(
    Config_G0,
    Prior_alpha
  )

print(resultados_dpmm)

resumo_dpmm = resultados_dpmm %>%
  filter(Status == "Concluido") %>%
  group_by(Config_G0, Prior_alpha) %>%
  summarise(
    K_Cocluster = mean(K_Dahl, na.rm = TRUE),
    K_posterior_media = mean(K_posterior_media, na.rm = TRUE),
    K_posterior_mediana = mean(K_posterior_mediana, na.rm = TRUE),
    Silhouette = mean(Silhouette_coclustering, na.rm = TRUE),
    Tempo_segundos = mean(Tempo_segundos, na.rm = TRUE),
    .groups = "drop"
  )

resultados_dpmm

# Salvar ambiente R
saveRDS(
  list(
    resultados = resultados_dpmm,
    resumo = resumo_dpmm,
    comparacoes_ARI = comparacoes_ARI,
    particoes = particoes,
    matrizes_coclustering = matrizes_coclustering,
    modelos = modelos,
    configuracoes_G0 = g0_configs,
    prioris_alpha = alpha_priors,
    X = X
  ),
  file = "objetos_completos_dpmm_lol_2025.rds"
)

# Recuperar modelos, caso não estejam no ambiente
if (!exists("modelos")) {
  objetos = readRDS(
    "objetos_completos_dpmm_lol_2025.rds"
  )
  modelos = objetos$modelos
}

# Extrair a trajetória de K de cada configuração
tracos_K = bind_rows(
  lapply(names(modelos), function(nome) {
    
    ajuste = modelos[[nome]]
    K = ajuste$K_samples
    
    # Recuperar K das partições, caso necessário
    if (is.null(K) && !is.null(ajuste$groups_samples)) {
      K = apply(
        ajuste$groups_samples,
        1,
        function(z) length(unique(z))
      )
    }
    
    if (is.null(K) || length(K) == 0) {
      return(NULL)
    }
    
    K = as.integer(K)
    
    partes = strsplit(
      nome, "__", fixed = TRUE
    )[[1]]
    
    data.frame(
      Config_G0 = partes[1],
      Prior_alpha = partes[2],
      Amostra = seq_along(K),
      K = K
    )
  })
)

if (nrow(tracos_K) == 0) {
  stop("Nenhuma trajetória de K encontrada.")
}

# Ordenar as configurações
tracos_K$Config_G0 = factor(
  tracos_K$Config_G0,
  levels = g0_configs$Config_G0
)

tracos_K$Prior_alpha = factor(
  tracos_K$Prior_alpha,
  levels = alpha_priors$Prior_alpha
)

# Calcular média acumulada
tracos_K = tracos_K %>%
  group_by(Config_G0, Prior_alpha) %>%
  arrange(Amostra, .by_group = TRUE) %>%
  mutate(
    K_media_acumulada = cumsum(K) / row_number()
  ) %>%
  ungroup()

p_trace_K = ggplot(
  tracos_K,
  aes(x = Amostra, y = K)
) +
  geom_line(
    color = "#003B5C",
    linewidth = 0.5
  ) +
  facet_wrap(
    ~ Config_G0 + Prior_alpha,
    ncol = 3,
    scales = "free_y"
  ) +
  scale_y_continuous(
    breaks = function(x) {
      seq(ceiling(x[1]), floor(x[2]), by = 1)
    }
  ) +
  labs(
    title = "Trajetórias MCMC do número de clusters",
    x = "Amostra posterior",
    y = "Número de clusters (K)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    strip.text = element_text(
      face = "bold",
      size = 9
    ),
    panel.grid.minor = element_blank()
  )

print(p_trace_K)


# ==================================================
# GRÁFICO 2 — CONVERGÊNCIA DA MÉDIA ACUMULADA
# ==================================================

p_media_K = ggplot(
  tracos_K,
  aes(x = Amostra, y = K_media_acumulada)
) +
  geom_line(
    color = "#059669",
    linewidth = 0.8
  ) +
  facet_grid(
    Config_G0 ~ Prior_alpha,
    scales = "free_y"
  ) +
  labs(
    title = "Estabilização da média posterior de K",
    subtitle = "Média acumulada ao longo das amostras MCMC",
    x = "Índice da amostra posterior armazenada",
    y = "Média acumulada de K"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16,
      hjust = 0.5
    ),
    plot.subtitle = element_text(hjust = 0.5),
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank(),
    panel.spacing = grid::unit(0.8, "lines")
  )

print(p_media_K)

# ==================================================
# GRÁFICO 3 — AUTOCORRELAÇÃO DE K
# ==================================================

listas_tracos = split(
  tracos_K,
  interaction(
    tracos_K$Config_G0,
    tracos_K$Prior_alpha,
    drop = TRUE
  )
)

dados_acf = bind_rows(
  lapply(listas_tracos, function(d) {
    
    if (nrow(d) < 3 || sd(d$K) == 0) {
      return(NULL)
    }
    
    ac = stats::acf(
      d$K,
      lag.max = min(50, nrow(d) - 1),
      plot = FALSE
    )
    
    valores = as.numeric(ac$acf)[-1]
    
    data.frame(
      Config_G0 = d$Config_G0[1],
      Prior_alpha = d$Prior_alpha[1],
      Lag = seq_along(valores),
      ACF = valores
    )
  })
)

p_acf_K = ggplot(
  dados_acf,
  aes(x = Lag, y = ACF)
) +
  geom_hline(
    yintercept = 0,
    color = "gray50"
  ) +
  geom_segment(
    aes(xend = Lag, yend = 0),
    color = "#2563EB",
    linewidth = 0.65
  ) +
  facet_grid(
    Config_G0 ~ Prior_alpha
  ) +
  labs(
    title = "Autocorrelação das cadeias de K",
    subtitle = "Dependência entre amostras posteriores",
    x = "Defasagem (lag)",
    y = "Autocorrelação"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    plot.subtitle = element_text(hjust = 0.5),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(p_acf_K)

# ARI — Concordância entre as configurações do DPMM

configs = names(particoes)

if (length(configs) < 2) {
  stop("São necessárias pelo menos duas partições.")
}

cat("Configurações disponíveis:", length(configs), "\n")

pares = combn(configs, 2, simplify = FALSE)

resultados_ARI = bind_rows(
  lapply(pares, function(par) {
    
    p1 = particoes[[par[1]]]
    p2 = particoes[[par[2]]]
    
    # Verificar se os jogadores correspondem
    if (
      anyNA(p1$Jogador) ||
      anyNA(p2$Jogador) ||
      anyDuplicated(p1$Jogador) > 0 ||
      anyDuplicated(p2$Jogador) > 0 ||
      !setequal(p1$Jogador, p2$Jogador)
    ) {
      stop("As partições não possuem os mesmos jogadores.")
    }
    
    # Reordenar jogadores da segunda configuração
    idx = match(p1$Jogador, p2$Jogador)
    
    if (
      anyNA(p1$Cluster_DPMM) ||
      anyNA(p2$Cluster_DPMM[idx])
    ) {
      stop("Existem classificações de cluster ausentes.")
    }
    
    # Calcular ARI
    ari = mclust::adjustedRandIndex(
      p1$Cluster_DPMM,
      p2$Cluster_DPMM[idx]
    )
    
    data.frame(
      Configuracao_1 = par[1],
      Configuracao_2 = par[2],
      ARI = ari
    )
  })
) %>%
  as_tibble() %>%
  arrange(desc(ARI))

# Matriz inicial com ARI = 1 na diagonal
matriz_ARI = matrix(
  1,
  nrow = length(configs),
  ncol = length(configs),
  dimnames = list(configs, configs)
)

# Preencher matriz simétrica
for (i in seq_len(nrow(resultados_ARI))) {
  
  a = resultados_ARI$Configuracao_1[i]
  b = resultados_ARI$Configuracao_2[i]
  valor = resultados_ARI$ARI[i]
  
  matriz_ARI[a, b] = valor
  matriz_ARI[b, a] = valor
}

hc_ARI = hclust(
  as.dist(1 - matriz_ARI),
  method = "average"
)

ordem_ARI = rownames(matriz_ARI)[hc_ARI$order]

# Identificar os 15 modelos como DP_LoL
# com subscritos na imagem

rotulos = setNames(
  paste0(
    "plain(DP)[plain(LoL)*','*",
    seq_along(configs),
    "]"
  ),
  configs
)

dados_heatmap_ARI = as.data.frame(
  as.table(matriz_ARI)
)

names(dados_heatmap_ARI) = c(
  "Configuracao_1",
  "Configuracao_2",
  "ARI"
)

dados_heatmap_ARI$Configuracao_1 = factor(
  dados_heatmap_ARI$Configuracao_1,
  levels = ordem_ARI
)

dados_heatmap_ARI$Configuracao_2 = factor(
  dados_heatmap_ARI$Configuracao_2,
  levels = rev(ordem_ARI)
)

# Criar os rotulos dos 15 modelos
configs = names(particoes)

rotulos = setNames(
  paste0(
    "plain(DP)[plain(LoL)*','*",
    seq_along(configs),
    "]"
  ),
  configs
)

# ============================================================
# GRAFICO
# ============================================================

p_heatmap_ARI = ggplot(
  dados_heatmap_ARI,
  aes(
    x = Configuracao_1,
    y = Configuracao_2,
    fill = ARI
  )
) +
  
  geom_tile(
    color = "white",
    linewidth = 0.6
  ) +
  
  geom_text(
    aes(
      label = sprintf("%.2f", ARI),
      color = ARI >= 0.65
    ),
    size = 2.7,
    fontface = "bold"
  ) +
  
  scale_color_manual(
    values = c(
      "FALSE" = "#243447",
      "TRUE" = "white"
    ),
    guide = "none"
  ) +
  
  scale_fill_gradientn(
    colors = c(
      "#C65D3B",
      "#FFFFFF",
      "royalblue"
    ),
    values = scales::rescale(
      c(-1, 0, 0.5, 1),
      from = c(-1, 1)
    ),
    limits = c(-1, 1),
    name = "ARI"
  ) +
  
  scale_x_discrete(
    labels = function(x) {
      parse(text = unname(rotulos[x]))
    }
  ) +
  
  scale_y_discrete(
    labels = function(x) {
      parse(text = unname(rotulos[x]))
    }
  ) +
  
  coord_fixed() +
  
  labs(
    x = NULL,
    y = NULL,
    title = NULL,
    subtitle = NULL,
    caption = NULL
  ) +
  
  theme_minimal(base_size = 11) +
  
  theme(
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    plot.caption = element_blank(),
    
    axis.text.x = element_text(
      angle = 60,
      hjust = 1,
      size = 9,
      color = "black"
    ),
    
    axis.text.y = element_text(
      size = 9,
      color = "black"
    ),
    
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    
    panel.grid = element_blank(),
    
    plot.margin = margin(15, 15, 15, 15)
  )

# Exibir
print(p_heatmap_ARI)


ggsave(
  filename = "ari_dpmm_lol.pdf",
  plot = p_heatmap_ARI,
  width = 11,
  height = 9,
  bg = "white"
)

# PCA DPMM

nome_config = "G0-5_beta_forte__Gamma(2,4)"

particao = particoes[[nome_config]]

cluster_dpmm = factor(
  particao$Cluster_DPMM[match(
    as.character(dados$Jogador),
    as.character(particao$Jogador)
  )]
)

K_dpmm = nlevels(cluster_dpmm)

pca_dpmm = prcomp(
  X_padronizado,
  center = FALSE,
  scale. = FALSE
)

# Variância explicada
variancia_dpmm = 100 * pca_dpmm$sdev^2 /
  sum(pca_dpmm$sdev^2)

cat(
  "Variância explicada pelos 3 primeiros componentes:",
  round(sum(variancia_dpmm[1:3]), 2),
  "%\n"
)

tabela_pca_dpmm = data.frame(
  Jogador = dados$Jogador,
  PC1 = pca_dpmm$x[, 1],
  PC2 = pca_dpmm$x[, 2],
  PC3 = pca_dpmm$x[, 3],
  Cluster = cluster_dpmm
)

paleta_base = c(
  "#F0E442",
  "#003B5C",
  "#56B4E9",
  "#009E73",
  "#D55E00",
  "#CC79A7",
  "#E69F00",
  "#0072B2",
  "#332288",
  "#88CCEE",
  "#44AA99",
  "#117733",
  "#999933",
  "#DDCC77",
  "#CC6677",
  "#882255",
  "#AA4499",
  "#661100",
  "#6699CC",
  "#AA4466",
  "#4477AA",
  "#228833",
  "#EE6677",
  "#BBBBBB"
)

simbolos_base = c(
  "circle",
  "square",
  "diamond",
  "cross",
  "x",
  "circle-open",
  "square-open",
  "diamond-open"
)

niveis = levels(tabela_pca_dpmm$Cluster)

cores = rep(
  paleta_base,
  length.out = K_dpmm
)

simbolos = rep(
  simbolos_base,
  length.out = K_dpmm
)

names(cores) = niveis
names(simbolos) = niveis

tamanhos = table(tabela_pca_dpmm$Cluster)

tabela_pca_dpmm$N_Cluster = as.integer(
  tamanhos[as.character(tabela_pca_dpmm$Cluster)]
)

tabela_pca_dpmm$Hover = paste0(
  "<b>Jogador:</b> ", tabela_pca_dpmm$Jogador,
  "<br><b>Cluster:</b> ", tabela_pca_dpmm$Cluster,
  "<br><b>Jogadores no cluster:</b> ",
  tabela_pca_dpmm$N_Cluster
)

p_pca_dpmm = plot_ly()

for (cl in niveis) {
  
  d = tabela_pca_dpmm[
    tabela_pca_dpmm$Cluster == cl,
    ,
    drop = FALSE
  ]
  
  p_pca_dpmm = p_pca_dpmm %>%
    add_trace(
      x = d$PC1,
      y = d$PC2,
      z = d$PC3,
      
      type = "scatter3d",
      mode = "markers",
      
      name = paste("Cluster", cl),
      
      text = d$Hover,
      hoverinfo = "text",
      
      marker = list(
        color = unname(cores[cl]),
        symbol = unname(simbolos[cl]),
        size = 5,
        opacity = 0.9,
        line = list(
          color = "#333333",
          width = 0.5
        )
      )
    )
}

p_pca_dpmm = p_pca_dpmm %>%
  layout(
    title = paste0(
      "PCA 3D — DPMM (K = ",
      K_dpmm,
      ")"
    ),
    
    scene = list(
      xaxis = list(
        title = paste0(
          "PC1 (", round(variancia_dpmm[1], 1), "%)"
        )
      ),
      
      yaxis = list(
        title = paste0(
          "PC2 (", round(variancia_dpmm[2], 1), "%)"
        )
      ),
      
      zaxis = list(
        title = paste0(
          "PC3 (", round(variancia_dpmm[3], 1), "%)"
        )
      )
    ),
    
    legend = list(
      title = list(text = "Clusters"),
      x = 1.02,
      y = 1
    ),
    
    margin = list(
      l = 0,
      r = 0,
      b = 0,
      t = 65
    )
  )

p_pca_dpmm

htmlwidgets::saveWidget(
  widget = p_pca_dpmm,
  file = "PCA_3D_DPMM_G05_Gamma24_Scale_Cholesky.html",
  selfcontained = TRUE
)














# ANALISE E VALIDACAO DOS AGRUPAMENTOS DE JOGADORES

stopifnot(
  length(cluster_kmeans) == nrow(X),
  length(cluster_gmm) == nrow(X)
)

diagnostico_cluster = function(X, clusters, metodo, distancia = NULL) {
  
  if (length(clusters) != nrow(X)) {
    stop("Numero de clusters diferente do numero de linhas de X.")
  }
  
  if (anyNA(clusters)) {
    stop("O vetor de clusters contem valores ausentes.")
  }
  
  clusters = as.factor(clusters)
  
  if (is.null(distancia)) {
    distancia = dist(X)
  }
  
  K = nlevels(clusters)
  N = nrow(X)
  
  tamanhos = as.data.frame(table(clusters))
  
  names(tamanhos) = c(
    "Cluster",
    "N_jogadores"
  )
  
  tamanhos$Percentual =
    100 * tamanhos$N_jogadores / N
  
  perfil = aggregate(
    as.data.frame(X),
    by = list(Cluster = clusters),
    FUN = mean
  )
  
  dispersao = aggregate(
    as.data.frame(X),
    by = list(Cluster = clusters),
    FUN = sd
  )

  silhouette_media = NA_real_
  silhouette_cluster = NULL
  silhouette_individual = NULL
  
  if (K >= 2 && K < N) {
    
    sil = cluster::silhouette(
      as.integer(clusters),
      distancia
    )
    
    silhouette_media = mean(
      sil[, "sil_width"]
    )
    
    silhouette_individual = data.frame(
      Cluster = clusters,
      Silhouette = sil[, "sil_width"]
    )
    
    silhouette_cluster = aggregate(
      Silhouette ~ Cluster,
      data = silhouette_individual,
      FUN = mean
    )
  }

  centroides = perfil
  
  matriz_centroides = as.matrix(
    centroides[, -1, drop = FALSE]
  )
  
  distancia_centroides = as.matrix(
    dist(matriz_centroides)
  )
  
  rownames(distancia_centroides) =
    as.character(centroides$Cluster)
  
  colnames(distancia_centroides) =
    as.character(centroides$Cluster)
  
  distancia_interna = sapply(
    levels(clusters),
    function(g) {
      
      indices = which(clusters == g)
      
      if (length(indices) < 2) {
        return(0)
      }
      
      X_cluster = X[
        indices,
        ,
        drop = FALSE
      ]
      
      mean(as.vector(dist(X_cluster)))
    }
  )
  
  return(
    list(
      metodo = metodo,
      tamanhos = tamanhos,
      perfil = perfil,
      dispersao = dispersao,
      silhouette_media = silhouette_media,
      silhouette_cluster = silhouette_cluster,
      silhouette_individual = silhouette_individual,
      distancia_centroides = distancia_centroides,
      distancia_interna = distancia_interna
    )
  )
}

estrutura_kmeans = diagnostico_cluster(
  X = X,
  clusters = cluster_kmeans,
  metodo = "K-means",
  distancia = distancia_X
)

estrutura_gmm = diagnostico_cluster(
  X = X,
  clusters = cluster_gmm,
  metodo = "GMM",
  distancia = distancia_X
)

estruturas_dpmm = lapply(
  names(particoes),
  function(nome) {
    
    diagnostico_cluster(
      X = X,
      clusters = particoes[[nome]]$Cluster_DPMM,
      metodo = nome,
      distancia = distancia_X
    )
  }
)

names(estruturas_dpmm) = names(particoes)

View(estruturas_dpmm)
View(estrutura_kmeans)
View(estrutura_gmm)

resumir_estrutura = function(estrutura) {
  
  tamanhos = estrutura$tamanhos$N_jogadores
  
  data.frame(
    Modelo = estrutura$metodo,
    
    Numero_clusters = length(tamanhos),
    
    Clusters_unitarios = sum(tamanhos == 1),
    
    Menor_cluster = min(tamanhos),
    
    Indice_silhueta = estrutura$silhouette_media,
    
    stringsAsFactors = FALSE
  )
}

# Reunir todos os modelos
todas_estruturas = c(
  list(
    estrutura_kmeans,
    estrutura_gmm
  ),
  estruturas_dpmm
)

# Construir a tabela comparativa
tabela_comparativa = do.call(
  rbind,
  lapply(todas_estruturas, resumir_estrutura)
)

rownames(tabela_comparativa) = NULL

# Arredondar o índice de silhueta
tabela_comparativa$Indice_silhueta = round(
  tabela_comparativa$Indice_silhueta,
  4
)

View(tabela_comparativa)


# ============================================================
# TABELA COMPARATIVA DOS METODOS
# Incluindo K a posteriori e K de Dahl
# ============================================================

# Reunir todos os modelos
todas_estruturas = c(
  list(
    estrutura_kmeans,
    estrutura_gmm
  ),
  estruturas_dpmm
)

# Construir a tabela comparativa
tabela_comparativa = do.call(
  rbind,
  lapply(todas_estruturas, resumir_estrutura)
)

rownames(tabela_comparativa) = NULL

# ------------------------------------------------------------
# Recuperar K posterior e K de Dahl do DPMM
# ------------------------------------------------------------

# Identificadores usados no codigo original
chaves_dpmm = paste(
  resultados_dpmm$Config_G0,
  resultados_dpmm$Prior_alpha,
  sep = "__"
)

# Localizar as configuracoes correspondentes
indices_dpmm = match(
  tabela_comparativa$Modelo,
  chaves_dpmm
)

# Media posterior do numero de grupos
tabela_comparativa$K_posteriori = round(
  resultados_dpmm$K_posterior_media[indices_dpmm],
  2
)

# Numero de grupos da particao de Dahl
tabela_comparativa$K_Dahl =
  resultados_dpmm$K_Dahl[indices_dpmm]

# ------------------------------------------------------------
# Organizar as colunas
# ------------------------------------------------------------

tabela_comparativa = tabela_comparativa %>%
  select(
    Modelo,
    Numero_clusters,
    K_posteriori,
    K_Dahl,
    Clusters_unitarios,
    Menor_cluster,
    Indice_silhueta
  )

# Arredondar a silhueta
tabela_comparativa$Indice_silhueta = round(
  tabela_comparativa$Indice_silhueta,
  4
)

# Visualizar
print(tabela_comparativa)

View(tabela_comparativa)



jogadores_por_config = lapply(
  particoes,
  function(p) {
    
    split(
      p$Jogador,
      p$Cluster_DPMM
    )
  }
)

nome_config = "G0-5_beta_forte__Gamma(2,4)"

for (nome in nome_config) {
  cat("\nCONFIGURACAO:", nome, "\n")
  print(jogadores_por_config[[nome]])
}

# Recuperar os jogadores agrupados
clusters_config = jogadores_por_config[[nome_config]]

# A tabela original da particao ja contem as duas colunas
tabela_clusters = data.frame(
  Jogador = as.character(
    particoes[[nome_config]]$Jogador
  ),
  Cluster = as.character(
    particoes[[nome_config]]$Cluster_DPMM
  )
)

rownames(tabela_clusters) = NULL

# Visualizar tabela
print(tabela_clusters)


# Perfil médio dos clisters nas variáveis originais
dados_cluster = dados %>%
  left_join(
    tabela_clusters,
    by = "Jogador"
  )

perfil_clusters = dados_cluster %>%
  group_by(Cluster) %>%
  summarise(
    
    N = n(),
    
    across(
      where(is.numeric),
      ~ mean(.x, na.rm = TRUE)
    ),
    
    .groups = "drop"
  )

print(perfil_clusters)






# ============================================================
# MAPA DE CALOR DOS PERFIS — DPMM COM CHOLESKY
# Grupos e variáveis ordenados por similaridade
# Escala de cores fixa: -3 a 3
# ============================================================

# 1. PACOTES
library(dplyr)
library(reshape2)
library(scales)

# ------------------------------------------------------------
# 2. RECUPERAR A PARTICAO DO DPMM COM CHOLESKY
# ------------------------------------------------------------

nome_config = "G0-5_beta_forte__Gamma(2,4)"

particao = particoes[[nome_config]]

if (is.null(particao)) {
  stop("Configuracao nao encontrada em 'particoes'.")
}

# Alinhar os grupos com a ordem dos jogadores nos dados
indices = match(
  as.character(dados$Jogador),
  as.character(particao$Jogador)
)

if (anyNA(indices)) {
  stop("Existem jogadores sem correspondencia na particao.")
}

cluster_dpmm = factor(
  particao$Cluster_DPMM[indices]
)

# Conferir compatibilidade dos dados
X_padronizado = as.matrix(X_padronizado)

stopifnot(
  nrow(X_padronizado) == length(cluster_dpmm),
  !anyNA(cluster_dpmm),
  all(is.finite(X_padronizado))
)

# ------------------------------------------------------------
# 3. CALCULAR PERFIS MEDIOS DOS GRUPOS
# ------------------------------------------------------------

# Usar variaveis padronizadas ANTES de Cholesky
# para manter a interpretacao original das variaveis

base_perfil = data.frame(
  Grupo = cluster_dpmm,
  as.data.frame(X_padronizado),
  check.names = FALSE
)

variaveis = colnames(X_padronizado)

# Media de cada variavel e tamanho de cada grupo
perfil_medio = base_perfil %>%
  group_by(Grupo) %>%
  summarise(
    N_jogadores = n(),
    across(
      all_of(variaveis),
      mean
    ),
    .groups = "drop"
  )

# ------------------------------------------------------------
# 4. CONSTRUIR MATRIZ DOS PERFIS
# ------------------------------------------------------------

matriz_perfis = as.matrix(
  perfil_medio[, variaveis, drop = FALSE]
)

rownames(matriz_perfis) = paste0(
  "Grupo ",
  perfil_medio$Grupo,
  " (n=",
  perfil_medio$N_jogadores,
  ")"
)

# ------------------------------------------------------------
# 5. ORDENAR OS GRUPOS POR SIMILARIDADE
# ------------------------------------------------------------

# Distancia euclidiana entre perfis medios
dist_linhas = dist(matriz_perfis)

# Agrupamento hierarquico de Ward
hc_linhas = hclust(
  dist_linhas,
  method = "ward.D2"
)

ordem_linhas = rownames(matriz_perfis)[
  hc_linhas$order
]

# ------------------------------------------------------------
# 6. ORDENAR AS VARIAVEIS POR SIMILARIDADE
# ------------------------------------------------------------

# Distancia entre os perfis das variaveis
dist_colunas = dist(t(matriz_perfis))

hc_colunas = hclust(
  dist_colunas,
  method = "ward.D2"
)

ordem_colunas = colnames(matriz_perfis)[
  hc_colunas$order
]

# ------------------------------------------------------------
# 7. REORGANIZAR A MATRIZ
# ------------------------------------------------------------

matriz_perfis_ord = matriz_perfis[
  ordem_linhas,
  ordem_colunas,
  drop = FALSE
]

dados_heatmap = reshape2::melt(
  matriz_perfis_ord
)

names(dados_heatmap) = c(
  "Grupo",
  "Variavel",
  "Media"
)

# Garantir que o ggplot respeite a ordenacao
dados_heatmap$Grupo = factor(
  dados_heatmap$Grupo,
  levels = rev(ordem_linhas)
)

dados_heatmap$Variavel = factor(
  dados_heatmap$Variavel,
  levels = ordem_colunas
)

# ------------------------------------------------------------
# 8. MAPA DE CALOR
# ------------------------------------------------------------

grafico_heatmap = ggplot(
  dados_heatmap,
  aes(
    x = Variavel,
    y = Grupo,
    fill = Media
  )
) +
  
  geom_tile(
    color = "white",
    linewidth = 0.35
  ) +
  
  scale_fill_gradient2(
    low = "#C65D3B",
    mid = "#FFFFFF",
    high = "royalblue",
    midpoint = 0,
    limits = c(-3, 3),
    breaks = seq(-3, 3, by = 1),
    oob = scales::squish,
    name = "Media do grupo\n(z-score)"
  ) +
  
  labs(
    x = NULL,
    y = NULL,
    title = NULL,
    subtitle = NULL,
    caption = NULL
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1,
      size = 10,
      color = "black"
    ),
    
    axis.text.y = element_text(
      size = 9,
      color = "black"
    ),
    
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    plot.caption = element_blank(),
    
    panel.grid = element_blank(),
    
    axis.ticks = element_blank(),
    
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    
    plot.background = element_rect(
      fill = "white",
      color = NA
    )
  )

# ------------------------------------------------------------
# 9. EXIBIR O GRAFICO
# ------------------------------------------------------------

print(grafico_heatmap)

# ------------------------------------------------------------
# 10. SALVAR AS FIGURAS
# ------------------------------------------------------------

ggsave(
  filename = "mapa_calor_perfis.pdf",
  plot = grafico_heatmap,
  width = 12,
  height = 9,
  dpi = 300,
  bg = "white"
)



# ============================================================
# TRAJETORIA A POSTERIORI DE K — DP_LoL,10
# Media a posteriori e K de Dahl
# ============================================================

# 1. Selecionar modelo
nome_config = "G0-5_beta_forte__Gamma(2,4)"

ajuste = modelos[[nome_config]]

if (is.null(ajuste)) {
  stop("Modelo DP_LoL,10 nao encontrado.")
}

# 2. Recuperar amostras posteriores
K_samples = ajuste$K_samples

if (is.null(K_samples) || length(K_samples) == 0) {
  K_samples = apply(
    ajuste$groups_samples,
    1,
    function(z) length(unique(z))
  )
}

K_samples = as.integer(K_samples)

# 3. Organizar dados
dados_K = data.frame(
  Amostra = seq_along(K_samples),
  K = K_samples
)

media_K = mean(K_samples)
K_Dahl = 22

# 4. Linhas de referencia
referencias = data.frame(
  Referencia = factor(
    c("Média a posteriori", "K de Dahl"),
    levels = c("Média a posteriori", "K de Dahl")
  ),
  Valor = c(media_K, K_Dahl)
)

# 5. Grafico
p_trace_K = ggplot(
  dados_K,
  aes(x = Amostra, y = K)
) +
  
  # Trajetoria em azul-acinzentado mais claro
  geom_line(
    color = "#8299AC",
    linewidth = 0.38,
    alpha = 0.65
  ) +
  
  # Linhas horizontais mais espessas
  geom_hline(
    data = referencias,
    aes(
      yintercept = Valor,
      color = Referencia,
      linetype = Referencia
    ),
    linewidth = 1.15,
    inherit.aes = FALSE
  ) +
  
  # Cores das linhas de referencia
  scale_color_manual(
    values = c(
      "Média a posteriori" = "#C62828",
      "K de Dahl" = "#7030A0"
    ),
    name = NULL
  ) +
  
  # Tracos maiores e distintos
  scale_linetype_manual(
    values = c(
      "Média a posteriori" = "longdash",
      "K de Dahl" = "dotdash"
    ),
    name = NULL
  ) +
  
  scale_x_continuous(
    breaks = seq(
      0,
      length(K_samples),
      by = 250
    ),
    expand = expansion(mult = c(0, 0.01))
  ) +
  
  scale_y_continuous(
    breaks = seq(19, 30, by = 5),
    expand = expansion(mult = 0)
  ) +
  
  coord_cartesian(
    ylim = c(19, 30)
  ) +
  
  labs(
    x = "Amostra a posteriori",
    y = "Número de grupos (K)"
  ) +
  
  theme_classic(base_size = 12) +
  
  theme(
    panel.grid = element_blank(),
    
    axis.line = element_line(
      color = "black",
      linewidth = 0.6
    ),
    
    axis.ticks = element_line(
      color = "black",
      linewidth = 0.5
    ),
    
    axis.text = element_text(
      size = 10,
      color = "black"
    ),
    
    axis.title = element_text(
      size = 11,
      color = "black"
    ),
    
    legend.position = "bottom",
    legend.direction = "horizontal",
    
    legend.text = element_text(
      size = 10,
      color = "black"
    ),
    
    # Aumentar o comprimento das linhas na legenda
    legend.key.width = grid::unit(2.5, "cm"),
    
    legend.background = element_blank(),
    
    plot.background = element_rect(
      fill = "white",
      color = NA
    )
  ) +
  
  guides(
    color = guide_legend(
      override.aes = list(linewidth = 1.4)
    ),
    linetype = guide_legend(
      override.aes = list(linewidth = 1.4)
    )
  )

# Exibir
print(p_trace_K)

# Salvar em PDF vetorial
ggsave(
  filename = "Trajetoria_K_DPLoL10.pdf",
  plot = p_trace_K,
  width = 7,
  height = 4.1,
  units = "in",
  bg = "white"
)
















# ============================================================
# AUTOCORRELACAO DE K — DP_LoL,10
# G0-5_beta_forte + Gamma(2,4)
# ============================================================

if (!exists("modelos")) {
  
  objetos = readRDS(
    "objetos_completos_dpmm_lol_2025.rds"
  )
  
  modelos = objetos$modelos
}

nome_config = "G0-5_beta_forte__Gamma(2,4)"

ajuste = modelos[[nome_config]]

if (is.null(ajuste)) {
  stop("Modelo DP_LoL,10 nao encontrado.")
}

# ------------------------------------------------------------
# 2. RECUPERAR AS AMOSTRAS A POSTERIORI DE K
# ------------------------------------------------------------

K_samples = ajuste$K_samples

if (is.null(K_samples) || length(K_samples) == 0) {
  
  K_samples = apply(
    ajuste$groups_samples,
    1,
    function(z) length(unique(z))
  )
}

K_samples = as.numeric(K_samples)

if (length(K_samples) < 2 || anyNA(K_samples)) {
  stop("Amostras de K insuficientes ou com valores ausentes.")
}

# ------------------------------------------------------------
# 3. CALCULAR A AUTOCORRELACAO
# ------------------------------------------------------------

acf_K = acf(
  K_samples,
  lag.max = min(60, length(K_samples) - 1),
  plot = FALSE
)

dados_acf = data.frame(
  Lag = as.numeric(acf_K$lag)[-1],
  ACF = as.numeric(acf_K$acf)[-1]
)

# Excluir lag 0, cuja autocorrelacao e sempre 1

# Limites aproximados sob hipotese de ruido branco
limite = 1.96 / sqrt(length(K_samples))


# ------------------------------------------------------------
# 4. GRAFICO DA AUTOCORRELACAO
# ------------------------------------------------------------

p_acf_K = ggplot(
  dados_acf,
  aes(x = Lag, y = ACF)
) +
  
  geom_segment(
    aes(
      xend = Lag,
      y = 0,
      yend = ACF
    ),
    color = "royalblue",
    linewidth = 0.75
  ) +
  
  # Linha horizontal em zero
  geom_hline(
    yintercept = 0,
    color = "black",
    linewidth = 0.45
  ) +
  
  scale_x_continuous(
    breaks = seq(0, 60, by = 10),
    limits = c(0, 60),
    expand = expansion(mult = 0)
  ) +
  
  scale_y_continuous(
    breaks = scales::breaks_pretty(n = 6)
  ) +
  
  labs(
    x = "Defasagem (lag)",
    y = "Autocorrelação de K",
    title = NULL,
    subtitle = NULL,
    caption = NULL
  ) +
  
  theme_classic(base_size = 12) +
  
  theme(
    panel.grid = element_blank(),
    
    axis.line = element_line(
      color = "black",
      linewidth = 0.6
    ),
    
    axis.ticks = element_line(
      color = "black",
      linewidth = 0.5
    ),
    
    axis.text.x = element_text(
      size = 10,
      color = "black"
    ),
    
    axis.text.y = element_text(
      size = 10,
      color = "black"
    ),
    
    axis.title.x = element_text(
      size = 11,
      color = "black"
    ),
    
    axis.title.y = element_text(
      size = 11,
      color = "black"
    ),
    
    plot.background = element_rect(
      fill = "white",
      color = NA
    )
  )

# Exibir
print(p_acf_K)

# Salvar em PDF
ggsave(
  filename = "autocorrelacao_k.pdf",
  plot = p_acf_K,
  width = 7,
  height = 3.8,
  units = "in",
  bg = "white"
)










# ============================================================
# MEDIA ACUMULADA DE K — DP_LoL,10
# G0-5_beta_forte + Gamma(2,4)
# ============================================================

# ------------------------------------------------------------
# 1. RECUPERAR OS MODELOS, SE NECESSARIO
# ------------------------------------------------------------

if (!exists("modelos")) {
  
  objetos = readRDS(
    "objetos_completos_dpmm_lol_2025.rds"
  )
  
  modelos = objetos$modelos
}

# ------------------------------------------------------------
# 2. SELECIONAR O MODELO DP_LoL,10
# ------------------------------------------------------------

nome_config = "G0-5_beta_forte__Gamma(2,4)"

ajuste = modelos[[nome_config]]

if (is.null(ajuste)) {
  stop("Modelo DP_LoL,10 nao encontrado.")
}

# ------------------------------------------------------------
# 3. RECUPERAR AS AMOSTRAS A POSTERIORI DE K
# ------------------------------------------------------------

K_samples = ajuste$K_samples

if (is.null(K_samples) || length(K_samples) == 0) {
  
  K_samples = apply(
    ajuste$groups_samples,
    1,
    function(z) length(unique(z))
  )
}

K_samples = as.numeric(K_samples)

if (length(K_samples) < 2 || anyNA(K_samples)) {
  stop("Amostras de K insuficientes ou invalidas.")
}

# ------------------------------------------------------------
# 4. CALCULAR A MEDIA ACUMULADA
# ------------------------------------------------------------

n_amostras = length(K_samples)

media_acumulada = cumsum(K_samples) /
  seq_len(n_amostras)

# Media a posteriori total
media_K = mean(K_samples)

# Numero de grupos da particao de Dahl
K_Dahl = 22

dados_media = data.frame(
  Amostra = seq_len(n_amostras),
  Media_acumulada = media_acumulada
)

cat(
  "Media a posteriori de K:",
  round(media_K, 4),
  "\n"
)

cat(
  "K de Dahl:",
  K_Dahl,
  "\n"
)

cat(
  "Media acumulada final:",
  round(tail(media_acumulada, 1), 4),
  "\n"
)

# ------------------------------------------------------------
# 5. LINHAS DE REFERENCIA
# ------------------------------------------------------------

referencias = data.frame(
  Referencia = factor(
    c("Média a posteriori", "K de Dahl"),
    levels = c("Média a posteriori", "K de Dahl")
  ),
  Valor = c(media_K, K_Dahl)
)

# ------------------------------------------------------------
# 6. GRAFICO DA MEDIA ACUMULADA
# ------------------------------------------------------------

p_media_K = ggplot(
  dados_media,
  aes(
    x = Amostra,
    y = Media_acumulada
  )
) +
  
  # Curva da media acumulada
  geom_line(
    color = "royalblue",
    linewidth = 0.95
  ) +
  
  # Media a posteriori e K de Dahl
  geom_hline(
    data = referencias,
    aes(
      yintercept = Valor,
      color = Referencia,
      linetype = Referencia
    ),
    linewidth = 1,
    inherit.aes = FALSE
  ) +
  
  scale_color_manual(
    values = c(
      "Média a posteriori" = "#C62828",
      "K de Dahl" = "#7030A0"
    ),
    name = NULL
  ) +
  
  scale_linetype_manual(
    values = c(
      "Média a posteriori" = "longdash",
      "K de Dahl" = "dotdash"
    ),
    name = NULL
  ) +
  
  scale_x_continuous(
    breaks = seq(0, n_amostras, by = 250),
    expand = expansion(mult = c(0, 0.01))
  ) +
  
  scale_y_continuous(
    breaks = seq(19, 29, by = 1)
  ) +
  
  coord_cartesian(
    ylim = c(19, 29)
  ) +
  
  labs(
    x = "Amostra a posteriori",
    y = "Média acumulada de K",
    title = NULL,
    subtitle = NULL,
    caption = NULL
  ) +
  
  theme_classic(base_size = 12) +
  
  theme(
    panel.grid = element_blank(),
    
    axis.line = element_line(
      color = "black",
      linewidth = 0.6
    ),
    
    axis.ticks = element_line(
      color = "black",
      linewidth = 0.5
    ),
    
    axis.text = element_text(
      size = 10,
      color = "black"
    ),
    
    axis.title = element_text(
      size = 11,
      color = "black"
    ),
    
    legend.position = "bottom",
    
    legend.direction = "horizontal",
    
    legend.text = element_text(
      size = 10,
      color = "black"
    ),
    
    legend.key.width = grid::unit(2.5, "cm"),
    
    legend.background = element_blank(),
    
    plot.background = element_rect(
      fill = "white",
      color = NA
    )
  )

print(p_media_K)

ggsave(
  filename = "media_acumulada_k.pdf",
  plot = p_media_K,
  width = 7,
  height = 4.1,
  units = "in",
  bg = "white"
)








# ============================================================
# TRAJETORIA A POSTERIORI DE ALPHA — DP_LoL,10
# G0-5_beta_forte + Gamma(2,4)
# ============================================================

# ------------------------------------------------------------
# 1. RECUPERAR OS MODELOS, SE NECESSARIO
# ------------------------------------------------------------

if (!exists("modelos")) {
  
  objetos = readRDS(
    "objetos_completos_dpmm_lol_2025.rds"
  )
  
  modelos = objetos$modelos
}

# ------------------------------------------------------------
# 2. SELECIONAR O MODELO DP_LoL,10
# ------------------------------------------------------------

nome_config = "G0-5_beta_forte__Gamma(2,4)"

ajuste = modelos[[nome_config]]

if (is.null(ajuste)) {
  stop("Modelo DP_LoL,10 nao encontrado.")
}

# ------------------------------------------------------------
# 3. RECUPERAR AS AMOSTRAS A POSTERIORI DE ALPHA
# ------------------------------------------------------------

alpha_samples = as.numeric(ajuste$alpha_samples)

if (
  length(alpha_samples) < 2 ||
  any(!is.finite(alpha_samples))
) {
  stop("Amostras de alpha ausentes ou invalidas.")
}

# ------------------------------------------------------------
# 4. ORGANIZAR OS DADOS
# ------------------------------------------------------------

n_amostras = length(alpha_samples)

dados_alpha = data.frame(
  Amostra = seq_len(n_amostras),
  Alpha = alpha_samples
)

media_alpha = mean(alpha_samples)
mediana_alpha = median(alpha_samples)
dp_alpha = sd(alpha_samples)

cat(
  "Media a posteriori de alpha:",
  round(media_alpha, 4), "\n"
)

cat(
  "Mediana a posteriori:",
  round(mediana_alpha, 4), "\n"
)

cat(
  "Desvio-padrao posterior:",
  round(dp_alpha, 4), "\n"
)

cat(
  "Numero de amostras:",
  n_amostras, "\n"
)

# ------------------------------------------------------------
# 5. LINHA DE REFERENCIA
# ------------------------------------------------------------

referencia_alpha = data.frame(
  Referencia = "Média a posteriori",
  Valor = media_alpha
)

# ------------------------------------------------------------
# 6. GRAFICO DA TRAJETORIA DE ALPHA
# ------------------------------------------------------------

p_trace_alpha = ggplot(
  dados_alpha,
  aes(
    x = Amostra,
    y = Alpha
  )
) +
  
  # Trajetoria de alpha
  geom_line(
    color = "#8299AC",
    linewidth = 0.45,
    alpha = 0.70
  ) +
  
  # Media a posteriori
  geom_hline(
    data = referencia_alpha,
    aes(
      yintercept = Valor,
      color = Referencia,
      linetype = Referencia
    ),
    linewidth = 1.15,
    inherit.aes = FALSE
  ) +
  
  scale_color_manual(
    values = c(
      "Média a posteriori" = "#C62828"
    ),
    name = NULL
  ) +
  
  scale_linetype_manual(
    values = c(
      "Média a posteriori" = "longdash"
    ),
    name = NULL
  ) +
  
  scale_x_continuous(
    breaks = seq(
      0,
      n_amostras,
      by = 250
    ),
    limits = c(0, n_amostras),
    expand = expansion(mult = c(0, 0.01))
  ) +
  
  scale_y_continuous(
    breaks = scales::breaks_pretty(n = 6),
    expand = expansion(mult = c(0.03, 0.06))
  ) +
  
  labs(
    x = "Amostra a posteriori",
    y = expression(
      "Parâmetro de concentração " * alpha
    ),
    title = NULL,
    subtitle = NULL,
    caption = NULL
  ) +
  
  theme_classic(base_size = 12) +
  
  theme(
    # Remover grade
    panel.grid = element_blank(),
    
    # Eixos pretos
    axis.line = element_line(
      color = "black",
      linewidth = 0.6
    ),
    
    axis.ticks = element_line(
      color = "black",
      linewidth = 0.5
    ),
    
    axis.text.x = element_text(
      size = 10,
      color = "black"
    ),
    
    axis.text.y = element_text(
      size = 10,
      color = "black"
    ),
    
    axis.title.x = element_text(
      size = 11,
      color = "black"
    ),
    
    axis.title.y = element_text(
      size = 11,
      color = "black"
    ),
    
    # Legenda abaixo do grafico
    legend.position = "bottom",
    
    legend.direction = "horizontal",
    
    legend.text = element_text(
      size = 10,
      color = "black"
    ),
    
    legend.key.width = grid::unit(2.5, "cm"),
    
    legend.background = element_blank(),
    
    plot.background = element_rect(
      fill = "white",
      color = NA
    )
  )

# ------------------------------------------------------------
# 7. EXIBIR
# ------------------------------------------------------------

print(p_trace_alpha)

# ------------------------------------------------------------
# 8. SALVAR PARA A DISSERTACAO
# ------------------------------------------------------------

ggsave(
  filename = "Trajetoria_Alpha_DPLoL10.pdf",
  plot = p_trace_alpha,
  width = 7,
  height = 4.1,
  units = "in",
  bg = "white"
)
























# ============================================================
# COMPOSICAO DOS 22 GRUPOS — DP_LoL,10
# ============================================================

# 1. Recuperar a particao
if (!exists("particoes")) {
  objetos = readRDS(
    "objetos_completos_dpmm_lol_2025.rds"
  )
  
  particoes = objetos$particoes
}

nome_config = "G0-5_beta_forte__Gamma(2,4)"

particao = particoes[[nome_config]]

if (is.null(particao)) {
  stop("Particao DP_LoL,10 nao encontrada.")
}

# 2. Calcular os tamanhos dos grupos
frequencias = table(particao$Cluster_DPMM)

tamanhos_grupos = data.frame(
  Cluster = as.integer(names(frequencias)),
  N_jogadores = as.integer(frequencias)
)

tamanhos_grupos$Grupo = paste0(
  "Grupo ",
  tamanhos_grupos$Cluster
)

tamanhos_grupos$Percentual =
  100 * tamanhos_grupos$N_jogadores /
  sum(tamanhos_grupos$N_jogadores)


# Atualizar a classificação dos grupos
tamanhos_grupos$Categoria = ifelse(
  tamanhos_grupos$N_jogadores <= 10,
  "Até 10 jogadores",
  "Mais de 10 jogadores"
)

# 4. Grafico de barras horizontais
p_tamanhos_dpmm = ggplot(
  tamanhos_grupos,
  aes(
    x = reorder(Grupo, N_jogadores),
    y = N_jogadores,
    fill = Categoria
  )
) +
  
  geom_col(
    width = 0.75
  ) +
  
  geom_text(
    aes(label = N_jogadores),
    hjust = -0.25,
    size = 3.4
  ) +
  
  coord_flip() +
  
  scale_fill_manual(
    values = c(
      "Até 10 jogadores" = "#C65D3B",
      "Mais de 10 jogadores" = "royalblue"
    ),
    name = NULL
  ) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.12))
  ) +
  
  labs(
    x = NULL,
    y = "Número de jogadores"
  ) +
  
  theme_classic(base_size = 12) +
  
  theme(
    panel.grid = element_blank(),
    
    axis.line = element_line(
      color = "black",
      linewidth = 0.6
    ),
    
    axis.text = element_text(
      color = "black",
      size = 10
    ),
    
    axis.title = element_text(
      color = "black",
      size = 11
    ),
    
    legend.position = "bottom",
    
    legend.text = element_text(
      size = 10
    ),
    
    plot.background = element_rect(
      fill = "white",
      color = NA
    )
  )

print(p_tamanhos_dpmm)

ggsave(
  "tamanho_grupos.pdf",
  plot = p_tamanhos_dpmm,
  width = 7,
  height = 6,
  bg = "white"
)






# ============================================================
# NOMES DOS JOGADORES POR GRUPO — DP_LoL,10
# ============================================================

# Recuperar particoes, se necessario
if (!exists("particoes")) {
  objetos = readRDS(
    "objetos_completos_dpmm_lol_2025.rds"
  )
  particoes = objetos$particoes
}

# Selecionar o modelo
nome_config = "G0-5_beta_forte__Gamma(2,4)"

particao = particoes[[nome_config]]

if (is.null(particao)) {
  stop("Particao nao encontrada.")
}

# Separar os jogadores pelos grupos
jogadores_por_grupo = split(
  as.character(particao$Jogador),
  particao$Cluster_DPMM
)

# Ordenar numericamente os grupos
jogadores_por_grupo = jogadores_por_grupo[
  order(as.integer(names(jogadores_por_grupo)))
]

# ------------------------------------------------------------
# MOSTRAR ATE 15 NOMES DE CADA GRUPO NO CONSOLE
# ------------------------------------------------------------

for (grupo in names(jogadores_por_grupo)) {
  
  jogadores = jogadores_por_grupo[[grupo]]
  
  cat(
    "\nGRUPO", grupo,
    "- Total:", length(jogadores),
    "jogadores\n"
  )
  
  cat(
    paste(head(jogadores, 15), collapse = ", "),
    "\n"
  )
}

# ------------------------------------------------------------
# SALVAR TODOS OS NOMES DOS 22 GRUPOS EM TXT
# ------------------------------------------------------------

linhas = unlist(
  lapply(names(jogadores_por_grupo), function(grupo) {
    
    jogadores = jogadores_por_grupo[[grupo]]
    
    c(
      paste0(
        "GRUPO ", grupo,
        " (n=", length(jogadores), ")"
      ),
      paste(jogadores, collapse = ", "),
      ""
    )
  })
)

writeLines(
  linhas,
  "Jogadores_por_Grupo_DPLoL10.txt"
)

cat("\nArquivo TXT salvo com sucesso!\n")

