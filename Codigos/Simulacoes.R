source("Funcoes_DPMM.R")

cholesky_transformation = function(Y) {
  Y = as.matrix(Y)
  
  S = stats::cov(Y)
  L = t(chol(S))
  
  Y_centered = sweep(Y, 2, colMeans(Y), "-")
  
  t(solve(L, t(Y_centered)))
}


criar_medias_grupos = function(K, p, d,
                               padrao_medias = "todas_variaveis") {
  if (!padrao_medias %in% c("uma_variavel", "todas_variaveis")) {
    stop("padrao_medias deve ser 'uma_variavel' ou 'todas_variaveis'.")
  }
  medias = matrix(0, nrow = K, ncol = p)
  if (padrao_medias == "uma_variavel") {
    medias[, 1] = (0:(K - 1)) * d
  } else {
    medias[,] = outer(0:(K - 1), rep(d, p), FUN = "*")
  }
  medias
}

alocar_n_por_grupo = function(n, K) {
  if (n < K) stop("O tamanho amostral n deve ser maior 
  ou igual ao numero de grupos K.")
  tamanhos = rep(n %/% K, K)
  resto = n %% K
  if (resto > 0) tamanhos[seq_len(resto)] = tamanhos[seq_len(resto)] + 1L
  tamanhos
}

seed_simulacao = function(seed_base, id_dados, id_metodo = 0L,
                          id_alpha = 0L, id_g0 = 0L) {
  valor = as.double(seed_base) + as.double(id_dados) * 100000 +
    as.double(id_metodo) * 1000 + as.double(id_alpha) * 100 + as.double(id_g0)
  as.integer(valor %% 2147483646 + 1)
}

executar_cronometrado = function(fun) {
  inicio = proc.time()[["elapsed"]]
  valor = tryCatch(
    list(ok = TRUE, valor = fun(), erro = NA_character_),
    error = function(e) list(ok = FALSE, valor = NULL, erro = conditionMessage(e))
  )
  valor$tempo = unname(proc.time()[["elapsed"]] - inicio)
  valor
}


executar_simulacoes_dpmm = function(
    n_replicas = 2,
    n_values = c(100),
    p_values = c(2),
    K_values = c(2, 5, 10),
    kmeans_grid = 1:15,
    separacoes = c(baixa = 1, media = 2, muito_alta = 4),
    sd = 1,
    padrao_medias = "todas_variaveis",
    usar_cholesky = FALSE,
    dahl_restringir_K_moda = FALSE,
    alpha_priors = configuracoes_alpha(),
    g0_configs = configuracoes_G0(sensibilidade = FALSE),
    dpmm_n_samples = 1000,
    dpmm_burnin = 1000,
    dpmm_thin = 1,
    dpmm_coclustering_samples = 500,
    dpmm_alpha_init = 1,
    dpmm_K_init = 3,
    gmm_G_max = 15,
    incluir_hierarquico = FALSE,
    salvar_dados = FALSE,
    arquivo = "resultados_simulacoes_dpmm.xlsx",
    seed_base = 20261015,
    verbose = TRUE,
    dpmm_fun = clustering_dpmm,
    max_linhas_aba = 900000,
    limite_aviso_dpmm = 10000,
    confirmar_execucao_grande = FALSE) {
  
  pacotes = c("mvtnorm", "mclust", "openxlsx", "cluster")
  
  ausentes = pacotes[
    !vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)
  ]
  
  if (length(ausentes)) {
    stop(
      "Instale os pacotes necessarios: install.packages(c(",
      paste(sprintf('"%s"', ausentes), collapse = ", "),
      "))"
    )
  }
  
  if (!is.function(dpmm_fun)) {
    stop("'dpmm_fun' deve ser uma funcao, como clustering_dpmm.")
  }
  
  args_funcao = names(formals(dpmm_fun))
  
  args_necessarios = c(
    "X", "prioris", "hiperparametros",
    "n_final_samples", "burnin", "saltos",
    "calcular_coclustering", "n_coclustering_samples"
  )
  
  if (!all(args_necessarios %in% args_funcao)) {
    stop(
      paste0(
        "A funcao dpmm_fun nao tem a assinatura esperada. ",
        "Argumentos encontrados: ",
        paste(args_funcao, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  # Aceitar nomes alternativos para os hiperparametros de G0.
  if (!"Config_G0" %in% names(g0_configs) &&
      "config" %in% names(g0_configs)) {
    names(g0_configs)[names(g0_configs) == "config"] = "Config_G0"
  }
  
  if (!"alfa0" %in% names(g0_configs) &&
      "a0" %in% names(g0_configs)) {
    names(g0_configs)[names(g0_configs) == "a0"] = "alfa0"
  }
  
  if (!"beta0" %in% names(g0_configs) &&
      "b0" %in% names(g0_configs)) {
    names(g0_configs)[names(g0_configs) == "b0"] = "beta0"
  }
  
  if (!"mu0" %in% names(g0_configs)) g0_configs$mu0 = 0
  if (!"mu0_tipo" %in% names(g0_configs)) g0_configs$mu0_tipo = "valor"
  if (!"beta0_tipo" %in% names(g0_configs)) g0_configs$beta0_tipo = "valor"
  
  colunas_g0 = c(
    "Config_G0", "mu0", "mu0_tipo", "lambda0", "alfa0",
    "beta0", "beta0_tipo"
  )
  
  if (!all(colunas_g0 %in% names(g0_configs))) {
    stop(
      "'g0_configs' precisa conter Config_G0, mu0, mu0_tipo, lambda0, ",
      "alfa0, beta0 e beta0_tipo."
    )
  }
  
  g0_configs = g0_configs[, colunas_g0, drop = FALSE]
  g0_configs$mu0_tipo = as.character(g0_configs$mu0_tipo)
  g0_configs$beta0_tipo = as.character(g0_configs$beta0_tipo)
  
  tipos_mu0_validos = c("valor", "media_X")
  tipos_beta0_validos = c("valor", "variancia_X")
  if (any(!g0_configs$mu0_tipo %in% tipos_mu0_validos)) {
    stop("mu0_tipo deve ser 'valor' ou 'media_X'.")
  }
  if (any(!g0_configs$beta0_tipo %in% tipos_beta0_validos)) {
    stop("beta0_tipo deve ser 'valor' ou 'variancia_X'.")
  }
  
  if (length(n_replicas) != 1 ||
      !is.finite(n_replicas) ||
      n_replicas < 1 ||
      n_replicas != as.integer(n_replicas)) {
    stop("'n_replicas' deve ser um inteiro positivo.")
  }
  
  if (!length(n_values) ||
      any(!is.finite(n_values)) ||
      any(n_values < 2) ||
      any(n_values != as.integer(n_values))) {
    stop("Todos os valores de n_values devem ser inteiros >= 2.")
  }
  
  if (!length(p_values) ||
      any(!is.finite(p_values)) ||
      any(p_values < 1) ||
      any(p_values != as.integer(p_values))) {
    stop("Todos os valores de p_values devem ser inteiros positivos.")
  }
  
  if (!length(K_values) ||
      any(!is.finite(K_values)) ||
      any(K_values < 2) ||
      any(K_values != as.integer(K_values))) {
    stop("Todos os K_values devem ser inteiros >= 2.")
  }
  
  if (!length(kmeans_grid) ||
      any(!is.finite(kmeans_grid)) ||
      any(kmeans_grid < 1) ||
      any(kmeans_grid != as.integer(kmeans_grid))) {
    stop("'kmeans_grid' deve conter inteiros positivos.")
  }
  
  kmeans_grid = sort(unique(as.integer(kmeans_grid)))
  
  if (!any(kmeans_grid <= min(n_values))) {
    stop(
      "'kmeans_grid' deve conter pelo menos um k menor ou igual ao menor n_values."
    )
  }
  
  if (min(n_values) < max(K_values)) {
    stop("Todos os tamanhos amostrais devem ser >= ao maior K selecionado.")
  }
  
  if (length(gmm_G_max) != 1 ||
      !is.finite(gmm_G_max) ||
      gmm_G_max < 1 ||
      gmm_G_max != as.integer(gmm_G_max)) {
    stop("'gmm_G_max' deve ser um inteiro positivo.")
  }
  
  if (is.null(names(separacoes)) ||
      any(!nzchar(names(separacoes))) ||
      any(!is.finite(separacoes)) ||
      any(separacoes <= 0)) {
    stop(
      "'separacoes' deve ser um vetor numerico nomeado ",
      "com valores positivos."
    )
  }
  
  if (length(sd) != 1 || !is.finite(sd) || sd <= 0) {
    stop("'sd' deve ser positivo.")
  }
  if (length(padrao_medias) != 1L ||
      !padrao_medias %in% c("uma_variavel", "todas_variaveis")) {
    stop("padrao_medias invalido.")
  }
  if (!is.logical(usar_cholesky) || length(usar_cholesky) != 1L ||
      is.na(usar_cholesky)) {
    stop("usar_cholesky deve ser TRUE ou FALSE.")
  }
  if (!is.logical(dahl_restringir_K_moda) ||
      length(dahl_restringir_K_moda) != 1L ||
      is.na(dahl_restringir_K_moda)) {
    stop("dahl_restringir_K_moda deve ser TRUE ou FALSE.")
  }
  
  if (length(max_linhas_aba) != 1 ||
      !is.finite(max_linhas_aba) ||
      max_linhas_aba < 1 ||
      max_linhas_aba != as.integer(max_linhas_aba) ||
      max_linhas_aba > 1048575) {
    stop(
      "'max_linhas_aba' deve ser inteiro entre 1 e 1.048.575."
    )
  }
  
  if (length(limite_aviso_dpmm) != 1 ||
      !is.finite(limite_aviso_dpmm) ||
      limite_aviso_dpmm < 1) {
    stop("'limite_aviso_dpmm' deve ser um numero positivo.")
  }
  
  if (!all(c("Prior_alpha", "a_alpha", "b_alpha") %in%
           names(alpha_priors))) {
    stop(
      "'alpha_priors' precisa ter as colunas Prior_alpha, a_alpha e b_alpha."
    )
  }
  
  if (!nrow(alpha_priors) || !nrow(g0_configs)) {
    stop("As tabelas alpha_priors e g0_configs nao podem estar vazias.")
  }
  
  if (any(!is.finite(alpha_priors$a_alpha)) ||
      any(!is.finite(alpha_priors$b_alpha)) ||
      any(alpha_priors$a_alpha <= 0 | alpha_priors$b_alpha <= 0)) {
    stop("a_alpha e b_alpha devem ser positivos e finitos.")
  }
  
  if (any(!is.finite(g0_configs$mu0)) ||
      any(!is.finite(g0_configs$lambda0)) ||
      any(!is.finite(g0_configs$alfa0)) ||
      any(!is.finite(g0_configs$beta0)) ||
      any(g0_configs$lambda0 <= 0 |
          g0_configs$alfa0 <= 0 |
          g0_configs$beta0 <= 0)) {
    stop("lambda0, alfa0 e beta0 devem ser positivos e finitos.")
  }
  
  if (dpmm_n_samples < 1 ||
      dpmm_n_samples != as.integer(dpmm_n_samples) ||
      dpmm_burnin < 0 ||
      dpmm_burnin != as.integer(dpmm_burnin) ||
      dpmm_thin < 1 ||
      dpmm_thin != as.integer(dpmm_thin)) {
    stop(
      "Use dpmm_n_samples >= 1, dpmm_burnin >= 0 e ",
      "dpmm_thin >= 1, todos inteiros."
    )
  }
  if (length(dpmm_coclustering_samples) != 1L ||
      !is.finite(dpmm_coclustering_samples) ||
      dpmm_coclustering_samples < 1L ||
      dpmm_coclustering_samples != as.integer(dpmm_coclustering_samples)) {
    stop("dpmm_coclustering_samples deve ser um inteiro positivo.")
  }
  
  grid_dados = expand.grid(
    K = as.integer(K_values),
    Separacao = names(separacoes),
    n = as.integer(n_values),
    p = as.integer(p_values),
    Replica = seq_len(as.integer(n_replicas)),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  
  grid_dados$Distancia = unname(
    separacoes[as.character(grid_dados$Separacao)]
  )
  
  letras_K = c(`2` = "A", `5` = "B", `10` = "C")
  
  indice_sep = match(
    as.character(grid_dados$Separacao),
    names(separacoes)
  )
  
  if (all(as.character(K_values) %in% names(letras_K)) &&
      length(separacoes) == 3L) {
    grid_dados$Cenario = paste0(
      unname(letras_K[as.character(grid_dados$K)]),
      indice_sep
    )
  } else {
    grid_dados$Cenario = paste0(
      "K", grid_dados$K, "_S", indice_sep
    )
  }
  
  # ID no formato A1R1D000001.
  grid_dados$ID_Dados = sprintf(
    "%sR%dD%06d",
    grid_dados$Cenario,
    grid_dados$Replica,
    seq_len(nrow(grid_dados))
  )
  
  ordem_cenarios = c(
    "A1", "A2", "A3",
    "B1", "B2", "B3",
    "C1", "C2", "C3"
  )
  
  grid_dados = grid_dados[
    order(
      match(grid_dados$Cenario, ordem_cenarios),
      grid_dados$n,
      grid_dados$p,
      grid_dados$Replica
    ),
    ,
    drop = FALSE
  ]
  
  rownames(grid_dados) = NULL
  
  resultados = list()
  dados_salvos = list()
  
  contador_resultados = 0L
  contador_dados = 0L
  
  total_dados = nrow(grid_dados)
  total_dpmm = total_dados * nrow(alpha_priors) * nrow(g0_configs)
  
  if (verbose) {
    message("Processando ", total_dados, " conjuntos de dados.")
  }
  
  if (total_dpmm > limite_aviso_dpmm &&
      !isTRUE(confirmar_execucao_grande)) {
    stop(
      sprintf(
        paste0(
          "O plano pede %s ajustes DPMM, acima do limite de aviso (%s). ",
          "Para confirmar esse custo, defina ",
          "confirmar_execucao_grande = TRUE."
        ),
        format(total_dpmm, big.mark = "."),
        format(limite_aviso_dpmm, big.mark = ".")
      ),
      call. = FALSE
    )
  }
  
  calcular_silhouette_media = function(clusters, distancia) {
    if (is.null(clusters) || length(clusters) < 3L || is.null(distancia)) {
      return(NA_real_)
    }
    
    clusters = as.character(clusters)
    if (anyNA(clusters)) {
      return(NA_real_)
    }
    
    rotulos = as.integer(factor(clusters))
    k_particao = length(unique(rotulos))
    n_particao = length(rotulos)
    
    if (k_particao < 2L || k_particao >= n_particao) {
      return(NA_real_)
    }
    
    tryCatch({
      sil = cluster::silhouette(rotulos, distancia)
      valor = mean(sil[, "sil_width"], na.rm = TRUE)
      if (length(valor) != 1L || !is.finite(valor)) NA_real_ else valor
    }, error = function(e) NA_real_)
  }
  
  registrar = function(
    d, metodo, clusters, tempo, erro = NA_character_,
    prior_alpha = "N/A", a_alpha = NA_real_, b_alpha = NA_real_,
    config_g0 = "N/A", mu0 = NA_real_, lambda0 = NA_real_,
    alfa0 = NA_real_, beta0 = NA_real_,
    k_post_media = NA_real_, k_post_mediana = NA_real_,
    k_post_moda = NA_real_, k_fornecido = FALSE,
    k_testado = NA_integer_,
    mu0_tipo = NA_character_, beta0_tipo = NA_character_,
    mu0_vetor = NA_character_, beta0_vetor = NA_character_,
    coclustering_dahl_loss = NA_real_,
    dahl_draw_index = NA_integer_,
    silhouette_coclustering = NA_real_,
    n_coclustering_amostras = NA_integer_,
    k_dahl_livre = NA_integer_, p_k_igual_1 = NA_real_,
    maior_grupo_frac_media = NA_real_, k_restrito_dahl = NA_integer_) {
    
    ok = !is.null(clusters) &&
      length(clusters) == length(d$y_true) &&
      !anyNA(clusters)
    
    if (ok) {
      clusters = as.character(clusters)
      k_estimado = length(unique(clusters))
      
      ari = mclust::adjustedRandIndex(
        as.character(d$y_true), clusters
      )
      
      nmi = calcular_NMI(clusters, d$y_true)
      silhouette_media = calcular_silhouette_media(
        clusters,
        d$dist_X
      )
      
      acerto_k = if (k_fornecido) {
        NA_real_
      } else {
        as.numeric(k_estimado == d$K)
      }
      
      status = "OK"
      erro_registro = NA_character_
      
    } else {
      k_estimado = NA_integer_
      ari = NA_real_
      nmi = NA_real_
      silhouette_media = NA_real_
      acerto_k = NA_real_
      status = "ERRO"
      
      erro_registro = erro
      
      if (is.na(erro_registro) || !nzchar(erro_registro)) {
        erro_registro = "O metodo nao retornou uma classificacao valida."
      }
    }
    
    sufixo_execucao = if (metodo == "Processo_Dirichlet") {
      paste(prior_alpha, config_g0, sep = "__")
    } else if (metodo == "KMeans" && !is.na(k_testado)) {
      paste0("K", k_testado)
    } else {
      ""
    }
    
    id_execucao = paste(
      d$ID_Dados,
      metodo,
      sufixo_execucao,
      sep = "__"
    )
    
    data.frame(
      ID_Dados = d$ID_Dados,
      ID_Execucao = id_execucao,
      Replica = d$Replica,
      Cenario = d$Cenario,
      K_verdadeiro = d$K,
      Separacao = d$Separacao,
      Distancia = d$Distancia,
      n = d$n,
      p = d$p,
      Metodo = metodo,
      K_estimado = k_estimado,
      K_testado = if (is.na(k_testado)) NA_integer_ else as.integer(k_testado),
      Acerto_K = acerto_k,
      ARI = ari,
      NMI = nmi,
      Silhouette = silhouette_media,
      K_selecionado_por_Silhouette = NA,
      Tempo_segundos = tempo,
      K_fornecido_ao_metodo = k_fornecido,
      Prior_alpha = prior_alpha,
      a_alpha = a_alpha,
      b_alpha = b_alpha,
      Config_G0 = config_g0,
      mu0_tipo = mu0_tipo,
      mu0 = mu0,
      mu0_vetor = mu0_vetor,
      lambda0 = lambda0,
      alfa0 = alfa0,
      beta0_tipo = beta0_tipo,
      beta0 = beta0,
      beta0_vetor = beta0_vetor,
      K_posterior_media = k_post_media,
      K_posterior_mediana = k_post_mediana,
      K_posterior_moda = k_post_moda,
      Coclustering_Dahl_Loss = coclustering_dahl_loss,
      Dahl_Draw_Index = dahl_draw_index,
      Silhouette_Coclustering = silhouette_coclustering,
      N_coclustering_amostras = n_coclustering_amostras,
      K_Dahl_Livre = k_dahl_livre,
      Prob_Posterior_K1 = p_k_igual_1,
      Fracao_Media_Maior_Grupo = maior_grupo_frac_media,
      K_Restrito_Dahl = k_restrito_dahl,
      Status = status,
      Erro = erro_registro,
      stringsAsFactors = FALSE
    )
  }
  
  calcular_resumo_medias = function(dados, n_replicas) {
    
    df_resumo = data.frame()
    
    if (is.null(dados) || nrow(dados) == 0L) {
      return(df_resumo)
    }
    
    chaves = c(
      "Metodo", "Cenario", "K_verdadeiro",
      "Separacao", "Distancia", "n", "p",
      "Prior_alpha", "a_alpha", "b_alpha",
      "Config_G0", "mu0_tipo", "lambda0", "alfa0", "beta0_tipo"
    )
    
    chave_df = dados[, chaves, drop = FALSE]
    
    chave_txt = lapply(chave_df, function(x) {
      ifelse(is.na(x), "<NA>", as.character(x))
    })
    
    grupo = do.call(paste, c(chave_txt, sep = "\r"))
    indices = split(seq_len(nrow(dados)), grupo)
    
    estatisticas = function(x) {
      x = x[!is.na(x) & is.finite(x)]
      nx = length(x)
      
      if (nx == 0L) {
        return(c(
          N_validas = 0,
          media = NA_real_,
          dp = NA_real_,
          mediana = NA_real_
        ))
      }
      
      c(
        N_validas = nx,
        media = mean(x),
        dp = if (nx >= 2L) stats::sd(x) else NA_real_,
        mediana = stats::median(x)
      )
    }
    
    metricas = c(
      "ARI", "NMI", "Silhouette", "K_estimado",
      "Silhouette_Coclustering", "Coclustering_Dahl_Loss",
      "mu0", "beta0"
    )
    
    linhas_resumo = vector("list", length(indices))
    contador_resumo = 0L
    
    for (idx in indices) {
      z = dados[idx, , drop = FALSE]
      linha = z[1, chaves, drop = FALSE]
      
      linha$N_replicas_planejadas = as.integer(n_replicas)
      
      for (m in metricas) {
        est = estatisticas(z[[m]])
        
        for (nm in names(est)) {
          nome_coluna = paste0(m, "_", nm)
          
          # Nao incluir ARI_N_validas nem n_execucoes no resumo.
          if (tolower(nome_coluna) %in% c("ari_n_validas", "n_execucoes")) {
            next
          }
          
          linha[[nome_coluna]] = unname(est[[nm]])
        }
      }
      
      tempos = z$Tempo_segundos
      tempos = tempos[!is.na(tempos) & is.finite(tempos)]
      
      linha$Tempo_segundos_media = if (length(tempos)) {
        mean(tempos)
      } else {
        NA_real_
      }
      
      contador_resumo = contador_resumo + 1L
      linhas_resumo[[contador_resumo]] = linha
    }
    
    df_resumo = do.call(rbind, linhas_resumo)
    rownames(df_resumo) = NULL
    
    nomes_minusculos = tolower(names(df_resumo))
    colunas_remover = names(df_resumo)[
      nomes_minusculos %in% c(
        "n_execucoes", "ari_n_validas", "k_estimado_n_validas", "acerto_k"
      ) | startsWith(nomes_minusculos, "k_posterior_")
    ]
    
    if (length(colunas_remover)) {
      df_resumo[colunas_remover] = NULL
    }
    
    if (all(c("Tempo_segundos_media", "ARI_media") %in% names(df_resumo))) {
      ordem_colunas = setdiff(names(df_resumo), "Tempo_segundos_media")
      pos_ari = match("ARI_media", ordem_colunas)
      ordem_colunas = append(
        ordem_colunas,
        "Tempo_segundos_media",
        after = pos_ari - 1L
      )
      df_resumo = df_resumo[, ordem_colunas, drop = FALSE]
    }
    
    ordem_cenario = match(
      df_resumo$Cenario,
      c("A1", "A2", "A3", "B1", "B2", "B3", "C1", "C2", "C3")
    )
    
    ordem_metodo = match(
      df_resumo$Metodo,
      c("KMeans", "GMM", "Hierarquico_Ward", "Processo_Dirichlet")
    )
    
    df_resumo = df_resumo[
      order(
        ordem_cenario,
        df_resumo$n,
        df_resumo$p,
        ordem_metodo,
        df_resumo$Prior_alpha,
        df_resumo$Config_G0
      ),
      ,
      drop = FALSE
    ]
    
    rownames(df_resumo) = NULL
    df_resumo
  }
  
  for (i in seq_len(total_dados)) {
    
    drow = grid_dados[i, , drop = FALSE]
    
    K = drow$K[[1]]
    n = drow$n[[1]]
    p = drow$p[[1]]
    rep_i = drow$Replica[[1]]
    dist = drow$Distancia[[1]]
    
    sid = seed_simulacao(seed_base, i)
    
    # Mensagem simples, sem barra de progresso.
    if (verbose) {
      message(
        sprintf(
          "Dados %d/%d: ID=%s | K=%d | separacao=%s | n=%d | p=%d",
          i, total_dados, drow$ID_Dados[[1]], K,
          drow$Separacao[[1]], n, p
        )
      )
    }
    
    # Geracao dos dados.
    gerado = tryCatch({
      set.seed(sid)
      
      n_por_grupo = alocar_n_por_grupo(n, K)
      
      medias = criar_medias_grupos(
        K = K, p = p, d = dist, padrao_medias = padrao_medias
      )
      
      Xk = lapply(seq_len(K), function(k) {
        mvtnorm::rmvnorm(
          n_por_grupo[k],
          mean = medias[k, ],
          sigma = diag(sd^2, p)
        )
      })
      
      X = do.call(rbind, Xk)
      colnames(X) = paste0("X", seq_len(p))
      
      y_true = factor(
        rep(seq_len(K), times = n_por_grupo)
      )
      
      list(X = X, y_true = y_true, n_por_grupo = n_por_grupo)
      
    }, error = function(e) {
      list(erro = conditionMessage(e))
    })
    
    if (!is.null(gerado$erro)) {
      warning(
        "Falha ao gerar ", drow$ID_Dados[[1]], ": ",
        gerado$erro
      )
      
      next
    }
    
    # Mesmo pre-processamento para TODOS os metodos (DPMM, K-means e GMM).
    # Fazer uma unica vez por conjunto de dados, sem usar os rotulos reais.
    X_ajuste = if (usar_cholesky) {
      cholesky_transformation(gerado$X)
    } else {
      gerado$X
    }
    colnames(X_ajuste) = colnames(gerado$X)
    if (usar_cholesky && i == 1L && verbose) {
      erro_centro = max(abs(colMeans(X_ajuste)))
      erro_cov = max(abs(stats::cov(X_ajuste) - diag(ncol(X_ajuste))))
      message(sprintf(
        "Cholesky aplicado: erro maximo media = %.2e; cov = %.2e",
        erro_centro, erro_cov
      ))
    }
    
    d = list(
      ID_Dados = drow$ID_Dados[[1]],
      Replica = rep_i,
      Cenario = as.character(drow$Cenario[[1]]),
      K = K,
      Separacao = as.character(drow$Separacao[[1]]),
      Distancia = dist,
      n = n,
      p = p,
      X = X_ajuste,
      X_original = gerado$X,
      y_true = gerado$y_true,
      dist_X = stats::dist(X_ajuste)
    )
    
    # Salvar os dados gerados, se solicitado.
    if (isTRUE(salvar_dados)) {
      contador_dados = contador_dados + 1L
      
      dados_salvos[[contador_dados]] = data.frame(
        ID_Dados = d$ID_Dados,
        Replica = rep_i,
        Cenario = d$Cenario,
        K_verdadeiro = K,
        Separacao = d$Separacao,
        Distancia = dist,
        n = n,
        p = p,
        Observacao = seq_len(n),
        Grupo_real = as.character(d$y_true),
        stats::setNames(as.data.frame(d$X_original),
                        paste0("Original_", colnames(d$X_original))),
        stats::setNames(as.data.frame(d$X),
                        paste0("Ajuste_", colnames(d$X))),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }
    
    kmeans_grid_atual = kmeans_grid[kmeans_grid <= n]
    resultados_km_candidatos = vector("list", length(kmeans_grid_atual))
    
    for (j_km in seq_along(kmeans_grid_atual)) {
      k_kmeans = kmeans_grid_atual[[j_km]]
      set.seed(seed_simulacao(seed_base, i, 1L, k_kmeans))
      
      km = executar_cronometrado(function() {
        stats::kmeans(
          d$X,
          centers = k_kmeans,
          nstart = 50,
          iter.max = 100
        )$cluster
      })
      
      res_km = registrar(
        d, "KMeans",
        if (km$ok) km$valor else NULL,
        km$tempo,
        erro = km$erro,
        # A selecao final de k nao usa o K verdadeiro; Acerto_K pode
        # ser calculado depois, apenas para avaliar o resultado.
        k_fornecido = FALSE,
        k_testado = k_kmeans
      )
      
      res_km$K_selecionado_por_Silhouette = FALSE
      resultados_km_candidatos[[j_km]] = res_km
      
      if (!km$ok && isTRUE(verbose)) {
        message(
          "K-means falhou para k=", k_kmeans,
          " no conjunto ", i, "/", total_dados,
          ": ", km$erro
        )
      }
    }
    
    if (length(resultados_km_candidatos)) {
      df_km_candidatos = do.call(rbind, resultados_km_candidatos)
      rownames(df_km_candidatos) = NULL
      
      candidatos_validos = which(
        df_km_candidatos$Status == "OK" &
          is.finite(df_km_candidatos$Silhouette)
      )
      
      if (length(candidatos_validos)) {
        indice_melhor = candidatos_validos[
          which.max(df_km_candidatos$Silhouette[candidatos_validos])
        ]
      } else {
        candidatos_fallback = which(
          df_km_candidatos$Status == "OK" &
            df_km_candidatos$K_testado >= 2L
        )
        if (!length(candidatos_fallback)) {
          candidatos_fallback = which(df_km_candidatos$Status == "OK")
        }
        
        indice_melhor = if (length(candidatos_fallback)) {
          candidatos_fallback[
            which.min(df_km_candidatos$K_testado[candidatos_fallback])
          ]
        } else {
          NA_integer_
        }
        
        if (isTRUE(verbose)) {
          message(
            "Silhouette indisponivel para o conjunto ", d$ID_Dados,
            "; aplicado criterio de fallback para selecionar k."
          )
        }
      }
      
      if (!is.na(indice_melhor)) {
        df_km_candidatos$K_selecionado_por_Silhouette[indice_melhor] = TRUE
        km_selecionado = df_km_candidatos[
          indice_melhor, , drop = FALSE
        ]
        
        contador_resultados = contador_resultados + 1L
        resultados[[contador_resultados]] = km_selecionado
      }
      
    }
    
    set.seed(seed_simulacao(seed_base, i, 2L))
    
    gmm = executar_cronometrado(function() {
      
      X_gmm = as.matrix(d$X)
      storage.mode(X_gmm) = "double"
      
      if (nrow(X_gmm) < 2L || ncol(X_gmm) < 1L) {
        stop("Dados insuficientes para ajustar o GMM.")
      }
      
      if (any(!is.finite(X_gmm))) {
        stop("Os dados do GMM contêm valores ausentes ou não finitos.")
      }
      
      G = seq_len(
        min(as.integer(gmm_G_max), nrow(X_gmm) - 1L)
      )
      
      ajuste_gmm = mclust::Mclust(
        data = X_gmm,
        G = G,
        verbose = FALSE
      )
      
      if (is.null(ajuste_gmm$classification)) {
        stop("O GMM não retornou classificações.")
      }
      
      as.integer(ajuste_gmm$classification)
    })
    
    res = registrar(
      d, "GMM",
      if (gmm$ok) gmm$valor else NULL,
      gmm$tempo,
      erro = gmm$erro
    )
    
    contador_resultados = contador_resultados + 1L
    resultados[[contador_resultados]] = res
    
    if (!gmm$ok && isTRUE(verbose)) {
      message(
        "GMM falhou para o conjunto ", i, "/", total_dados,
        ": ", gmm$erro
      )
    }
    
    if (isTRUE(incluir_hierarquico)) {
      hc = executar_cronometrado(function() {
        stats::cutree(
          stats::hclust(
            stats::dist(d$X),
            method = "ward.D2"
          ),
          k = K
        )
      })
      
      res = registrar(
        d, "Hierarquico_Ward",
        if (hc$ok) hc$valor else NULL,
        hc$tempo,
        erro = hc$erro,
        k_fornecido = TRUE
      )
      
      contador_resultados = contador_resultados + 1L
      resultados[[contador_resultados]] = res
    }
    
    for (ia in seq_len(nrow(alpha_priors))) {
      
      al = alpha_priors[ia, , drop = FALSE]
      
      for (ig in seq_len(nrow(g0_configs))) {
        
        g0 = g0_configs[ig, , drop = FALSE]
        
        alpha_label = as.character(al$Prior_alpha[[1]])
        g0_label = as.character(g0$Config_G0[[1]])
        
        mu0_tipo = as.character(g0$mu0_tipo[[1]])
        beta0_tipo = as.character(g0$beta0_tipo[[1]])
        p_atual = ncol(d$X)
        
        mu0_aplicado = switch(
          mu0_tipo,
          valor = rep(as.numeric(g0$mu0[[1]]), p_atual),
          media_X = colMeans(d$X),
          stop("Tipo de mu0 nao reconhecido: ", mu0_tipo)
        )
        beta0_aplicado = switch(
          beta0_tipo,
          valor = rep(as.numeric(g0$beta0[[1]]), p_atual),
          variancia_X = apply(d$X, 2, stats::var),
          stop("Tipo de beta0 nao reconhecido: ", beta0_tipo)
        )
        beta0_aplicado = pmax(beta0_aplicado, .Machine$double.eps)
        
        sd_seed = seed_simulacao(
          seed_base, i, 3L, ia, ig
        )
        
        K_init = if (
          is.character(dpmm_K_init) &&
          length(dpmm_K_init) == 1L &&
          dpmm_K_init == "K_verdadeiro"
        ) {
          K
        } else {
          as.integer(dpmm_K_init)
        }
        
        K_init = max(1L, min(n, K_init))
        
        dp = executar_cronometrado(function() {
          
          fit = dpmm_fun(
            X = d$X,
            prioris = list(
              mu0 = mu0_aplicado,
              lambda0 = g0$lambda0[[1]],
              alfa0 = g0$alfa0[[1]],
              beta0 = beta0_aplicado
            ),
            hiperparametros = list(
              a_alpha = al$a_alpha[[1]],
              b_alpha = al$b_alpha[[1]]
            ),
            n_final_samples = as.integer(dpmm_n_samples),
            burnin = as.integer(dpmm_burnin),
            saltos = as.integer(dpmm_thin),
            seed = sd_seed,
            verbose = FALSE,
            calcular_coclustering = TRUE,
            n_coclustering_samples = as.integer(dpmm_coclustering_samples),
            alpha_init = dpmm_alpha_init,
            K_init = K_init
          )
          
          C = fit$coclustering
          if (is.null(C) || !is.matrix(C) ||
              !all(dim(C) == c(nrow(d$X), nrow(d$X)))) {
            stop(
              "A matriz de coclustering nao foi retornada ou tem dimensoes invalidas."
            )
          }
          
          gs = fit$groups_samples
          if (is.null(gs) || !is.matrix(gs) || ncol(gs) != nrow(d$X)) {
            stop("O ajuste nao retornou groups_samples no formato
            draws x observacoes.")
          }
          
          indices_C = fit$indices_coclustering
          if (is.null(indices_C) || length(indices_C) < 1L) {
            if (nrow(gs) == min(as.integer(dpmm_n_samples),
                                as.integer(dpmm_coclustering_samples))) {
              indices_C = seq_len(nrow(gs))
            } else {
              stop(
                "O DPMM precisa retornar indices_coclustering para aplicar Dahl ",
                "aos mesmos draws que formaram C."
              )
            }
          }
          
          ks = fit$K_samples
          if (is.null(ks) || length(ks) < 1L) {
            gs = fit$groups_samples
            if (is.null(gs) || !is.matrix(gs)) {
              stop("O ajuste nao retornou K_samples nem groups_samples.")
            }
            ks = apply(gs, 1, function(g) length(unique(g)))
          }
          ks = as.integer(ks)
          tab_k = table(ks)
          k_moda = as.integer(names(tab_k)[which.max(tab_k)])
          
          consenso = dpmm_particao_dahl(
            groups_samples = gs,
            C = C,
            indices_draws = indices_C,
            K_restrito = if (dahl_restringir_K_moda) k_moda else NULL
          )
          
          # Diagnosticos: distinguir K=1 na posteriori de K=1 so em Dahl.
          prob_K1 = mean(ks == 1L)
          maior_grupo_frac_media = mean(apply(gs, 1L, function(z) {
            max(tabulate(as.integer(factor(z)))) / length(z)
          }))
          
          list(
            clusters = consenso$clusters,
            K_media = mean(ks),
            K_mediana = stats::median(ks),
            K_moda = k_moda,
            K_dahl_livre = consenso$K_dahl_livre,
            K_restrito_dahl = consenso$K_restrito,
            Prob_K1 = prob_K1,
            Maior_grupo_frac_media = maior_grupo_frac_media,
            Dahl_loss = consenso$dahl_loss,
            Dahl_draw_index = consenso$dahl_draw_index,
            Silhouette_Coclustering = consenso$silhouette_coclustering,
            N_coclustering = if (!is.null(fit$n_coclustering_samples)) {
              fit$n_coclustering_samples
            } else {
              length(indices_C)
            }
          )
        })
        
        if (dp$ok) {
          fit_part = dp$valor
          clusters_dp = fit_part$clusters
          extras = fit_part
        } else {
          clusters_dp = NULL
          
          extras = list(
            K_media = NA_real_,
            K_mediana = NA_real_,
            K_moda = NA_real_,
            K_dahl_livre = NA_integer_,
            K_restrito_dahl = NA_integer_,
            Prob_K1 = NA_real_,
            Maior_grupo_frac_media = NA_real_,
            Dahl_loss = NA_real_,
            Dahl_draw_index = NA_integer_,
            Silhouette_Coclustering = NA_real_,
            N_coclustering = NA_integer_
          )
        }
        
        res = registrar(
          d,
          "Processo_Dirichlet",
          clusters_dp,
          dp$tempo,
          erro = dp$erro,
          prior_alpha = alpha_label,
          a_alpha = al$a_alpha[[1]],
          b_alpha = al$b_alpha[[1]],
          config_g0 = g0_label,
          mu0 = mean(mu0_aplicado),
          lambda0 = g0$lambda0[[1]],
          alfa0 = g0$alfa0[[1]],
          beta0 = mean(beta0_aplicado),
          k_post_media = extras$K_media,
          k_post_mediana = extras$K_mediana,
          k_post_moda = extras$K_moda,
          mu0_tipo = mu0_tipo,
          beta0_tipo = beta0_tipo,
          mu0_vetor = paste(signif(mu0_aplicado, 6), collapse = ";"),
          beta0_vetor = paste(signif(beta0_aplicado, 6), collapse = ";"),
          coclustering_dahl_loss = extras$Dahl_loss,
          dahl_draw_index = extras$Dahl_draw_index,
          silhouette_coclustering = extras$Silhouette_Coclustering,
          n_coclustering_amostras = extras$N_coclustering,
          k_dahl_livre = extras$K_dahl_livre,
          p_k_igual_1 = extras$Prob_K1,
          maior_grupo_frac_media = extras$Maior_grupo_frac_media,
          k_restrito_dahl = extras$K_restrito_dahl
        )
        
        contador_resultados = contador_resultados + 1L
        resultados[[contador_resultados]] = res
        
        # Mostrar ARI e K de Dahl assim que CADA ajuste DPMM termina.
        if (isTRUE(verbose)) {
          cat(sprintf(
            "[DPMM] Cenario=%s | Replica=%d/%d | alpha=%s | G0=%s |
            K_real=%d | K_Dahl=%s | K_posterior_moda=%s | ARI=%s | 
            Status=%s\n",
            as.character(d$Cenario),
            as.integer(d$Replica),
            as.integer(n_replicas),
            alpha_label,
            g0_label,
            as.integer(d$K),
            as.character(res$K_estimado[[1]]),
            as.character(res$K_posterior_moda[[1]]),
            if (is.na(res$ARI[[1]])) "NA" else sprintf("%.4f", res$ARI[[1]]),
            as.character(res$Status[[1]])
          ))
          flush.console()
        }
      }
    }
    
    # Salvar o cenario assim que todas as suas simulacoes terminarem.
    cenario_atual = as.character(grid_dados$Cenario[i])
    
    fim_cenario = i == total_dados ||
      grid_dados$Cenario[i + 1L] != cenario_atual
    
    if (fim_cenario) {
      
      # Resultados do cenário atual
      resultados_atuais = do.call(rbind, resultados)
      
      resultados_cenario = resultados_atuais[
        resultados_atuais$Cenario == cenario_atual,
        ,
        drop = FALSE
      ]
      
      # Criar arquivo Excel do cenário
      wb_cenario = openxlsx::createWorkbook()
      
      # Na planilha de cenário, ocultar colunas auxiliares de diagnóstico.
      colunas_remover_cenario = c(
        "K_testado",
        "K_selecionado_por_Silhouette",
        "Status",
        "Erro",
        "K_fornecido_ao_metodo",
        "K_Fornecido"
      )
      resultados_cenario_planilha = resultados_cenario[
        , setdiff(names(resultados_cenario), colunas_remover_cenario),
        drop = FALSE
      ]
      
      # Sheet principal do cenário
      openxlsx::addWorksheet(wb_cenario, cenario_atual)
      
      openxlsx::writeData(
        wb_cenario,
        sheet = cenario_atual,
        x = resultados_cenario_planilha,
        withFilter = nrow(resultados_cenario_planilha) > 0
      )
      
      openxlsx::freezePane(
        wb_cenario,
        sheet = cenario_atual,
        firstRow = TRUE
      )
      
      resumo_medias_cenario = calcular_resumo_medias(
        resultados_cenario,
        n_replicas
      )
      
      openxlsx::addWorksheet(wb_cenario, "Resumo_Medias")
      
      openxlsx::writeData(
        wb_cenario,
        sheet = "Resumo_Medias",
        x = resumo_medias_cenario,
        withFilter = nrow(resumo_medias_cenario) > 0
      )
      
      openxlsx::freezePane(
        wb_cenario,
        sheet = "Resumo_Medias",
        firstRow = TRUE
      )
      
      # Salvar arquivo do cenário
      diretorio_saida = dirname(arquivo)
      
      if (!dir.exists(diretorio_saida)) {
        dir.create(
          diretorio_saida,
          recursive = TRUE,
          showWarnings = FALSE
        )
      }
      
      nome_base = tools::file_path_sans_ext(basename(arquivo))
      
      arquivo_cenario = file.path(
        diretorio_saida,
        paste0(nome_base, "_", cenario_atual, ".xlsx")
      )
      
      openxlsx::saveWorkbook(
        wb_cenario,
        file = arquivo_cenario,
        overwrite = TRUE
      )
      
      if (verbose) {
        message(
          "Cenário ", cenario_atual,
          " salvo em: ", arquivo_cenario
        )
      }
    }
    
  }
  
  df_resultados = if (length(resultados)) {
    do.call(rbind, resultados)
  } else {
    data.frame()
  }
  
  df_dados = if (length(dados_salvos)) {
    do.call(rbind, dados_salvos)
  } else {
    data.frame()
  }
  
  # Reutiliza o mesmo calculo aplicado ao fim de cada cenario.
  df_resumo = calcular_resumo_medias(df_resultados, n_replicas)
  
  config_execucao = data.frame(
    Parametro = c(
      "n_replicas", "n_values", "p_values", "K_values",
      "kmeans_grid", "separacoes", "sd", "padrao_medias",
      "distancia_adjacent_centroides", "usar_cholesky", "dahl_restringir_K_moda",
      "dpmm_n_samples", "dpmm_burnin",
      "dpmm_thin", "dpmm_coclustering_samples",
      "dpmm_alpha_init", "dpmm_K_init",
      "gmm_G_max", "incluir_hierarquico", "salvar_dados",
      "seed_base", "max_linhas_aba", "limite_aviso_dpmm",
      "confirmar_execucao_grande", "metrica_NMI", "metrica_Silhouette",
      "KMeans", "GMM", "DPMM_particao"
    ),
    
    Valor = c(
      as.character(n_replicas),
      paste(n_values, collapse = ", "),
      paste(p_values, collapse = ", "),
      paste(K_values, collapse = ", "),
      paste(kmeans_grid, collapse = ", "),
      paste(
        paste0(names(separacoes), "=", separacoes),
        collapse = "; "
      ),
      as.character(sd),
      padrao_medias,
      if (padrao_medias == "todas_variaveis") {
        "sqrt(p) * d: d deslocamento por coordenada"
      } else {
        "d: apenas a coordenada 1 separa os grupos"
      },
      as.character(usar_cholesky),
      as.character(dahl_restringir_K_moda),
      as.character(dpmm_n_samples),
      as.character(dpmm_burnin),
      as.character(dpmm_thin),
      as.character(dpmm_coclustering_samples),
      as.character(dpmm_alpha_init),
      as.character(dpmm_K_init),
      as.character(gmm_G_max),
      as.character(incluir_hierarquico),
      as.character(salvar_dados),
      as.character(seed_base),
      as.character(max_linhas_aba),
      as.character(limite_aviso_dpmm),
      as.character(confirmar_execucao_grande),
      "2*MI/(H_estimado+H_real)",
      "Media dos coeficientes silhouette; NA se k < 2 ou k >= n.",
      paste0(
        "Testa kmeans_grid; seleciona, por conjunto de dados, o k com ",
        "maior Silhouette media valida. Empate: menor k."
      ),
      "Mclust seleciona numero de componentes por BIC",
      paste0(
        "Dahl minimiza a perda em C sobre os draws usados para formar C. ",
        if (dahl_restringir_K_moda) {
          "Restricao adicional: K igual a moda posterior, NAO ao K verdadeiro. "
        } else {
          "Sem restricao sobre K. "
        },
        "K_Dahl_Livre registra o resultado sem restricao."
      )
    ),
    
    stringsAsFactors = FALSE
  )
  
  # Criacao do arquivo Excel
  wb = openxlsx::createWorkbook()
  
  estilo_cabecalho = openxlsx::createStyle(
    fontColour = "#FFFFFF",
    fgFill = "#244062",
    textDecoration = "bold",
    halign = "center",
    valign = "center",
    wrapText = TRUE,
    border = "Bottom",
    borderColour = "#17324D"
  )
  
  add_sheet = function(nome, dados, filtro = TRUE) {
    
    openxlsx::addWorksheet(
      wb, nome, gridLines = FALSE
    )
    
    if (is.null(dados) || !ncol(dados)) {
      dados = data.frame(
        Informacao = "Nenhum registro disponivel.",
        stringsAsFactors = FALSE
      )
    }
    
    openxlsx::writeData(
      wb, nome, dados,
      withFilter = isTRUE(filtro) && nrow(dados) > 0
    )
    
    if (ncol(dados) > 0) {
      
      openxlsx::addStyle(
        wb, nome, estilo_cabecalho,
        rows = 1,
        cols = seq_len(ncol(dados)),
        gridExpand = TRUE
      )
      
      openxlsx::freezePane(wb, nome, firstRow = TRUE)
      
      openxlsx::setColWidths(
        wb, nome,
        cols = seq_len(ncol(dados)),
        widths = 18
      )
      
      nomes_longos = intersect(
        c("Erro", "Valor", "ID_Execucao", "ID_Dados", "Cenario"),
        names(dados)
      )
      
      if (length(nomes_longos)) {
        openxlsx::setColWidths(
          wb, nome,
          cols = match(nomes_longos, names(dados)),
          widths = 25
        )
      }
      
      if ("Erro" %in% names(dados)) {
        openxlsx::setColWidths(
          wb, nome,
          cols = match("Erro", names(dados)),
          widths = 55
        )
      }
      
      # Melhorar a leitura das descricoes na configuracao.
      if (nome == "Config_Execucao" && "Valor" %in% names(dados)) {
        openxlsx::setColWidths(
          wb, nome,
          cols = match("Valor", names(dados)),
          widths = 65
        )
      }
    }
  }
  
  add_sheet_chunked = function(
    prefixo, dados, chunk_size = max_linhas_aba) {
    
    if (is.null(dados) || nrow(dados) == 0L) {
      add_sheet(prefixo, dados, filtro = FALSE)
      return(invisible(NULL))
    }
    
    if (nrow(dados) <= chunk_size) {
      add_sheet(prefixo, dados)
      return(invisible(NULL))
    }
    
    partes = split(
      seq_len(nrow(dados)),
      ceiling(seq_len(nrow(dados)) / chunk_size)
    )
    
    for (j in seq_along(partes)) {
      
      nome = sprintf("%s_%03d", prefixo, j)
      
      if (nchar(nome) > 31L) {
        nome = substr(nome, 1L, 31L)
      }
      
      add_sheet(
        nome,
        dados[partes[[j]], , drop = FALSE]
      )
    }
    
    invisible(NULL)
  }
  
  # Abas individuais dos nove cenarios, em maiusculo.
  cenarios_esperados = unique(as.character(grid_dados$Cenario))
  
  for (cen in cenarios_esperados) {
    
    z = if (nrow(df_resultados)) {
      df_resultados[
        df_resultados$Cenario == cen,
        ,
        drop = FALSE
      ]
    } else {
      data.frame()
    }
    
    if (nrow(z)) {
      
      ord_met = match(
        z$Metodo,
        c("KMeans", "GMM", "Hierarquico_Ward", "Processo_Dirichlet")
      )
      
      z = z[
        order(
          ord_met,
          z$n,
          z$p,
          z$Replica,
          z$K_testado,
          z$Prior_alpha,
          z$Config_G0
        ),
        ,
        drop = FALSE
      ]
      
      rownames(z) = NULL
    }
    
    colunas_remover_cenario = c(
      "K_testado",
      "K_selecionado_por_Silhouette",
      "Status",
      "Erro",
      "K_fornecido_ao_metodo",
      "K_Fornecido"
    )
    z_planilha = z[
      , setdiff(names(z), colunas_remover_cenario),
      drop = FALSE
    ]
    
    # Nome da planilha exatamente como A1, A2, ..., C3.
    add_sheet(cen, z_planilha)
  }
  
  # Abas gerais.
  add_sheet("Resumo_Medias", df_resumo)
  add_sheet("Config_Execucao", config_execucao)
  
  # Nao criar a aba Leia-me: seu conteudo esta em Config_Execucao.
  
  if (isTRUE(salvar_dados)) {
    add_sheet_chunked("Dados_Gerados", df_dados)
  }
  
  diretorio_saida = dirname(arquivo)
  
  if (!dir.exists(diretorio_saida)) {
    dir.create(
      diretorio_saida,
      recursive = TRUE,
      showWarnings = FALSE
    )
  }
  
  openxlsx::saveWorkbook(
    wb,
    file = arquivo,
    overwrite = TRUE
  )
  
  if (verbose) {
    message(
      "\nConcluido. Arquivo salvo em: ",
      normalizePath(arquivo, mustWork = FALSE)
    )
  }
  
  invisible(list(
    arquivo = normalizePath(arquivo, mustWork = FALSE),
    Resultados = df_resultados,
    Resumo = df_resumo,
    Dados_Gerados = if (isTRUE(salvar_dados)) df_dados else NULL
  ))
}

#Simulacao
hiper_G0 = configuracoes_G0(sensibilidade = TRUE)

saida = executar_simulacoes_dpmm(
  n_replicas = 20,
  n_values = c(500),
  p_values = c(5),
  K_values = c(2, 5, 10),
  separacoes = c(baixa = 1, media = 2, muito_alta = 4),
  padrao_medias = "todas_variaveis",
  usar_cholesky = TRUE,      
  dahl_restringir_K_moda = FALSE,
  alpha_priors = configuracoes_alpha(),
  g0_configs = hiper_G0,
  dpmm_n_samples = 2000,
  dpmm_burnin = 1000,
  dpmm_thin = 2,
  dpmm_coclustering_samples = 500,
  dpmm_alpha_init = 1,
  dpmm_K_init = 3,
  arquivo = "resultados_simulacoes_dpmm_novo_desenho.xlsx",
  confirmar_execucao_grande = TRUE,
  verbose = TRUE)