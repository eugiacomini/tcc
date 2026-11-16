dpmm_expand_param = function(x, p, name) {
  x = as.numeric(x)
  if (length(x) == 1L) return(rep(x, p))
  if (length(x) != p) {
    stop(sprintf("'%s' deve ser escalar ou ter comprimento ncol(X).", name))
  }
  x
}

dpmm_posterior_stats = function(nk, sum_x, sum_x2, prior) {
  # Para um grupo vazio, a posterior coincide com a priori G0.
  if (nk == 0L) {
    return(list(mu = prior$mu0, lambda = prior$lambda0,
                alfa = prior$alfa0, beta = prior$beta0))
  }
  
  ymean = sum_x / nk
  lambda_n = prior$lambda0 + nk
  mu_n = (prior$lambda0 * prior$mu0 + sum_x) / lambda_n
  alfa_n = prior$alfa0 + nk / 2
  beta_n = prior$beta0 + 0.5 * (
    sum_x2 + prior$lambda0 * prior$mu0^2 - lambda_n * mu_n^2
  )
  beta_n = pmax(beta_n, .Machine$double.eps)
  
  list(mu = mu_n, lambda = lambda_n, alfa = alfa_n, beta = beta_n)
}

dpmm_log_predictive = function(x, mu, lambda, alfa, beta) {
  # Predicao Student-t independente por dimensao.
  df = 2 * alfa
  scale = sqrt(beta * (lambda + 1) / (alfa * lambda))
  sum(stats::dt((x - mu) / scale, df = df, log = TRUE) - log(scale))
}

dpmm_group_probabilities = function(x, nk, sum_x, sum_x2, alpha, prior) {
  K = length(nk)
  log_prob = rep(-Inf, K + 1L)
  
  if (K > 0L) {
    for (k in seq_len(K)) {
      post = dpmm_posterior_stats(nk[k], sum_x[k, ], sum_x2[k, ], prior)
      log_prob[k] = log(nk[k]) + dpmm_log_predictive(
        x, post$mu, post$lambda, post$alfa, post$beta
      )
    }
  }
  
  post0 = dpmm_posterior_stats(0L, rep(0, length(x)), rep(0, length(x)), prior)
  log_prob[K + 1L] = log(alpha) + dpmm_log_predictive(
    x, post0$mu, post0$lambda, post0$alfa, post0$beta
  )
  
  max_log = max(log_prob)
  if (!is.finite(max_log)) {
    # Salvaguarda numerica: volta as probabilidades CRP se as densidades falharem.
    w = c(nk, alpha)
    return(w / sum(w))
  }
  weights = exp(log_prob - max_log)
  weights[!is.finite(weights)] = 0
  total = sum(weights)
  if (!is.finite(total) || total <= 0) {
    w = c(nk, alpha)
    return(w / sum(w))
  }
  weights / total
}

dpmm_update_alpha = function(alpha, K, n, a_alpha, b_alpha) {
  eta = stats::rbeta(1, shape1 = alpha + 1, shape2 = n)
  rate = b_alpha - log(eta)
  pi_eta = (a_alpha + K - 1) /
    (n * rate + a_alpha + K - 1)
  pi_eta = min(max(pi_eta, 0), 1)
  s = stats::rbinom(1, size = 1, prob = pi_eta)
  # Escobar--West: s=1 seleciona a componente com forma a_alpha+K.
  stats::rgamma(1, shape = a_alpha + K - 1 + s, rate = rate)
}

clustering_dpmm = function(
    X,
    prioris = list(),
    hiperparametros = list(),
    n_final_samples = 1000,
    burnin = 1000,
    saltos = 5,
    seed = NULL,
    verbose = TRUE,
    calcular_coclustering = TRUE,
    n_coclustering_samples = 500,
    alpha_init = 1,
    K_init = 3) {
  
  tempo_total_inicio = Sys.time()
  if (!is.null(seed)) set.seed(seed)
  if (is.data.frame(X)) X = data.matrix(X) else X = as.matrix(X)
  storage.mode(X) = "double"
  if (nrow(X) < 1L || ncol(X) < 1L) stop("X deve ter ao menos uma linha e uma coluna.")
  if (any(!is.finite(X))) stop("X nao pode conter NA, NaN ou valores infinitos.")
  
  n = nrow(X)
  p = ncol(X)
  prior_raw = utils::modifyList(
    list(mu0 = 0, lambda0 = 1, alfa0 = 2, beta0 = 1), prioris
  )
  prior = list(
    mu0 = dpmm_expand_param(prior_raw$mu0, p, "mu0"),
    lambda0 = dpmm_expand_param(prior_raw$lambda0, p, "lambda0"),
    alfa0 = dpmm_expand_param(prior_raw$alfa0, p, "alfa0"),
    beta0 = dpmm_expand_param(prior_raw$beta0, p, "beta0")
  )
  if (any(!is.finite(unlist(prior))) || any(prior$lambda0 <= 0) ||
      any(prior$alfa0 <= 0) || any(prior$beta0 <= 0)) {
    stop("mu0 deve ser finito; lambda0, alfa0 e beta0 devem ser positivos e finitos.")
  }
  
  hiper = utils::modifyList(list(a_alpha = 1, b_alpha = 1), hiperparametros)
  if (length(hiper$a_alpha) != 1L || length(hiper$b_alpha) != 1L ||
      !is.finite(hiper$a_alpha) || !is.finite(hiper$b_alpha) ||
      hiper$a_alpha <= 0 || hiper$b_alpha <= 0) {
    stop("a_alpha e b_alpha devem ser escalares positivos e finitos.")
  }
  if (length(alpha_init) != 1L || !is.finite(alpha_init) || alpha_init <= 0) {
    stop("alpha_init deve ser positivo e finito.")
  }
  if (length(K_init) != 1L || !is.finite(K_init) || K_init != as.integer(K_init) ||
      K_init < 1L || K_init > n) {
    stop("K_init deve ser inteiro entre 1 e nrow(X).")
  }
  if (length(n_final_samples) != 1L || n_final_samples < 1L ||
      n_final_samples != as.integer(n_final_samples) ||
      length(burnin) != 1L || burnin < 0L || burnin != as.integer(burnin) ||
      length(saltos) != 1L || saltos < 1L || saltos != as.integer(saltos)) {
    stop("Use n_final_samples >= 1, burnin >= 0 e saltos >= 1, todos inteiros.")
  }
  if (length(n_coclustering_samples) != 1L ||
      !is.finite(n_coclustering_samples) ||
      n_coclustering_samples < 1L ||
      n_coclustering_samples != as.integer(n_coclustering_samples)) {
    stop("n_coclustering_samples deve ser um inteiro positivo.")
  }
  
  # Inicializacao com K_init grupos, evitando comecar necessariamente com K=n.
  K_init = as.integer(K_init)
  groups = sample(rep(seq_len(K_init), length.out = n))
  K = K_init
  nk = tabulate(groups, nbins = K)
  sum_x = matrix(0, nrow = K, ncol = p)
  sum_x2 = matrix(0, nrow = K, ncol = p)
  for (k in seq_len(K)) {
    idx = which(groups == k)
    sum_x[k, ] = colSums(X[idx, , drop = FALSE])
    sum_x2[k, ] = colSums(X[idx, , drop = FALSE]^2)
  }
  
  n_iter = as.integer(burnin + n_final_samples * saltos)
  groups_samples = matrix(NA_integer_, nrow = n_final_samples, ncol = n)
  alpha_samples = numeric(n_final_samples)
  K_samples = integer(n_final_samples)
  saved = 0L
  alpha = as.numeric(alpha_init)
  tempo_mcmc_inicio = Sys.time()
  progress_step = max(1L, floor(n_iter / 20L))
  
  for (iter in seq_len(n_iter)) {
    for (i in sample.int(n)) {
      current = groups[i]
      nk[current] = nk[current] - 1L
      sum_x[current, ] = sum_x[current, ] - X[i, ]
      sum_x2[current, ] = sum_x2[current, ] - X[i, ]^2
      
      if (nk[current] == 0L) {
        if (current < K) groups[groups > current] = groups[groups > current] - 1L
        keep = setdiff(seq_len(K), current)
        nk = nk[keep]
        sum_x = sum_x[keep, , drop = FALSE]
        sum_x2 = sum_x2[keep, , drop = FALSE]
        K = K - 1L
      }
      
      probs = dpmm_group_probabilities(X[i, ], nk, sum_x, sum_x2, alpha, prior)
      chosen = sample.int(K + 1L, size = 1L, prob = probs)
      
      if (chosen <= K) {
        groups[i] = chosen
        nk[chosen] = nk[chosen] + 1L
        sum_x[chosen, ] = sum_x[chosen, ] + X[i, ]
        sum_x2[chosen, ] = sum_x2[chosen, ] + X[i, ]^2
      } else {
        K = K + 1L
        groups[i] = K
        nk = c(nk, 1L)
        sum_x = rbind(sum_x, X[i, ])
        sum_x2 = rbind(sum_x2, X[i, ]^2)
      }
    }
    
    alpha = dpmm_update_alpha(alpha, K, n, hiper$a_alpha, hiper$b_alpha)
    
    if (iter > burnin && (iter - burnin) %% saltos == 0L) {
      saved = saved + 1L
      groups_samples[saved, ] = groups
      alpha_samples[saved] = alpha
      K_samples[saved] = K
    }
    if (verbose && (iter %% progress_step == 0L || iter == n_iter)) {
      cat(sprintf("Iteracao %d/%d | grupos: %d | alpha: %.4f\n", iter, n_iter, K, alpha))
    }
  }
  tempo_mcmc = as.numeric(difftime(Sys.time(), tempo_mcmc_inicio, units = "secs"))
  
  coclustering = NULL
  tempo_coclustering = NA_real_
  n_coclustering_usadas = 0L
  indices_coclustering = integer(0)
  if (isTRUE(calcular_coclustering)) {
    inicio_coclustering = Sys.time()
    
    n_coclustering_usadas = min(
      as.integer(n_final_samples),
      as.integer(n_coclustering_samples)
    )
    indices_coclustering = if (n_coclustering_usadas == n_final_samples) {
      seq_len(n_final_samples)
    } else {
      sort(sample.int(n_final_samples, n_coclustering_usadas, replace = FALSE))
    }
    
    coclustering = matrix(0, nrow = n, ncol = n)
    for (s in indices_coclustering) {
      z_s = groups_samples[s, ]
      indices_grupos = split(seq_len(n), z_s)
      for (idx_grupo in indices_grupos) {
        coclustering[idx_grupo, idx_grupo] =
          coclustering[idx_grupo, idx_grupo, drop = FALSE] + 1
      }
    }
    coclustering = coclustering / n_coclustering_usadas
    diag(coclustering) = 1
    tempo_coclustering = as.numeric(difftime(Sys.time(), inicio_coclustering, units = "secs"))
  }
  tempo_total = as.numeric(difftime(Sys.time(), tempo_total_inicio, units = "secs"))
  
  structure(list(
    prioris = prior,
    hiperparametros = hiper,
    groups_samples = groups_samples,
    alpha_samples = alpha_samples,
    K_samples = K_samples,
    coclustering = coclustering,
    n_coclustering_samples = n_coclustering_usadas,
    indices_coclustering = indices_coclustering,
    tempos = list(mcmc = tempo_mcmc, coclustering = tempo_coclustering, total = tempo_total),
    call = match.call()
  ), class = "clustering_dpmm")
}

configuracoes_G0 = function(sensibilidade = FALSE, mu0 = 0) {
  # G0: Normal-Inverse-Gamma independente por dimensao.
  # alfa0/beta0 parametrizam a priori Inverse-Gamma de sigma^2.
  #
  # Quando sensibilidade=TRUE, sao comparadas quatro estrategias que
  # correspondem aos testes de priori ja realizados:
  # 1) original; 2) apenas centro empirico; 3) apenas beta0 pela variancia
  # marginal dos dados; 4) ambas as adaptacoes. As variantes empiricas sao
  # plug-in/empirical-Bayes e devem ser identificadas como tal nos resultados.
  if (!isTRUE(sensibilidade)) {
    return(data.frame(
      Config_G0 = "G0-1_original",
      mu0 = mu0,
      mu0_tipo = "valor",
      lambda0 = 1,
      alfa0 = 2,
      beta0 = 1,
      beta0_tipo = "valor",
      stringsAsFactors = FALSE
    ))
  }
  
  data.frame(
    Config_G0 = c(
      "G0-1_original",
      "G0-2_mu0_media_X",
      "G0-3_beta0_variancia_X",
      "G0-4_combinado_media_e_variancia"
    ),
    mu0 = c(mu0, 0, mu0, 0),
    mu0_tipo = c("valor", "media_X", "valor", "media_X"),
    lambda0 = rep(1, 4),
    alfa0 = rep(2, 4),
    beta0 = rep(1, 4),
    beta0_tipo = c("valor", "valor", "variancia_X", "variancia_X"),
    stringsAsFactors = FALSE
  )
}

configuracoes_alpha = function() {
  data.frame(
    Prior_alpha = c("Gamma(1,1)", "Gamma(2,4)", "Gamma(1,0.1)"),
    a_alpha = c(1, 2, 1),
    b_alpha = c(1, 4, 0.1),
    stringsAsFactors = FALSE
  )
}

calcular_NMI = function(grupos_estimados, grupos_reais) {
  tab = table(as.character(grupos_estimados), as.character(grupos_reais))
  if (sum(tab) == 0) return(NA_real_)
  
  pij = tab / sum(tab)
  pi = rowSums(pij)
  pj = colSums(pij)
  esperado = outer(pi, pj)
  pos = pij > 0
  mi = sum(pij[pos] * log(pij[pos] / esperado[pos]))
  h_estimado = -sum(pi[pi > 0] * log(pi[pi > 0]))
  h_real = -sum(pj[pj > 0] * log(pj[pj > 0]))
  denom = h_estimado + h_real
  
  if (denom <= .Machine$double.eps) return(1)
  max(0, min(1, 2 * mi / denom))
}

dpmm_particao_dahl = function(groups_samples, C, indices_draws = NULL,
                              K_restrito = NULL) {
  # Metodo de Dahl (2006): seleciona uma particao MCMC cuja matriz binaria
  # de co-associacao esta mais proxima da matriz media de coclustering C.
  # A minimizacao e feita sobre os pares i < j; isso equivale a metade da
  # distancia de Frobenius quadratica entre as matrizes simetricas.
  #
  # IMPORTANTE: indices_draws deve identificar exatamente os draws usados
  # para calcular C. Assim, a particao escolhida e comparada com a mesma
  # amostra posterior que originou a matriz media.
  
  groups_samples = as.matrix(groups_samples)
  C = as.matrix(C)
  
  if (!is.matrix(groups_samples) || nrow(groups_samples) < 1L ||
      ncol(groups_samples) < 2L) {
    stop("groups_samples deve ser uma matriz draws x observacoes.")
  }
  if (nrow(C) != ncol(C) || nrow(C) != ncol(groups_samples)) {
    stop("C deve ser quadrada e ter uma linha/coluna por observacao.")
  }
  if (anyNA(groups_samples)) {
    stop("groups_samples contem valores ausentes.")
  }
  if (any(!is.finite(C))) {
    stop("C contem valores nao finitos.")
  }
  
  n_draws = nrow(groups_samples)
  n = ncol(groups_samples)
  
  if (is.null(indices_draws)) {
    indices_draws = seq_len(n_draws)
  }
  indices_draws = as.integer(indices_draws)
  
  if (length(indices_draws) < 1L || anyNA(indices_draws) ||
      any(indices_draws < 1L | indices_draws > n_draws) ||
      anyDuplicated(indices_draws)) {
    stop("indices_draws deve conter indices unicos e validos de groups_samples.")
  }
  
  # Corrigir apenas desvios numericos; preservar dimensoes explicitamente.
  C = (C + t(C)) / 2
  C[] = pmax(0, pmin(1, C))
  diag(C) = 1
  
  upper = upper.tri(C)
  C_upper = C[upper]
  
  # Parte constante da perda de Dahl: soma C_ij^2 para todos os pares.
  constante = sum(C_upper^2)
  
  perdas = numeric(length(indices_draws))
  K_draw = integer(length(indices_draws))
  
  for (j in seq_along(indices_draws)) {
    z = groups_samples[indices_draws[j], ]
    grupos = split(seq_len(n), as.character(z))
    K_draw[j] = length(grupos)
    
    # Para cada draw:
    # L(z) = sum_{i<j} C_ij^2 +
    #        sum_{grupos g} [choose(n_g,2) - 2*sum_{i<j em g} C_ij].
    # Essa forma evita construir uma matriz n x n para cada draw.
    contribuicao_dentro = 0
    
    for (idx in grupos) {
      ng = length(idx)
      if (ng < 2L) next
      
      bloco = C[idx, idx, drop = FALSE]
      soma_C_dentro = sum(bloco[upper.tri(bloco)])
      contribuicao_dentro = contribuicao_dentro +
        (ng * (ng - 1) / 2) - 2 * soma_C_dentro
    }
    
    perdas[j] = constante + contribuicao_dentro
  }
  
  # Dahl classico: minimiza a perda sobre TODAS as particoes candidatas.
  melhor_livre = which.min(perdas)
  
  # Opcao de resumo: restringir a escolha ao K modal a posteriori.
  # A MATRIZ C NAO e alterada nem e utilizado o K verdadeiro.
  # Esta nao e a estimativa original de Dahl, e sim Dahl com K restrito.
  K_restrito_efetivo = if (is.null(K_restrito)) NA_integer_ else as.integer(K_restrito)
  if (is.null(K_restrito)) {
    elegiveis = seq_along(perdas)
  } else {
    if (length(K_restrito) != 1L || is.na(K_restrito) ||
        K_restrito < 1L || K_restrito != as.integer(K_restrito)) {
      stop("K_restrito deve ser um inteiro positivo ou NULL.")
    }
    elegiveis = which(K_draw == as.integer(K_restrito))
    if (!length(elegiveis)) {
      warning("K modal ausente na subamostra de Dahl; usado Dahl livre.")
      elegiveis = seq_along(perdas)
      K_restrito_efetivo = NA_integer_
    }
  }
  melhor = elegiveis[which.min(perdas[elegiveis])]
  draw_escolhido = indices_draws[melhor]
  clusters = as.integer(factor(groups_samples[draw_escolhido, ]))
  K_final = length(unique(clusters))
  
  # Silhueta calculada na dissimilaridade posterior D = 1-C, apenas descritiva.
  D = 1 - C
  diag(D) = 0
  sil_C = NA_real_
  if (K_final >= 2L && K_final < n && requireNamespace("cluster", quietly = TRUE)) {
    sil_C = tryCatch({
      sil = cluster::silhouette(clusters, stats::as.dist(D))
      mean(sil[, "sil_width"], na.rm = TRUE)
    }, error = function(e) NA_real_)
  }
  
  list(
    clusters = clusters,
    K = as.integer(K_final),
    K_dahl_livre = as.integer(K_draw[melhor_livre]),
    indice_dahl_livre = as.integer(indices_draws[melhor_livre]),
    K_restrito = K_restrito_efetivo,
    dahl_loss = unname(perdas[melhor]),
    dahl_draw_index = as.integer(draw_escolhido),
    dahl_candidate_position = as.integer(melhor),
    K_draw = as.integer(K_draw[melhor]),
    silhouette_coclustering = sil_C,
    dahl_losses = perdas,
    candidate_draw_indices = indices_draws,
    distance = D
  )
}