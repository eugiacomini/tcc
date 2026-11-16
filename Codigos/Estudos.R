# Função de distribuição empírica
F_empirica = function(data, xgrid) {
  sapply(xgrid, function(t) mean(data <= t))
}

set.seed(811819)

alpha = 5

# Amostras da distribuição verdadeira N(2, 2²)
g_n8   = rnorm(8,   mean = 2, sd = 2)
g_n50  = rnorm(50,  mean = 2, sd = 2)
g_n100 = rnorm(100, mean = 2, sd = 2)

# Grade de avaliação
x = seq(-4, 8, length.out = 500)

# Distribuição base G0 = N(0, 1)
G0 = pnorm(x, mean = 0, sd = 1)

# n = 8
Fn8 = F_empirica(g_n8, x)

post8 = (alpha/(alpha + 8)) * G0 +
  (8/(alpha + 8)) * Fn8

# n = 50
Fn50 = F_empirica(g_n50, x)

post50 = (alpha/(alpha + 50)) * G0 +
  (50/(alpha + 50)) * Fn50

# n = 100
Fn100 = F_empirica(g_n100, x)

post100 = (alpha/(alpha + 100)) * G0 +
  (100/(alpha + 100)) * Fn100

# Distribuição verdadeira
truth = pnorm(x, mean = 2, sd = 2)

pdf(
  file = "estudo_simulacao_1.pdf",
  width = 487/72,
  height = 296/72,
  family = "Helvetica",
  paper = "special",
  onefile = FALSE
)

# Configurações gráficas
par(
  mfrow = c(1, 3),
  mar = c(4, 4, 5.5, 1)
)

# GRÁFICO 1: n = 8

plot(
  x, post8,
  type = "l",
  lwd = 2,
  col = "black",
  main = "",
  ylab = "F(x)",
  xlab = "x",
  ylim = c(0, 1)
)

mtext(
  "n = 8",
  side = 3,
  line = 2.3,
  cex = 1.2
)

lines(x, G0, lty = 2)
lines(x, Fn8, lty = 3)

lines(
  x, truth,
  col = "royalblue",
  lty = 1,
  lwd = 1.5
)

legend(
  "bottomright",
  legend = c(
    "Posteriori E(F|y)",
    "G0",
    "Empírica",
    "Verdadeira"
  ),
  col = c(
    "black",
    "black",
    "black",
    "royalblue"
  ),
  lty = c(1, 2, 3, 1),
  lwd = c(2, 1, 1, 1.5),
  cex = 0.8,
  bty = "n"
)

# GRÁFICO 2: n = 50

plot(
  x, post50,
  type = "l",
  lwd = 2,
  col = "black",
  main = "",
  ylab = "F(x)",
  xlab = "x",
  ylim = c(0, 1)
)

mtext(
  "n = 50",
  side = 3,
  line = 2.3,
  cex = 1.2
)

lines(x, G0, lty = 2)
lines(x, Fn50, lty = 3)

lines(
  x, truth,
  col = "royalblue",
  lty = 1,
  lwd = 1.5
)

legend(
  "bottomright",
  legend = c(
    "Posteriori E(F|y)",
    "G0",
    "Empírica",
    "Verdadeira"
  ),
  col = c(
    "black",
    "black",
    "black",
    "royalblue"
  ),
  lty = c(1, 2, 3, 1),
  lwd = c(2, 1, 1, 1.5),
  cex = 0.8,
  bty = "n"
)

# GRÁFICO 3: n = 100

plot(
  x, post100,
  type = "l",
  lwd = 2,
  col = "black",
  main = "",
  ylab = "F(x)",
  xlab = "x",
  ylim = c(0, 1)
)

mtext(
  "n = 100",
  side = 3,
  line = 2.3,
  cex = 1.2
)

lines(x, G0, lty = 2)
lines(x, Fn100, lty = 3)

lines(
  x, truth,
  col = "royalblue",
  lty = 1,
  lwd = 1.5
)

legend(
  "bottomright",
  legend = c(
    "Posteriori E(F|y)",
    "G0",
    "Empírica",
    "Verdadeira"
  ),
  col = c(
    "black",
    "black",
    "black",
    "royalblue"
  ),
  lty = c(1, 2, 3, 1),
  lwd = c(2, 1, 1, 1.5),
  cex = 0.8,
  bty = "n"
)


dev.off()

set.seed(811819)

# Funcao para gerar amostras do processo de dirichlet via esquema da urna de polya

rDirProcessPolya = function(n, alpha, amostrador_G0) {
  
  x = numeric(n)
  
  x[1] = amostrador_G0(1)
  
  for(i in 2:n) {
    
    p_novo = alpha / (alpha + i - 1)
    
    if(runif(1) < p_novo) {
      x[i] = amostrador_G0(1)
    } else {
      x[i] = sample(x[1:(i - 1)], size = 1)
    }
  }
  
  x
}

alpha_values = c(0.2, 4, 20, 100)

n = 200
n_rep = 8

x_grid = seq(-4, 4, length.out = 500)

# G0 = N(0,1)
G0_cdf = pnorm(x_grid, mean = 0, sd = 1)

amostrador_G0 = function(n) {
  rnorm(n, mean = 0, sd = 1)
}

pdf(
  file = "estudo_simulacao_2.pdf",
  width = 486 / 72,
  height = 296 / 72,
  family = "Helvetica",
  paper = "special",
  onefile = FALSE
)

par(
  mfrow = c(2, 2),
  mar = c(3, 3, 0.5, 0.5),
  oma = c(0, 0, 0, 0),
  mgp = c(1.7, 0.5, 0)
)

for(alpha in alpha_values) {
  
  plot(
    x_grid,
    G0_cdf,
    type = "l",
    lwd = 3,
    col = "royalblue",
    ylim = c(0, 1),
    main = "",
    xlab = expression(theta),
    ylab = expression(F(theta))
  )
  
  for(rep in 1:n_rep) {
    
    amostra = rDirProcessPolya(
      n = n,
      alpha = alpha,
      amostrador_G0 = amostrador_G0
    )
    
    Fn = F_empirica(amostra, x_grid)
    
    lines(
      x_grid,
      Fn,
      col = adjustcolor("gray40", alpha.f = 0.35),
      lwd = 1
    )
  }
  
  legend(
    "topleft",
    inset = 0.01,
    legend = c(
      expression(G[0]),
      "Realizações"
    ),
    col = c("royalblue", "gray40"),
    lwd = c(3, 1),
    cex = 0.9,
    y.intersp = 0.9,
    x.intersp = 0.7,
    seg.len = 1.8,
    bty = "n"
  )
  
  text(
    x = max(x_grid),
    y = 0.05,
    labels = bquote(alpha == .(alpha)),
    adj = c(1, 0),
    cex = 1.1,
    font = 2
  )
}

dev.off()