### Lista 02 - Ex 4

n <- 20
n_1 <- 10
n_0 <- 10
M <- choose(n, n_1)
aux_z_perm <- combn(n, n_1)
R <- 10^3

exact_p_value <- c()
p_valor_aprox1 <- c()
p_valor_aprox2 <- c()

for (j in 1:1000) {
  print(j)
  set.seed(j)
  Y0 <- rnorm(n, 0)
  tau <- rnorm(n, 3, 0.5)
  Y1 <- Y0 + tau
  z <- sample(c(rep(1, n_1), rep(0, n_0)))
  Y <- z * Y1 + (1 - z) * Y0
  dif_mean <- mean(Y[z == 1]) - mean(Y[z == 0])

  dif_mean_perm <- c()
  z_perm <- matrix(0, n, ncol = M)
  for (i in 1:M) {
    z_perm_index <- aux_z_perm[, i]
    z_perm[z_perm_index, i] <- 1
    dif_mean_perm[i] <- mean(Y[z_perm[, i] == 1]) - mean(Y[z_perm[, i] == 0])
  }
  exact_p_value[j] <- mean(dif_mean_perm > dif_mean)

  dif_mean_perm_aprox <- c()
  for (i in 1:M) {
    set.seed(i)
    z_perm <- sample(z)
    dif_mean_perm_aprox[i] <- mean(Y[z_perm == 1]) - mean(Y[z_perm == 0])
  }
    
  
  p_valor_aprox1[j] <- mean(dif_mean_perm_aprox > dif_mean)
  p_valor_aprox2[j] <- (1 + sum(dif_mean_perm_aprox > dif_mean)) / (1 + R)
}


# R <- 10^5
> mean((exact_p_value-p_valor_aprox1)^2)
[1] 5.090995e-10
> mean((exact_p_value-p_valor_aprox2)^2)
[1] 1.027347e-07