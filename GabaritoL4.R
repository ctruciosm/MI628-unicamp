library(causaldata)
library(dplyr)
library(car)
library(tidyr)

# 1
data(thornton_hiv)
glimpse(thornton_hiv)

dados = thornton_hiv %>% tidyr::drop_na()
glimpse(dados)

modelo <- lm(got ~ any, data = dados)
summary(modelo)
hccm(modelo, type = "hc2")

tau <- mean(dados$got[dados$any == 1]) - mean(dados$got[dados$any == 0])
vhat <- var(dados$got[dados$any == 1])/sum(dados$any) + var(dados$got[dados$any == 0]) / (length(dados$any)- sum(dados$any))


neyman_sre <- function(y, z, x) {
  x_levels <- unique(x)
  K <- length(x_levels)
  pi_k <- rep(0, K)
  tau_k <- rep(0, K)
  v_k <- rep(0, K)
  n <- length(z)
  for (k in 1:K) {
    x_k <- x_levels[k]
    z_k <- z[x == x_k]
    y_k <- y[x == x_k]
    pi_k[k] <- length(z_k)/n
    tau_k[k] <- mean(y_k[z_k == 1]) - mean(y_k[z_k == 0])
    v_k[k] <- var(y_k[z_k == 1]) / sum(z_k) + var(y_k[z_k == 0]) / sum(1 - z_k)
  }
  tau_s <- sum(pi_k * tau_k)
  v_s <- sum(pi_k^2 * v_k) 
  return(c(tau_s, v_s))
}

y <- dados$got
z <- dados$any
x <- dados$villnum
neyman_sre(y, z, x)

# Ajuste por covariaveis
## Lin

modelo <- lm(got ~ any*(distvct + age), data = dados)
summary(modelo)
sqrt(diag(hccm(modelo)))
tabela <- data.frame(Est = coef(modelo), Std = sqrt(diag(hccm(modelo))))
tabela |> mutate(t = Est / Std) |> 
  mutate(p_valor = 2*(1 - pnorm(abs(t)))) |> 
  round(4)

modelo <- lm(got ~ any*(distvct + age + ), data = dados)
summary(modelo)
sqrt(diag(hccm(modelo)))
tabela <- data.frame(Est = coef(modelo), Std = sqrt(diag(hccm(modelo))))
tabela |> mutate(t = Est / Std) |> 
  mutate(p_valor = 2*(1 - pnorm(abs(t)))) |> 
  round(4)

# 2
dados <- read.csv("/Users/ctruciosm/Dropbox/Teaching/MI628/MI628-unicamp/datasets/AL2009.csv")
glimpse(dados)
dados_tratados <- dados |> 
  mutate(pr02 = ifelse(is.na(pr02), mean(pr02, na.rm = TRUE), pr02)) |> 
  mutate(Y = 0.5*pr01 + 0.5*pr02, .keep = "unused") |> 
  pivot_wider(names_from = z, values_from = c("pr99", "pr00", "Y")) |> 
  mutate(diff_Y = Y_1 - Y_0, 
         diff_pr99 = pr99_1 - pr99_0,
         diff_pr00 = pr00_1 - pr00_0)

modelo <- lm(diff_Y ~ diff_pr99 + diff_pr00, data = dados_tratados)
summary(modelo)


