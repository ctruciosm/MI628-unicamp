# Todos os Z sob MPE

library(HistData)
taus_i_obs <- ZeaMays$diff
tau_obs <- mean(taus_i_obs)
n_pairs <- nrow(ZeaMays)

z_mpe <- expand.grid(c(0, 1), c(0, 1))
for (i in 3:n_pairs) {
  z_mpe <- tidyr::expand_grid(z_mpe, c(0, 1), .name_repair = "minimal")
}
z_mpe <- as.matrix(z_mpe, ncol = n_pairs)
s_mpe <- 2 * z_mpe - 1
taus_ran <- rowMeans(s_mpe * abs(taus_i_obs))
mean(taus_ran >= tau_obs)

taus_ran_2 <- apply(s_mpe * abs(taus_i_obs),1, sum)/n_pairs
mean(taus_ran_2 >= tau_obs)

taus_ran_3 <- apply(zzz * abs(taus_i_obs), 1, sum)/n_pairs
mean(taus_ran_3 >= tau_obs)

difference <- ZeaMays$diff
abs_diff <- abs(difference)
n_pairs <- nrow(ZeaMays)
t_obs <- mean(difference)
t_ran <- sapply(1:2^n_pairs, function(x) mean(mp_enumerate(x, n_pairs)*abs_diff))
