# besagproper2 leroux comparison.
library(INLA)
library(ggplot2)
library(CARBayes)


setwd("D:/DRC/gaussian_process_AOC")
data = readRDS("./data/inla_data/data_prepared_for_inla.RData")

data_mat_b = readRDS("./data/inla_data/data_mat_b.RData")
data_mat_w = readRDS("./data/inla_data/data_mat_w_mixed_time.RData")


test_w = data_mat_w

test_w[1:1500,1:1500] = 0
test_w[2,1:1500] = 1
test_w[1:1500,2] = 1

# inla besagpropers

formula = controle_binom~1

id_besagproper2_matw = update(formula, . ~. +f(ID, model = "besagproper2", graph = data_mat_w))

data.model <- inla(
  id_besagproper2_matw,
  data = as.data.frame(data),
  control.fixed = list(
    mean.intercept = 0,
    prec.intercept = 1000
  ),
  family = "binomial",
  control.compute = list(
    dic = TRUE, waic = TRUE, cpo = TRUE,
    return.marginals.predictor = TRUE
  ),
  control.predictor = list(
    compute = TRUE,
    link = 1
  )
)

data$besagproper2 = data.model$summary.fitted.values$mean



leroux.model = S.CARleroux(as.formula(formula), data = data,
            family="binomial",
            trials =rep(1,nrow(data)), rho = NULL ,
            W = data_mat_w, burnin = 9000, n.sample = 1000000, thin = 10)


library(coda)
trace_beta0 <- mcmc(leroux.model$samples$beta)
plot(trace_beta0)
trace_tau2 <- mcmc(leroux.model$samples$tau2)
plot(trace_tau2)
trace_rho <- mcmc(leroux.model$samples$rho)
plot(trace_rho)
trace_phi <- mcmc(leroux.model$samples$phi)
plot(trace_phi)


print(leroux.model)            
plot(leroux.model$sa)            
k = leroux.model$samples
hist(k$Y[1,])

data$leroux = leroux.model$fitted.values


phi <- leroux.model$samples$phi     # iterations × areas
phi_centered <- t(apply(phi, 1, function(x) {
  x - mean(x)
}))

beta0 <- leroux.model$samples$beta
beta0_adj <- beta0 + rowMeans(phi)

D <- diag(rowSums(data_mat_b))
Q <- D - data_mat_b

library(MASS)
Qinv <- ginv(Q)

c <- exp(mean(log(diag(Qinv))))


eta <- sweep(phi_centered/c, 1, beta0_adj, "+")
p <- plogis(eta)

fitted_mean <- colMeans(p)
data$leroux_scaled = fitted_mean

plot(data[which(data$cell_id %in% c(6000:6400)),"controle_binom"])
plot(data[which(data$cell_id %in% c(6000:6400)),"leroux"])
#2390 
samples = leroux.model$samples#
plot(samples$phi[,2390])
mean(samples$phi[,2390])
samples$beta[2390,]
mean(samples$fitted[,2390])

plot(plogis(samples$phi[,2390] + samples$beta[2390,]))
plot(samples$fitted[,2390])

eta <- sweep(samples$phi[,2390], 1, samples$beta[2390,], "+")



plot(data[,c("lg_mix_time_mean")])
plot(data[1:1500,c("leroux_scaled")])
plot(data[2,c("leroux_scaled")])

test_w[1:1500,2] = 1



# calculate fitted values on my own:
X <- model.matrix(~ 1, data)

beta0 <- leroux.model$samples$beta0
beta  <- leroux.model$samples$beta
phi   <- leroux.model$samples$phi


X <- model.matrix(~ 1, data)

beta0 <- leroux.model$samples$beta0
beta  <- leroux.model$samples$beta
phi   <- leroux.model$samples$phi

M <- length(beta)
n <- ncol(phi)

fitted_mat <- matrix(NA, M, n)

for (m in 1:M) {
  eta_m <- as.numeric(X %*% beta[m, ]) + phi[m, ]
  fitted_mat[m, ] <- plogis(eta_m)
}

fitted_manual <- colMeans(fitted_mat)

# ---- Plot ----
p <- ggplot(data) +
  geom_sf(aes(fill =  leroux)) +
  scale_fill_viridis_c() +
  theme_minimal()
p

p <- ggplot(data) +
  geom_sf(aes(fill =  leroux_scaled)) +
  scale_fill_viridis_c() +
  theme_minimal()
p

p <- ggplot(data) +
  geom_sf(aes(fill =  besagproper2)) +
  scale_fill_viridis_c() +
  theme_minimal()
p
