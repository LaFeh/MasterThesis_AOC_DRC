library(TMB)
library(Matrix)
library(RSpectra)   # fast sparse eigenvalues -- install if needed

setwd("D:/DRC/gaussian_process_AOC")

# ── 1. Clean recompile ────────────────────────────────────────────────────────
try(dyn.unload(dynlib("./tmb_try/leroux")), silent = TRUE)
file.remove("./tmb_try/leroux_inla_mean_adjusted_faster.o")
file.remove("./tmb_try/leroux_inla_mean_adjusted_faster.dll")

compile("./tmb_try/leroux_inla_mean_adjusted_faster.cpp")
dyn.load(dynlib("./tmb_try/leroux_inla_mean_adjusted_faster"))

# ── 2. Load data ──────────────────────────────────────────────────────────────
data_mat_w <- readRDS("./data/inla_data/data_mat_w_mixed_time_decay.RData")
#data_mat_b <- readRDS("./data/inla_data/data_mat_b.RData")
data       <- readRDS("./data/inla_data/data_prepared_for_inla.RData")

# ── 3. Build W ────────────────────────────────────────────────────────────────
W <- as.matrix(data_mat_b)
W <- (W + t(W)) / 2
diag(W) <- 0
W <- W / mean(W[W > 0])    # rescale to mean edge weight = 1
W_sp <- as(W, "dgCMatrix")
N    <- nrow(data)
stopifnot(nrow(W_sp) == N, ncol(W_sp) == N)

# ── 4. Eigenvalues of Laplacian (D-W) -- computed ONCE, fast via RSpectra ────
# RSpectra works on sparse matrices directly -- no dense conversion, O(N*k)
D_sp    <- Diagonal(x = rowSums(W))
L_sp    <- D_sp - W_sp                         # sparse Laplacian

# Compute all N eigenvalues using RSpectra
# For N=3194 this is much faster than base::eigen on dense matrix
library(PRIMME)
eig_DmW <- eigen(as.matrix(L_sp), symmetric = TRUE, only.values = TRUE)$values # N-1 largest
eig_DmW <- sort(c(eig_DmW))                # append known zero, sort asc

cat("N:", N, "\n")
cat("Min eig(D-W):", min(eig_DmW), "\n")
cat("Negative eigenvalues:", sum(eig_DmW < -1e-8), "\n")

# ── 5. Design matrix ──────────────────────────────────────────────────────────
X <- model.matrix(~ 1, data)
stopifnot(nrow(X) == N)

# ── 6. Handle NAs ────────────────────────────────────────────────────────────
obs_idx  <- which(!is.na(data$controle_binom)) - 1L
y_filled <- data$controle_binom
y_filled[is.na(y_filled)] <- 0
cat("Observed:", length(obs_idx), "| Missing:", sum(is.na(data$controle_binom)), "\n")

# ── 7. Dimension checks ───────────────────────────────────────────────────────
stopifnot(length(y_filled) == N, length(eig_DmW) == N,
          nrow(X) == N, nrow(W_sp) == N,
          all(is.finite(W_sp@x)), all(is.finite(eig_DmW)))
cat("All checks passed.\n")

# ── 8. Data + parameters ──────────────────────────────────────────────────────
data_lst <- list(
  y       = y_filled,
  n       = rep(1, N),
  X       = X,
  W       = W_sp,
  eig_DmW = eig_DmW,
  obs_idx = as.integer(obs_idx)
)

p_obs <- mean(data$controle_binom, na.rm = TRUE)
parameters <- list(
  beta      = qlogis(p_obs),
  phi       = rep(0, N),
  log_tau   = log(0.5),
  logit_rho = qlogis(0.7)
)

# ── 9. Build objective ────────────────────────────────────────────────────────
obj <- MakeADFun(
  data          = data_lst,
  parameters    = parameters,
  random        = "phi",
  DLL           = "leroux_inla_mean_adjusted_faster",
  silent        = FALSE,
  inner.control = list(sparse = TRUE, maxit = 20, tol10 = 1e-6)
)

# Benchmark single evaluation
cat("Timing single eval:\n")
print(system.time(obj$fn(obj$par)))
print(system.time(obj$gr(obj$par)))

# ── 10. Two-stage optimisation ────────────────────────────────────────────────
fit1 <- nlminb(obj$par, obj$fn, obj$gr,
               lower = c(rep(-Inf, ncol(X)), -Inf, -6),
               upper = c(rep( Inf, ncol(X)),  Inf,  6),
               control = list(iter.max = 500, rel.tol = 1e-8))
cat("Stage 1 mgc:", max(abs(obj$gr(fit1$par))), "\n")

fit2 <- nlminb(fit1$par, obj$fn, obj$gr,
               lower = c(rep(-Inf, ncol(X)), -Inf, -6),
               upper = c(rep( Inf, ncol(X)),  Inf,  6),
               control = list(iter.max = 1000, rel.tol = 1e-12, x.tol = 1e-12))
cat("Stage 2 mgc:", max(abs(obj$gr(fit2$par))), "\n")
cat("Convergence:", fit2$convergence, "| Message:", fit2$message, "\n")
fit <- fit2

# ── 11. sdreport ──────────────────────────────────────────────────────────────
rep <- sdreport(obj)

# Fixed effects
fixed <- summary(rep, "fixed")
cat("\n=== Fixed effects ===\n"); print(fixed)

beta_est <- fixed[rownames(fixed) == "beta",      "Estimate"]
beta_se  <- fixed[rownames(fixed) == "beta",      "Std. Error"]
lt_est   <- fixed[rownames(fixed) == "log_tau",   "Estimate"]
lr_est   <- fixed[rownames(fixed) == "logit_rho", "Estimate"]

cat("\nbeta (probability scale):", round(plogis(beta_est), 4),
    "95% CI:", round(plogis(beta_est - 1.96*beta_se), 3),
    "to", round(plogis(beta_est + 1.96*beta_se), 3), "\n")
cat("tau:", round(exp(lt_est), 4), "\n")
cat("rho:", round(plogis(lr_est), 4), "\n")

# ── 12. Marginal probabilities (INLA-style) ───────────────────────────────────
eta_mean <- obj$report()$eta

phi_rep <- summary(rep, "random")
phi_rep <- phi_rep[rownames(phi_rep) == "phi_c", ]
phi_sd  <- phi_rep[, "Std. Error"]

# Total eta SD = phi uncertainty + beta uncertainty
eta_sd <- sqrt(phi_sd^2 + beta_se^2)

# Probit approximation: E[plogis(eta)] ~ pnorm(eta/sqrt(1 + c^2*var(eta)))
c2         <- pi^2 / 3
p_marginal <- pnorm(eta_mean / sqrt(1 + c2 * eta_sd^2))
p_plugin   <- plogis(eta_mean)
p_lo       <- pnorm((eta_mean - 1.96*eta_sd) / sqrt(1 + c2 * eta_sd^2))
p_hi       <- pnorm((eta_mean + 1.96*eta_sd) / sqrt(1 + c2 * eta_sd^2))

cat("\nPlugin p range:   ", round(range(p_plugin),   3), "\n")
cat("Marginal p range: ", round(range(p_marginal), 3), "\n")
cat("Observed proportion:", round(p_obs, 3), "\n")

# ── 13. Attach to data ────────────────────────────────────────────────────────
data$p_plugin   <- p_plugin
data$p_marginal <- p_marginal
data$p_lo       <- p_lo
data$p_hi       <- p_hi
data$phi_mean   <- phi_rep[, "Estimate"]
data$phi_se     <- phi_sd
data$observed   <- !is.na(data$controle_binom)

cat("\nSample predictions:\n")
print(head(data[, c("controle_binom", "p_marginal", "p_lo", "p_hi", "observed")]))

cat("\nNll:", fit$objective, "\n")
cat("AIC:", 2 * fit$objective + 2 * length(fit$par), "\n")


p <- ggplot2::ggplot(data) +
  geom_sf(aes(fill =  phi_mean)) +
  scale_fill_viridis_c() +
  theme_minimal()

p