library(TMB)
library(Matrix)

setwd("D:/DRC/gaussian_process_AOC")

# ── 1. Clean recompile ────────────────────────────────────────────────────────
try(dyn.unload(dynlib("./tmb_try/leroux_gmrf")), silent = TRUE)
file.remove("./tmb_try/leroux_gmrf.o")
file.remove("./tmb_try/leroux_gmrf.dll")

compile("./tmb_try/leroux_gmrf.cpp")
dyn.load(dynlib("./tmb_try/leroux_gmrf"))

# ── 2. Load data ──────────────────────────────────────────────────────────────
data_mat_w <- readRDS("./data/inla_data/data_mat_w_mixed_time.RData")
data_mat_b <- readRDS("./data/inla_data/data_mat_b.RData")
data       <- readRDS("./data/inla_data/data_prepared_for_inla.RData")

# ── 3. Build W ────────────────────────────────────────────────────────────────
W <- as.matrix(data_mat_b)
W <- (W + t(W)) / 2          # enforce symmetry
diag(W) <- 0                 # no self-loops
W <- W / mean(W[W > 0])      # rescale: mean edge weight = 1
W_sp <- as(W, "dgCMatrix")
N    <- nrow(data)
stopifnot(nrow(W_sp) == N, ncol(W_sp) == N)
# No eigenvalues needed -- GMRF_t handles log-det internally via sparse Cholesky
cat("N:", N, "\n")
cat("Non-zeros in W:", nnzero(W_sp), "\n")
cat("Average neighbours:", nnzero(W_sp) / N, "\n")

# ── 4. Design matrix ──────────────────────────────────────────────────────────
X <- model.matrix(~ 1, data)   # intercept only; add covariates here
stopifnot(nrow(X) == N)

# ── 5. Handle NAs ────────────────────────────────────────────────────────────
obs_idx  <- which(!is.na(data$controle_binom)) - 1L   # 0-based for C++
y_filled <- data$controle_binom
y_filled[is.na(y_filled)] <- 0    # placeholder, excluded from likelihood

cat("Observed:", length(obs_idx),
    "| Missing:", sum(is.na(data$controle_binom)),
    "| Total:", N, "\n")

# ── 6. Dimension checks ───────────────────────────────────────────────────────
stopifnot(length(y_filled) == N, nrow(X) == N,
          nrow(W_sp) == N, ncol(W_sp) == N,
          all(is.finite(W_sp@x)))
cat("All checks passed.\n")

# ── 7. Data + parameters ──────────────────────────────────────────────────────
# Note: no eig_DmW needed anymore -- handled by GMRF_t in cpp
data_lst <- list(
  y       = y_filled,
  n       = rep(1, N),
  X       = X,
  W       = W_sp,
  obs_idx = as.integer(obs_idx)
)

p_obs <- mean(data$controle_binom, na.rm = TRUE)
parameters <- list(
  beta      = qlogis(p_obs),   # warm start at empirical mean
  phi       = rep(0, N),
  log_tau   = log(0.5),
  logit_rho = qlogis(0.7)
)

# ── 8. Build AD objective ─────────────────────────────────────────────────────
obj <- MakeADFun(
  data          = data_lst,
  parameters    = parameters,
  random        = "phi",
  DLL           = "leroux_gmrf",
  silent        = FALSE,
  inner.control = list(sparse = TRUE, maxit = 20, tol10 = 1e-6)
)

# Benchmark
#cat("\nTiming single evaluation:\n")
#print(system.time(obj$fn(obj$par)))
#print(system.time(obj$gr(obj$par)))

# ── 9. Two-stage optimisation ─────────────────────────────────────────────────
cat("\n--- Stage 1 ---\n")
fit1 <- nlminb(
  start     = obj$par,
  objective = obj$fn,
  gradient  = obj$gr,
  lower     = c(rep(-Inf, ncol(X)), -Inf, -3),
  upper     = c(rep( Inf, ncol(X)),  Inf,  5),
  control   = list(iter.max = 500, rel.tol = 1e-8)
)

cat("Stage 1 mgc:", max(abs(obj$gr(fit1$par))), "\n")

cat("\n--- Stage 2 ---\n")
fit2 <- nlminb(
  start     = fit1$par,
  objective = obj$fn,
  gradient  = obj$gr,
  lower     = c(rep(-Inf, ncol(X)), -Inf, -6),
  upper     = c(rep( Inf, ncol(X)),  Inf,  6),
  control   = list(iter.max = 1000, rel.tol = 1e-12, x.tol = 1e-12)
)
cat("Stage 2 mgc:", max(abs(obj$gr(fit2$par))), "\n")
cat("Convergence:", fit2$convergence, "| Message:", fit2$message, "\n")
fit <- fit2

# ── 10. Uncertainty ───────────────────────────────────────────────────────────
rep <- sdreport(obj)

# ── 11. Fixed effects ─────────────────────────────────────────────────────────
cat("\n=== Fixed effects (logit scale) ===\n")
fixed <- summary(rep, "fixed")
print(fixed)

beta_est <- fixed[rownames(fixed) == "beta",      "Estimate"]
beta_se  <- fixed[rownames(fixed) == "beta",      "Std. Error"]
lt_est   <- fixed[rownames(fixed) == "log_tau",   "Estimate"]
lr_est   <- fixed[rownames(fixed) == "logit_rho", "Estimate"]

cat("\nbeta:\n")
cat("  Probability:  ", round(plogis(beta_est), 4), "\n")
cat("  95% CI:       ",
    round(plogis(beta_est - 1.96*beta_se), 3), "to",
    round(plogis(beta_est + 1.96*beta_se), 3), "\n")
cat("tau:", round(exp(lt_est), 4), "\n")
cat("rho:", round(plogis(lr_est), 4), "\n")

# ── 12. Marginal probabilities (INLA-style, pulled toward 0.5) ────────────────
# eta point estimates -- fast, no Jacobian
eta_mean <- obj$report()$eta

# phi_c SEs from sdreport -- cheap because phi is random effect
phi_rep <- summary(rep, "random")
phi_rep <- phi_rep[rownames(phi_rep) == "phi_c", ]
phi_sd  <- phi_rep[, "Std. Error"]

# Total eta SD: phi uncertainty + beta uncertainty
eta_sd <- sqrt(phi_sd^2 + beta_se^2)

# Probit approximation: E[plogis(eta)] ~ pnorm(eta / sqrt(1 + c^2*var(eta)))
# This is exactly what INLA reports -- shrinks predictions toward 0.5
c2         <- pi^2 / 3
p_marginal <- pnorm(eta_mean / sqrt(1 + c2 * eta_sd^2))
p_plugin   <- plogis(eta_mean)   # plug-in for comparison only

# 95% credible intervals
p_lo <- pnorm((eta_mean - 1.96*eta_sd) / sqrt(1 + c2 * eta_sd^2))
p_hi <- pnorm((eta_mean + 1.96*eta_sd) / sqrt(1 + c2 * eta_sd^2))

cat("\n=== Predicted probabilities ===\n")
cat("Plugin p range:   ", round(range(p_plugin),   3), "\n")
cat("Marginal p range: ", round(range(p_marginal), 3), "\n")
cat("Observed proportion:", round(p_obs, 3), "\n")

# ── 13. Attach predictions to data ────────────────────────────────────────────
data$p_plugin   <- p_plugin      # plug-in (not recommended for reporting)
data$p_marginal <- p_marginal    # INLA-style (recommended)
data$p_lo       <- p_lo          # 95% CI lower
data$p_hi       <- p_hi          # 95% CI upper
data$phi_mean   <- phi_rep[, "Estimate"]
data$phi_se     <- phi_sd
data$observed   <- !is.na(data$controle_binom)

cat("\nSample predictions (first 6 rows):\n")
print(head(data[, c("controle_binom", "p_marginal", "p_lo", "p_hi",
                    "phi_mean", "observed")]))

# ── 14. Model fit ─────────────────────────────────────────────────────────────
cat("\n=== Model fit ===\n")
cat("Marginal nll:", fit$objective, "\n")
cat("AIC (approx):", 2 * fit$objective + 2 * length(fit$par), "\n")

# ── 15. Fake data test ────────────────────────────────────────────────────────
run_fake_test <- function() {
  N_f  <- 5
  W_f  <- matrix(c(0,1,0,0,1, 1,0,1,0,0, 0,1,0,1,0,
                   0,0,1,0,1, 1,0,0,1,0), N_f, N_f)
  W_f_sp <- as(W_f, "dgCMatrix")
  y_f    <- c(1, 0, NA, 1, 0)
  obs_f  <- which(!is.na(y_f)) - 1L
  y_fill <- ifelse(is.na(y_f), 0, y_f)
  dl <- list(y=y_fill, n=rep(1,N_f), X=matrix(1,N_f,1),
             W=W_f_sp, obs_idx=as.integer(obs_f))
  pl <- list(beta=0, phi=rep(0,N_f), log_tau=0, logit_rho=0)
  obj_t <- MakeADFun(dl, pl, random="phi", DLL="leroux", silent=TRUE)
  cat("\nFake test nll:", obj_t$fn(obj_t$par), "\n")
  cat("Fake gradient finite:", all(is.finite(obj_t$gr(obj_t$par))), "\n")
}
run_fake_test()
