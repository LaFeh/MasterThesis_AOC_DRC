library(TMB)
library(Matrix)

setwd("D:/DRC/gaussian_process_AOC")

# ── 1. Clean recompile ────────────────────────────────────────────────────────
try(dyn.unload(dynlib("./tmb_try/leroux_with_priors_constraint_alldata_wthsumtozero")), silent = TRUE)
file.remove("./tmb_try/leroux_with_priors_constraint_alldata_wthsumtozero.o")
file.remove("./tmb_try/leroux_with_priors_constraint_alldata_wthsumtozero.dll")

compile("./tmb_try/leroux_with_priors_constraint_alldata_wthsumtozero.cpp")
dyn.load(dynlib("./tmb_try/leroux_with_priors_constraint_alldata_wthsumtozero"))

# ── 2. Load data ──────────────────────────────────────────────────────────────
data_mat_w <- readRDS("./data/inla_data/data_mat_w_mixed_time_decay.RData")
data       <- readRDS("./data/inla_data/data_prepared_for_inla.RData")

# ── 3. Build W over ALL areas (including NA areas) ───────────────────────────
W <- as.matrix(data_mat_w)
W <- (W + t(W)) / 2    # enforce symmetry
diag(W) <- 0           # no self-loops
W_sp <- as(W, "dgCMatrix")

N <- nrow(data)
stopifnot(nrow(W_sp) == N)
stopifnot(ncol(W_sp) == N)

# ── 4. Eigenvalues of Laplacian (D - W) ──────────────────────────────────────
D       <- Diagonal(x = rowSums(W))
L       <- D - W_sp

#eig_DmW <- eigen(as.matrix(L), symmetric = TRUE, only.values = TRUE)$values
load(file = "./tmb_try/eigvalues_data_mat_w_mixed_time_decay.RData")

cat("N areas:", N, "\n")
cat("Min eigenvalue of (D-W):", min(eig_DmW), "\n")
cat("Negative eigenvalues (> -1e-8 is fine):", sum(eig_DmW < -1e-8), "\n")

# ── 5. Design matrix (all N rows, no NA dropping) ────────────────────────────
X <- model.matrix(~ 1, data)   # intercept only

lower_bounds_beta <- rep(-Inf, ncol(X))
upper_bounds_beta <- rep(Inf,  ncol(X))

stopifnot(nrow(X) == N)

# ── 6. Handle NAs in response ────────────────────────────────────────────────
obs_idx  <- which(!is.na(data$control_binom)) - 1L   # 0-based for C++
y_filled <- data$control_binom
y_filled[is.na(y_filled)] <- 0

cat("Observed:", length(obs_idx), "/ Missing:", sum(is.na(data$control_binom)),
    "/ Total:", N, "\n")

# ── 7. Dimension checks ───────────────────────────────────────────────────────
stopifnot(length(y_filled)  == N)
stopifnot(length(eig_DmW)   == N)
stopifnot(nrow(X)           == N)
stopifnot(nrow(W_sp)        == N)
stopifnot(all(is.finite(W_sp@x)))
stopifnot(all(is.finite(eig_DmW)))
cat("All dimension checks passed.\n")

# ── 8. Data and parameter lists ───────────────────────────────────────────────
data_lst <- list(
  y       = y_filled,
  n       = rep(1, N),
  X       = X,
  W       = W_sp,
  eig_DmW = eig_DmW,
  obs_idx = as.integer(obs_idx),
  beta_prior_sd   = 2.5,
  tau_prior_shape = 0.5,
  tau_prior_scale = 2
)

parameters <- list(
  beta     = rep(0, ncol(X)),
  phi_free = rep(0, N - 1),   # N-1 free components; last phi recovered as -sum
  log_tau  = 0
)

# ── 9. Build AD objective ─────────────────────────────────────────────────────
#
#  random = "phi_free":  TMB integrates out the N-1 free spatial components via
#  the Laplace approximation.  The N-th component phi[N] = -sum(phi_free) is
#  computed deterministically inside the template, so the constraint sum(phi)=0
#  holds exactly at every evaluation — not just approximately at the optimum.
#
obj <- MakeADFun(
  data       = data_lst,
  parameters = parameters,
  random     = "phi_free",
  DLL        = "leroux_with_priors_constraint_alldata_wthsumtozero",
  silent     = FALSE
)

# Quick sanity check
cat("nll at start:", obj$fn(obj$par), "\n")
cat("Gradient finite:", all(is.finite(obj$gr(obj$par))), "\n")

# ── 10. Optimise ──────────────────────────────────────────────────────────────
fit <- nlminb(
  start     = obj$par,
  objective = obj$fn,
  gradient  = obj$gr,
  lower     = c(lower_bounds_beta, -6),
  upper     = c(upper_bounds_beta,  6),
  control   = list(iter.max = 5000, eval.max = 2000)
)
cat("Convergence:", fit$convergence, "\n")
cat("Message:", fit$message, "\n")
cat("Gradient at optimum:", obj$gr(fit$par), "\n")

# ── 11. Uncertainty ───────────────────────────────────────────────────────────
rep   <- sdreport(obj, par.fixed = fit$par)
fixed <- summary(rep, "fixed")    # beta, log_tau with SEs
print(fixed)

save(rep, file = "./tmb_try/leroux_with_priors_constraint_mat_w.RData")

# ── 12. Extract posterior mode predictions ────────────────────────────────────
obj$fn(fit$par)
full_par    <- obj$env$last.par.best
report_vals <- obj$report(full_par)

# Confirm the constraint: should be 0 (or machine-epsilon, e.g. ~1e-14)
cat("sum(phi) at mode:", report_vals$phi_sum, "\n")

data$p_mean   <- as.numeric(report_vals$p)
data$phi_mean <- as.numeric(plogis(report_vals$phi))
data$eta_mean <- as.numeric(report_vals$eta)

library(ggplot2)
p <- ggplot2::ggplot(data) +
  geom_sf(aes(fill = phi_mean)) +
  scale_fill_viridis_c() +
  theme_minimal()

p
