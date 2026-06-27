library(TMB)
library(Matrix)

setwd("D:/DRC/gaussian_process_AOC")

# ── 1. Clean recompile ────────────────────────────────────────────────────────
try(dyn.unload(dynlib("./tmb_try/leroux_with_priors_wo_constraint_alldata")), silent = TRUE)
file.remove("./tmb_try/leroux_with_priors_wo_constraint_alldata.o")
file.remove("./tmb_try/leroux_with_priors_wo_constraint_alldata.dll")

compile("./tmb_try/leroux_with_priors_wo_constraint_alldata.cpp")
dyn.load(dynlib("./tmb_try/leroux_with_priors_wo_constraint_alldata"))

# ── 2. Load data ──────────────────────────────────────────────────────────────

data_mat_w <- readRDS("./data/inla_data/data_mat_w_mixed_time_decay.RData")
#data_mat_b <- readRDS("./data/inla_data/data_mat_b.RData")
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


eig_DmW <- eigen(as.matrix(L), symmetric = TRUE, only.values = TRUE)$values
#save(eig_DmW,file = "./tmb_try/eigvalues_data_mat_w_mixed_time_decay.RData")
#load(file = "./tmb_try/eigvalues_data_mat_w_mixed_time_decay.RData")

cat("N areas:", N, "\n")
cat("Min eigenvalue of (D-W):", min(eig_DmW), "\n")  # expect >= 0 (or tiny negative)
cat("Negative eigenvalues (> -1e-8 is fine):", sum(eig_DmW < -1e-8), "\n")

# ── 5. Design matrix (all N rows, no NA dropping) ────────────────────────────
# If you have covariates with NAs, either impute them or use only intercept here
X <- model.matrix(~ 1 , data)   # intercept only -- replace with your formula

lower_bounds_beta = rep(-Inf, ncol(X))
upper_bounds_beta = rep(Inf, ncol(X))

stopifnot(nrow(X) == N)

# ── 6. Handle NAs in response ────────────────────────────────────────────────
obs_idx  <- which(!is.na(data$control_binom)) - 1L   # 0-based for C++
y_filled <- data$control_binom
y_filled[is.na(y_filled)] <- 0   # placeholder; these rows excluded from likelihood

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
  beta_prior_sd        = 2.5,
  tau_prior_shape   = 0.5,
  tau_prior_scale     = 2,
  logit_rho_prior_mean = 10,
  logit_rho_prior_sd   = 1
)

parameters <- list(
  beta      = rep(0, ncol(X)),
  phi       = rep(0, N),
  log_tau   = 0,    # tau = 1
  logit_rho = 0.9     # rho = 0.5
)

# ── 9. Build AD objective ─────────────────────────────────────────────────────
obj <- MakeADFun(
  data       = data_lst,
  parameters = parameters,
  random     = "phi",       # phi integrated out by Laplace for ALL N areas
  DLL        = "leroux_with_priors_wo_constraint_alldata",
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
  lower     = c(lower_bounds_beta, -6, -6),  # bound logit_rho
  upper     = c(upper_bounds_beta,  6,  6),
  control   = list(iter.max = 5000, eval.max = 2000)
)
cat("Convergence:", fit$convergence, "\n")
cat("Message:", fit$message, "\n")
cat("Convergences:", obj$gr(fit$par), "\n")


# ── 11. Uncertainty ───────────────────────────────────────────────────────────
rep <- sdreport(obj, par.fixed = fit$par)
fixed = summary(rep, "fixed")    # beta, log_tau, logit_rho with SEs

save(rep,file = "./tmb_try/leroux_with_priors_wo_constraint_mat_w.RData")
#save(rep,file = "./tmb_try/leroux_with_priors_wo_constraint_mat_b.RData")
print(fixed)




# make sure the random effects are optimized at fit$par
obj$fn(fit$par)

# full parameter vector: fixed + random modes
full_par <- obj$env$last.par.best

# now REPORT() values are available
report_vals <- obj$report(full_par)

data$p_mean   <- as.numeric(report_vals$p)
data$phi_mean   <- as.numeric(plogis(report_vals$phi))
data$eta_mean <- as.numeric(report_vals$eta)

library(ggplot2)
p <- ggplot2::ggplot(data) +
  geom_sf(aes(fill =  phi_mean)) +
  scale_fill_viridis_c() +
  theme_minimal()

p


