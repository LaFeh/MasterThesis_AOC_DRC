library(TMB)
library(Matrix)

setwd("D:/DRC/gaussian_process_AOC")

# ── 1. Clean recompile ────────────────────────────────────────────────────────
try(dyn.unload(dynlib("./tmb_try/leroux_inla_mean_adjusted")), silent = TRUE)
file.remove("./tmb_try/leroux_inla_mean_adjusted.o")
file.remove("./tmb_try/leroux_inla_mean_adjusted.dll")

compile("./tmb_try/leroux_inla_mean_adjusted.cpp")
dyn.load(dynlib("./tmb_try/leroux_inla_mean_adjusted"))

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

cat("N areas:", N, "\n")
cat("Min eigenvalue of (D-W):", min(eig_DmW), "\n")  # expect >= 0 (or tiny negative)
cat("Negative eigenvalues (> -1e-8 is fine):", sum(eig_DmW < -1e-8), "\n")
#plot(data$geometry)
#plot(data[which(is.na(data$lg_landcover_mean)),]$geometry,col ="red",add =T)


#data[which(is.na(data$lg_landcover_mean)),]$lg_landcover_mean = mean(data$lg_landcover_mean,na.rm =T)
# ── 5. Design matrix (all N rows, no NA dropping) ────────────────────────────
# If you have covariates with NAs, either impute them or use only intercept here
X <- model.matrix(~ 1 , data)   # intercept only -- replace with your formula

lower_bounds_beta = rep(-Inf, ncol(X))
upper_bounds_beta = rep(Inf, ncol(X))


#X[,"total_events"] <- scale(X[,"total_events"], center=TRUE, scale=FALSE)
#X[,"total_fatalities"] <- scale(X[,"total_fatalities"], center=TRUE, scale=FALSE)
#X[,"lg_rain_mean"] <- scale(X[,"lg_rain_mean"], center=TRUE, scale=FALSE)
#X[,"lg_landcover_mean"] <- scale(X[,"lg_landcover_mean"], center=TRUE, scale=FALSE)
stopifnot(nrow(X) == N)

# ── 6. Handle NAs in response ────────────────────────────────────────────────
obs_idx  <- which(!is.na(data$controle_binom)) - 1L   # 0-based for C++
y_filled <- data$controle_binom
y_filled[is.na(y_filled)] <- 0   # placeholder; these rows excluded from likelihood

cat("Observed:", length(obs_idx), "/ Missing:", sum(is.na(data$controle_binom)),
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
  n       = rep(1, N),             # Bernoulli; change if binomial counts
  X       = X,
  W       = W_sp,
  eig_DmW = eig_DmW,
  obs_idx = as.integer(obs_idx),   # 0-based indices of observed rows
  sum_to_zero_kappa = 0.1/N        # soft sum-to-zero on phi; mimics INLA constraint
)

parameters <- list(
  beta      = rep(0, ncol(X)),
  phi       = rep(0, N),
  log_tau   = 0,    # tau = 1
  logit_rho = 0.5     # rho = 0.5
)

# ── 9. Build AD objective ─────────────────────────────────────────────────────
obj <- MakeADFun(
  data       = data_lst,
  parameters = parameters,
  random     = "phi",       # phi integrated out by Laplace for ALL N areas
  DLL        = "leroux_inla_mean_adjusted",
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
  lower     = c(lower_bounds_beta, -Inf, -0.5),  # bound logit_rho
  upper     = c(upper_bounds_beta,  5,  6),
  control   = list(iter.max = 5000, eval.max = 2000)
)
cat("Convergence:", fit$convergence, "\n")
cat("Message:", fit$message, "\n")

# ── 11. Uncertainty ───────────────────────────────────────────────────────────
rep <- sdreport(obj)
fixed = summary(rep, "fixed")    # beta, log_tau, logit_rho with SEs
  



# ── 12. Extract predictions for all ar eas (observed + missing) ────────────────
rep_full <- summary(rep, "report")  # tau, rho on natural scale


# Predicted probabilities p = invlogit(eta) for all N areas
p_est <- rep_full[rownames(rep_full) == "p", ]
data$p_mean <- p_est[, "Estimate"]
data$p_se   <- p_est[, "Std. Error"]

# Spatial random effects phi for all N areas
phi_est <- summary(rep, "random")
phi_all <- phi_est[rownames(phi_est) == "phi", ]
data$phi_mean <- phi_all[, "Estimate"]
data$phi_se   <- phi_all[, "Std. Error"]

# Flag which rows were observed vs predicted
data$observed <- !is.na(data$controle_binom)

cat("\nPredicted probabilities at missing locations:\n")
print(head(data[!data$observed, c("p_mean", "p_se", "phi_mean")]))

library(ggplot2)
p <- ggplot2::ggplot(data) +
  geom_sf(aes(fill =  p_mean)) +
  scale_fill_viridis_c() +
  theme_minimal()


p


#data$p_mean_binary = data$p_mean


p <- ggplot2::ggplot(data) +
  geom_sf(aes(fill =  mix_time_mean_scaled)) +
  scale_fill_viridis_c() +
  theme_minimal()

p



#
phi_w   <- summary(rep_w,   "random")[, "Estimate"]
phi_bin <- summary(rep_bin, "random")[, "Estimate"]
phi_w   <- phi_w[  names(phi_w)   == "phi"]
phi_bin <- phi_bin[names(phi_bin) == "phi"]

cat("Correlation of phi_weighted vs phi_binary:", 
    cor(phi_w, phi_bin), "\n")

plot(phi_w, phi_bin,
     xlab = "phi (weighted W)",
     ylab = "phi (binary W)",
     main = "Spatial random effects: weighted vs binary")
abline(0, 1, col = "red")

# Max absolute difference in predicted probabilities
p_w   <- summary(rep_w,   "report")[rownames(summary(rep_w,   "report")) == "p", "Estimate"]
p_bin <- summary(rep_bin, "report")[rownames(summary(rep_bin, "report")) == "p", "Estimate"]
cat("Max diff in predicted p:", max(abs(p_w - p_bin)), "\n")
cat("Mean diff in predicted p:", mean(abs(p_w - p_bin)), "\n")
# If rho ~ 0 in both, the spatial structure is weak regardless of W
# If rho ~ 1 in both, spatial structure is strong but weights don't matter

# ── 13. Fake data test (run standalone to verify model before real data) ──────
run_fake_test <- function() {
  N <- 5
  W_f <- matrix(c(
    0,1,0,0,1,
    1,0,1,0,0,
    0,1,0,1,0,
    0,0,1,0,1,
    1,0,0,1,0
  ), N, N)
  W_f_sp  <- as(W_f, "dgCMatrix")
  L_f     <- Diagonal(x = rowSums(W_f)) - W_f_sp
  eig_f   <- eigen(as.matrix(L_f), symmetric = TRUE, only.values = TRUE)$values

  # 3rd observation is "missing"
  y_f      <- c(1, 0, NA, 1, 0)
  obs_f    <- which(!is.na(y_f)) - 1L
  y_filled <- ifelse(is.na(y_f), 0, y_f)

  dl <- list(
    y = y_filled, n = rep(1, N),
    X = matrix(1, N, 1),
    W = W_f_sp, eig_DmW = eig_f,
    obs_idx = as.integer(obs_f),
    sum_to_zero_kappa = 1000
  )
  pl <- list(beta = 0, phi = rep(0, N), log_tau = 0, logit_rho = 0)

  obj_t <- MakeADFun(dl, pl, random = "phi", DLL = "leroux", silent = TRUE)
  cat("Fake test nll:", obj_t$fn(obj_t$par), "\n")
  cat("Fake gradient finite:", all(is.finite(obj_t$gr(obj_t$par))), "\n")
}
run_fake_test()



##


# ── Fit model 1: weighted W ───────────────────────────────────────────────────
data_lst_w <- list(
  y       = y_filled,
  n       = rep(1, N),
  X       = X,
  W       = W_sp,          # your weighted matrix
  eig_DmW = eig_DmW,
  obs_idx = as.integer(obs_idx),
  sum_to_zero_kappa = 1000
)

obj_w <- MakeADFun(data_lst_w, parameters, random = "phi", DLL = "leroux", silent = TRUE)
fit_w <- nlminb(obj_w$par, obj_w$fn, obj_w$gr,
                lower     = c(lower_bounds_beta, -Inf, -0.5),  # bound logit_rho
                upper     = c(upper_bounds_beta,  5,  6),
                )
rep_w <- sdreport(obj_w)

# ── Fit model 2: binary W ─────────────────────────────────────────────────────
b_bin    <- (as.matrix(data_mat_b) > 0) * 1
b_bin    <- (b_bin + t(b_bin)) / 2
diag(b_bin) <- 0
b_bin_sp <- as(b_bin, "dgCMatrix")

D_bin    <- Diagonal(x = rowSums(b_bin))
L_bin    <- D_bin - b_bin_sp
eig_bin  <- eigen(as.matrix(L_bin), symmetric = TRUE, only.values = TRUE)$values

data_lst_bin <- list(
  y       = y_filled,
  n       = rep(1, N),
  X       = X,
  W       = b_bin_sp,      # binary 0/1
  eig_DmW = eig_bin,
  obs_idx = as.integer(obs_idx),
  sum_to_zero_kappa = 1000
)

obj_bin <- MakeADFun(data_lst_bin, parameters, random = "phi", DLL = "leroux", silent = TRUE)
fit_bin <- nlminb(obj_bin$par, obj_bin$fn, obj_bin$gr,
                  lower = c(rep(-Inf, ncol(X)), -Inf, -6),
                  upper = c(rep( Inf, ncol(X)),  Inf,  6))
rep_bin <- sdreport(obj_bin)

# ── Compare ───────────────────────────────────────────────────────────────────
cat("=== Weighted ===\n");  print(summary(rep_w,   "report"))
cat("=== Binary ===\n");    print(summary(rep_bin, "report"))

# phi correlation
phi_w   <- summary(rep_w,   "random")[rownames(summary(rep_w,   "random")) == "phi", "Estimate"]
phi_bin <- summary(rep_bin, "random")[rownames(summary(rep_bin, "random")) == "phi", "Estimate"]
cat("Correlation of phi estimates:", cor(phi_w, phi_bin), "\n")

# predicted p
p_w   <- summary(rep_w,   "report")[rownames(summary(rep_w,   "report")) == "p", "Estimate"]
p_bin <- summary(rep_bin, "report")[rownames(summary(rep_bin, "report")) == "p", "Estimate"]
cat("Max diff in predicted p:", max(abs(p_w - p_bin)), "\n")
cat("Mean diff in predicted p:", mean(abs(p_w - p_bin)), "\n")

plot(p_w, p_bin,
     xlab = "p weighted", ylab = "p binary",
     main = "Predicted probabilities: weighted vs binary")
abline(0, 1, col = "red")
