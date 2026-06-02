library(TMB)
library(Matrix)

setwd("D:/DRC/gaussian_process_AOC")
# ── 1. Clean recompile ────────────────────────────────────────────────────────
try(dyn.unload(dynlib("./tmb_try/leroux")), silent = TRUE)
file.remove("./tmb_try/leroux.o")
file.remove("./tmb_try/leroux.dll")

compile("./tmb_try/leroux.cpp")
dyn.load(dynlib("./tmb_try/leroux"))


# ----------------------

data = readRDS("./data/inla_data/data_prepared_for_inla.RData")

data_mat_b = readRDS("./data/inla_data/data_mat_b.RData")
data_mat_w = readRDS("./data/inla_data/data_mat_w_mixed_time.RData")

obs_idx <- which(!is.na(data$controle_binom)) - 1L

# y: fill NAs with 0 (value doesn't matter, they won't enter likelihood)
y_filled <- data$controle_binom
y_filled[is.na(y_filled)] <- 0

# X: build on full data -- no NA dropping
X <- model.matrix(~ 1, data)   # intercept only; add covariates carefully
N = nrow(data)


# ── 2. Build W ────────────────────────────────────────────────────────────────
W <- as.matrix(data_mat_w)
W <- (W + t(W)) / 2          # enforce symmetry
diag(W) <- 0                 # no self-loops
W_sp <- as(W, "dgCMatrix")   # TMB requires dgCMatrix exactly

# ── 3. Eigenvalues of the Laplacian (D - W) directly ─────────────────────────
# This is correct for ANY weighted graph (regular or irregular row sums).
# Do NOT use eigenvalues of W alone -- that only works for regular graphs.
D     <- Diagonal(x = rowSums(W))
L     <- D - W_sp                          # graph Laplacian
eig_DmW <- eigen(as.matrix(L),
                 symmetric   = TRUE,
                 only.values = TRUE)$values

# Sanity: all eigenvalues of a valid Laplacian should be >= 0
cat("Min eigenvalue of (D-W):", min(eig_DmW), "\n")   # expect >= 0
cat("N eigenvalues:", length(eig_DmW), "\n")

# ── 4. Design matrix ──────────────────────────────────────────────────────────
#X <- model.matrix(controle_binom ~ 1, data)   # add covariates as needed

# ── 5. Dimension checks BEFORE MakeADFun ─────────────────────────────────────
N <- nrow(data)
stopifnot(nrow(W_sp)    == N)
stopifnot(ncol(W_sp)    == N)
stopifnot(length(eig_DmW) == N)
stopifnot(nrow(X)       == N)
#stopifnot(!anyNA(data$controle_binom))
stopifnot(!anyNA(eig_DmW))
#stopifnot(all(is.finite(W@x)))          # no Inf/NaN in sparse values
cat("All checks passed. N =", N, "\n")

# # ── 6. Data and parameter lists ───────────────────────────────────────────────
data_lst <- list(
  y       = y_filled,   # binary 0/1
  n       = rep(1, N),             # Bernoulli trials
  X       = X,
  W       = W_sp,
  eig_DmW = eig_DmW                # eigenvalues of Laplacian
)


parameters <- list(
  beta      = rep(0, ncol(X)),
  phi       = rep(0, N),
  log_tau   = 0,       # tau = 1
  logit_rho = 0        # rho = 0.5
  
)

# ── 7. MakeADFun ─────────────────────────────────────────────────────────────
obj <- MakeADFun(
  data       = data_lst,
  parameters = parameters,
  random     = "phi",
  DLL        = "leroux",
  silent     = FALSE
)

# ── 8. Quick checks before optimising ────────────────────────────────────────
cat("nll at start:", obj$fn(obj$par), "\n")
cat("Gradient finite:", all(is.finite(obj$gr(obj$par))), "\n")

# ── 9. Optimise ───────────────────────────────────────────────────────────────
fit <- nlminb(
  start     = obj$par,
  objective = obj$fn,
  gradient  = obj$gr,
  control   = list(iter.max = 1000, eval.max = 2000)
)
cat("Convergence:", fit$convergence, "\n")
cat("Message:", fit$message, "\n")

# ── 10. Results ───────────────────────────────────────────────────────────────
rep <- sdreport(obj)
summary(rep, "fixed")    # beta, log_tau, logit_rho
summary(rep, "report")   # tau, rho on natural scale

# ── Fake data test (run this FIRST if real data still aborts) ─────────────────
run_fake_test <- function() {
  N <- 5
  W_fake <- matrix(c(
    0, 1, 0, 0, 1,
    1, 0, 1, 0, 0,
    0, 1, 0, 1, 0,
    0, 0, 1, 0, 1,
    1, 0, 0, 1, 0
  ), N, N)
  W_fake_sp <- as(W_fake, "dgCMatrix")
  L_fake    <- Diagonal(x = rowSums(W_fake)) - W_fake_sp
  eig_fake  <- eigen(as.matrix(L_fake), symmetric = TRUE, only.values = TRUE)$values
  
  dl <- list(
    y = c(1,0,1,1,0), n = rep(1,N),
    X = matrix(1, N, 1),
    W = W_fake_sp, eig_DmW = eig_fake
  )
  pl <- list(beta = 0, phi = rep(0,N), log_tau = 0, logit_rho = 0)
  
  obj_test <- MakeADFun(dl, pl, random = "phi", DLL = "leroux", silent = FALSE)
  cat("Fake data nll:", obj_test$fn(obj_test$par), "\n")
  cat("Fake gradient finite:", all(is.finite(obj_test$gr(obj_test$par))), "\n")
}
run_fake_test()
