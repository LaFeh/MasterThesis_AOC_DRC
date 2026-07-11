#05a run model

library(Matrix)
setwd("D:/DRC/gaussian_process_AOC")
source("./05_model/05a_a_helper_run_model.R")
# ── 1. set up ──────────────────────────────────────────────────────────────
degree_of_neighbour = "first"
covariates_title = "Intercept"
other_name = "" 

covariates = as.formula("~1")

#directory for saving all the output
output_folder_name = paste0("model_",
                  degree_of_neighbour,
                  "_neighbour_covariates_",covariates_title,
                  other_name)

output_path = paste0("./05_model/",output_folder_name)
dir.create(output_path)
dir.create(paste0(output_path,"/plots"))


data_mat_w <- readRDS(paste0("./data/data_for_prediction/data_mat_w_mixed_time_",degree_of_neighbour,"_neighbour.RData"))
rho =0.6


# ── 3. Build W over ALL areas (including NA areas) ───────────────────────────
W <- as.matrix(data_mat_w)
W <- (W + t(W)) / 2    # enforce symmetry
diag(W) <- 0           # no self-loops
W_sp <- as(W, "dgCMatrix")


# ── 4. Eigenvalues of Laplacian (D - W) ──────────────────────────────────────
D       <- Diagonal(x = rowSums(W))
L       <- D - W_sp


# load or calculate eigenvalues of W
eigenvalue_file_path = paste0("./05_model/eigvalues_data_mat_w_mixed_time_",degree_of_neighbour,"_neighbour.RData")

if (file.exists(eigenvalue_file_path)){
  
  load(file =  paste0("./05_model/eigvalues_data_mat_w_mixed_time_",degree_of_neighbour,"_neighbour.RData"))

} else {
  
  eig_DmW <- eigen(as.matrix(L), symmetric = TRUE, only.values = TRUE)$values
  save(eig_DmW,file = paste0("./05_model/eigvalues_data_mat_w_mixed_time_",degree_of_neighbour,"_neighbour.RData"))

}

cat("Min eigenvalue of (D-W):", min(eig_DmW), "\n")  # expect >= 0 (or tiny negative)
cat("Negative eigenvalues (> -1e-8 is fine):", sum(eig_DmW < -1e-8), "\n")


# ── 7. Dimension checks ───────────────────────────────────────────────────────
stopifnot(all(is.finite(W_sp@x)))
stopifnot(all(is.finite(eig_DmW)))
cat("All dimension checks passed.\n")

load(file = "./data/acled_conflict_mnth.RData")
yrs = unique(substr(acled_conflict_mnth$year_mnth,1,4))
mnths = unique(substr(acled_conflict_mnth$year_mnth,5,7))

date_combinations = dplyr::cross_join(tidyr::as_tibble(yrs),tidyr::as_tibble(mnths))

date_combinations$time_step = 1:nrow(date_combinations)
date_combinations$year_mnth = as.numeric(paste0(date_combinations$value.x,date_combinations$value.y))


all_dates = date_combinations$year_mnth

for ( date in all_dates){
  
  
  load(paste0("./data/data_for_prediction/",date,"_events.RData"))
  N <- nrow(data)
  
  X <- model.matrix(covariates, data)   # intercept only -- replace with your formula
  
  lower_bounds_beta = rep(-Inf, ncol(X))
  upper_bounds_beta = rep(Inf, ncol(X))
  
  stopifnot(nrow(X) == N)
  stopifnot(nrow(W_sp) == N)
  stopifnot(ncol(W_sp) == N)
  stopifnot(length(eig_DmW)   == N)
  
  # ── 6. Handle NAs in response ────────────────────────────────────────────────
  obs_idx  <- which(!is.na(data$control_binom)) - 1L   # 0-based for C++
  y_filled <- data$control_binom
  y_filled[is.na(y_filled)] <- 0   # placeholder; these rows excluded from likelihood
  
  cat("Observed:", length(obs_idx), "/ Missing:", sum(is.na(data$control_binom)),
      "/ Total:", N, "\n")
  stopifnot(length(y_filled)  == N)
  
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
    #logit_rho_prior_mean = 10,
    #logit_rho_prior_sd   = 1,
    rho = rho
  )
  
  
  parameters <- list(
    beta      = rep(0, ncol(X)),
    phi       = rep(0, N),
    log_tau   = 0#,    # tau = 1
    #logit_rho = 0.9     # rho = 0.5
  )
  
  run_model(date = date,
            data_lst, 
            parameters,
            cpp_file = paste0("./05_model/leroux_with_priors_wo_constraint_alldata"),
            output_path = output_path)
  
}

