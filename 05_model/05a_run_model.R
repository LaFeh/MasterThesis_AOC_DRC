#05a run model

library(Matrix)
source("./00b_helper_create_grid_name.R")
source("./05_model/05a_a_helper_run_model.R")
# ── 1. set up ──────────────────────────────────────────────────────────────


# -- 1.1 grid setup ------------------------
grid_add_streets = T
grid_add_nationalparks =T
grid_add_waterways = T

grid_cell_size     <- 5000


# -- 1.2 model setup ------------------------

model_dates_to_run = "202405" #if NULL all are run
model_degree_of_neighbour = "second"
model_bol_distance = TRUE
model_other_name = "_friction_surface_simple_grid" 
model_rho = 0.6

covariates_list = create_covariates_list()

model_covariates_title = "Intercept"
model_covariates = covariates_list[[model_covariates_title]]



#directory for saving all the output
output_folder_name = paste0("model_",
                            model_degree_of_neighbour,
                  "_neighbour_covariates_",model_covariates_title,
                  model_other_name)

output_path = paste0("./05_model/",output_folder_name)
dir.create(output_path)
dir.create(paste0(output_path,"/plots"))



name_of_grid = create_grid_name(add_waterways = grid_add_waterways,
                                add_nationalparks = grid_add_nationalparks,
                                add_streets = grid_add_streets,
                                cell_size = grid_cell_size)

name_of_grid_clean = gsub(".shp","",name_of_grid)

path_of_adjacency_matrix = paste0("./data/data_for_prediction/mat_w_mixedtime_neighbour",
                                  model_degree_of_neighbour,
                                  "_distance_", model_bol_distance,
                                  "_",name_of_grid_clean,".RData")


path_of_eigenvalue = paste0("./05_model/eigvalues_mat_w_mixedtime_",
                              model_degree_of_neighbour,
                              "_distance_", model_bol_distance,
                              "_",name_of_grid_clean,".RData")

##########################################


data_mat_w <- readRDS(path_of_adjacency_matrix)




# ── 3. Build W over ALL areas (including NA areas) ───────────────────────────
W <- as.matrix(data_mat_w)
W <- (W + t(W)) / 2    # enforce symmetry
diag(W) <- 0           # no self-loops
W_sp <- as(W, "dgCMatrix")


# ── 4. Eigenvalues of Laplacian (D - W) ──────────────────────────────────────
D       <- Diagonal(x = rowSums(W))
L       <- D - W_sp


# load or calculate eigenvalues of W



if (file.exists(path_of_eigenvalue)){

  load(file =  path_of_eigenvalue)

} else {
  
  eig_DmW <- eigen(as.matrix(L), symmetric = TRUE, only.values = TRUE)$values
  save(eig_DmW,file = path_of_eigenvalue)

}

cat("Min eigenvalue of (D-W):", min(eig_DmW), "\n")  # expect >= 0 (or tiny negative)
cat("Negative eigenvalues (> -1e-8 is fine):", sum(eig_DmW < -1e-8), "\n")


# ── 7. Dimension checks ───────────────────────────────────────────────────────
stopifnot(all(is.finite(W_sp@x)))
stopifnot(all(is.finite(eig_DmW)))
cat("All dimension checks passed.\n")

load(file = "./data/acled_conflict_mnth.RData")


if(!is.null(model_dates_to_run)){
  
  all_dates = model_dates_to_run
  
} else{
  
  yrs = unique(substr(acled_conflict_mnth$year_mnth,1,4))
  mnths = unique(substr(acled_conflict_mnth$year_mnth,5,7))
  date_combinations = dplyr::cross_join(tidyr::as_tibble(yrs),tidyr::as_tibble(mnths))
  date_combinations$time_step = 1:nrow(date_combinations)
  date_combinations$year_mnth = as.numeric(paste0(date_combinations$value.x,date_combinations$value.y))
  
  all_dates = date_combinations$year_mnth
  
}

for ( date in all_dates){
  print(date)
  
  load(paste0("./data/data_for_prediction/",date,"_events.RData"))
  N <- nrow(data)
  
  X <- model.matrix(model_covariates, data)   # intercept only -- replace with your formula
  
  lower_bounds_beta = rep(-Inf, ncol(X))
  upper_bounds_beta = rep(Inf, ncol(X))
  
  stopifnot(nrow(X) == N)
  stopifnot(nrow(W_sp) == N)
  stopifnot(ncol(W_sp) == N)
  stopifnot(length(eig_DmW)   == N)
  
  # ── 6. Handle NAs in response ────────────────────────────────────────────────
  obs_idx  <- which(!is.na(data$control_binom)) - 1L   # 0-based for C++
  y_filled <- data$control_binom
  y_filled[is.na(y_filled)] <- 0  
  
  weights_list = mat2listw(W,style ="W",zero.policy=TRUE)
  moran_result <- spdep::moran.test(y_filled, weights_list, adjust = T)
  cat(moran_result[["estimate"]])
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
    rho = model_rho
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


