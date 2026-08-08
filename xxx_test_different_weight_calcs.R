### try different distances
sqrt_dist_in_km <- function(m,dist){
  1 / sqrt(m * (dist/1000))
  
}
 
dist_in_km <- function(m,dist){
  1 / (m * (dist/1000))
  
}

sqrt_dist <- function(m,dist){
  1 / sqrt(m * (dist))
  
}

source("./05_model/05a_a_helper_run_model.R")
prepare_adj_matrix_for_prediction <- function(frontline_data,
                                              N,
                                              date = NULL,
                                              snap = 0,
                                              second_degree_neighbours = TRUE,
                                              bol_distance = TRUE,
                                              FUN = sqrt_dist_in_km){
  
  library(units)
  
  if (is.null(date)){
    date <- paste0(unique(frontline_data$year_mnth)[1], "01")
  }
  
  tm <- zoo::as.yearmon(as.Date(date, format = "%Y%m%d"), format = "%Y%M")
  data <- frontline_data %>% filter(time == tm)
  stopifnot(nrow(data) == N)
  
  data <- cbind(data, st_coordinates(st_centroid(data$geometry)))
  data$mix_time_mean_decay <- log(data$mix_time_mean) - min(log(data$mix_time_mean))
  data <- data[order(data$cell_id), ]
  
  # Plain vectors -> no sf/data.frame dispatch overhead inside loops
  coords <- st_coordinates(st_centroid(data$geometry))  # recompute after sort, cheap once
  decay  <- data$mix_time_mean_decay
  
  data_adj <- poly2nb(data, queen = TRUE, snap = snap)
  stopifnot(dim(data_adj)[1] == N)
  bol_nghbr <- vapply(data_adj, function(x) x[1] != 0, logical(1))
  cat(paste0("In Total ", sum(!bol_nghbr), " cells have no neighbours!"))
  
  # Planar Euclidean distance from precomputed centroids, instead of st_distance()
  # NB: only valid if `data` is in a projected CRS. If it's lon/lat, keep st_distance().
  euclid_dist <- function(i, j) sqrt((coords[i,1]-coords[j,1])^2 + (coords[i,2]-coords[j,2])^2)
  
  if (!second_degree_neighbours) {
    
    weights_neightbours_decay <- vector("list", length(data_adj))
    for (id_n_set in seq_along(data_adj)) {
      n_set <- data_adj[[id_n_set]]
      if (identical(n_set, 0L)) {
        weights_neightbours_decay[[id_n_set]] <- numeric(0)
        next
      }
      m <- (decay[id_n_set] + decay[n_set]) / 2       # vectorised over all neighbours at once
      w <- 1 / m
      w[w > 1] <- 1
      stopifnot(!any(is.na(w)))
      weights_neightbours_decay[[id_n_set]] <- w
    }
    
    data_adj_nb <- nb2listw(data_adj, zero.policy = TRUE)
    data_adj_nb$weights <- weights_neightbours_decay
    data_mat_w_decay <- listw2mat(data_adj_nb)
    saveRDS(data_mat_w_decay, "./data/data_for_prediction/data_mat_w_mixed_time_first_neighbour.RData")
    
  } else {
    
    data_mat <- nb2mat(data_adj, zero.policy = TRUE)
    data_mat_2 <- data_mat %*% data_mat
    diag(data_mat_2) <- 0
    
    data_adj_second <- mat2listw(data_mat_2, zero.policy = TRUE, style = "B")
    
    # compute each row's first-degree set once, reuse instead of recomputing `which()` twice per pair
    first_deg_list <- lapply(seq_len(nrow(data_mat)), function(i) which(data_mat[i, ] > 0))
    
    weights_second_neightbours_decay <- vector("list", length(data_adj_second$neighbours))
    
    for (start_point in seq_along(data_adj_second$neighbours)) {
      
      all_neighbours <- data_adj_second$neighbours[[start_point]]
      if (all(all_neighbours == 0)) {
        weights_second_neightbours_decay[[start_point]] <- 0
        next
      }
      
      first_deg_sp <- first_deg_list[[start_point]]
      weights <- numeric(length(all_neighbours))
      
      for (end_point_idx in seq_along(all_neighbours)) {
        end_point <- all_neighbours[end_point_idx]
        
        intermediates <- which(data_mat[start_point, ] > 0 & data_mat[, end_point] > 0)
        idx <- c(start_point, end_point, intermediates)
        m <- mean(decay[idx], na.rm = TRUE)
        
        if (bol_distance) {
          dist <- euclid_dist(start_point, end_point)
          if (dist < 1) dist <- 1
          weights[end_point_idx] <- FUN(m,dist)
        } else {
          weights[end_point_idx] <- 1 / (m^2)
        }
        
        if (weights[end_point_idx] > 1) weights[end_point_idx] <- 1
        stopifnot(!is.na(weights[end_point_idx]))
      }
      
      
      #}
      
      weights_second_neightbours_decay[[start_point]] <- weights
    }
    
    data_adj_nb <- nb2listw(data_adj_second$neighbours, zero.policy = TRUE)
    data_adj_nb$weights <- weights_second_neightbours_decay
    data_mat_w_decay <- listw2mat(data_adj_nb)
    saveRDS(data_mat_w_decay,
            paste0("./data/data_for_prediction/data_mat_w_mixed_time_second_neighbour_distance_", bol_distance, ".RData"))
  }
}



library(Matrix)

# ── 1. set up ──────────────────────────────────────────────────────────────
degree_of_neighbour = "second"
covariates_title = "Intercept"
bol_distance = TRUE
FUN=sqrt_dist_in_km
FUN_name = as.character("sqrt_dist_in_km")

other_name = paste0("_friction_surface_wo_streams_with_streets_",FUN_name) 

covariates = as.formula("~1")

#directory for saving all the output
output_folder_name = paste0("model_",
                            degree_of_neighbour,
                            "_neighbour_covariates_",covariates_title,
                            other_name)

output_path = paste0("./05_model/",output_folder_name)
dir.create(output_path)
dir.create(paste0(output_path,"/plots"))




load(file = "./data/acled_conflict_mnth.RData")
yrs = unique(substr(acled_conflict_mnth$year_mnth,1,4))
mnths = unique(substr(acled_conflict_mnth$year_mnth,5,7))

date_combinations = dplyr::cross_join(tidyr::as_tibble(yrs),tidyr::as_tibble(mnths))

date_combinations$time_step = 1:nrow(date_combinations)
date_combinations$year_mnth = as.numeric(paste0(date_combinations$value.x,date_combinations$value.y))


all_dates = date_combinations$year_mnth


print(date)

load(paste0("./data/data_for_prediction/",date,"_events.RData"))
N <- nrow(data)

prepare_adj_matrix_for_prediction(data,
                                              N,
                                              date = NULL,
                                              snap = 10,
                                              second_degree_neighbours = TRUE,
                                              bol_distance = TRUE,
                                              FUN = FUN)

data_mat_w <- readRDS(paste0("./data/data_for_prediction/data_mat_w_mixed_time_",degree_of_neighbour,"_neighbour_distance_",bol_distance,".RData"))
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
eigenvalue_file_path = paste0("./05_model/eigvalues_data_mat_w_mixed_time_",degree_of_neighbour,"_neighbour_distance",bol_distance,"_",other_name,".RData")

eig_DmW <- eigen(as.matrix(L), symmetric = TRUE, only.values = TRUE)$values
save(eig_DmW,file = eigenvalue_file_path)



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
y_filled[is.na(y_filled)] <- 0  

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





