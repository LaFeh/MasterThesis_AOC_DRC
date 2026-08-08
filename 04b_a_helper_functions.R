#04b_a helper functions


prepare_data_for_prediction <- function(frontline_data,
                                        date_to_predict, N){
  
  # frontline data _control needs to be boolean - only 0 or 1
  
  tm = zoo::as.yearmon(as.Date(date_to_predict),format = "%Y%M")
  data = frontline_data%>%
    filter(time == tm)
  stopifnot(nrow(data)==N)
  data = cbind(data,st_coordinates(st_centroid(data$geometry)))
  data = data[order(data$cell_id),]
  stopifnot(nrow(data)==N)
  
  date = substr(as.Date(date_to_predict),1,7)
  date = as.character(gsub("-","",date))

  save(data, file = paste0("./data/data_for_prediction/",date,"_events.RData"))
  
}



# different distance functions to use
sqrt_dist_in_km <- function(m,dist){
  1 / sqrt(m * (dist/1000))
  
}

dist_in_km <- function(m,dist){
  1 / (m * (dist/1000))
  
}

sqrt_dist <- function(m,dist){
  1 / sqrt(m * (dist))
  
}

# prepare adj_matrix
prepare_adj_matrix_for_prediction <- function(frontline_data,
                                              N,
                                              date = NULL,
                                              snap = 0,
                                              second_degree_neighbours = TRUE,
                                              bol_distance = TRUE,
                                              FUN = dist_in_km){
  
  
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
