#04b_a helper functions


prepare_data_for_prediction <- function(frontline_data,
                                        date_to_predict, N, grid_file_name){
  
  # frontline data _control needs to be boolean - only 0 or 1
  
  tm = zoo::as.yearmon(as.Date(date_to_predict),format = "%Y%M")
  data = frontline_data[which(frontline_data$time == tm),]
  stopifnot(nrow(data)==N)
  data = cbind(data,st_coordinates(st_centroid(data$geometry)))
  data = data[order(data$cell_id),]
  stopifnot(nrow(data)==N)
  
  date = substr(as.Date(date_to_predict),1,7)
  date = as.character(gsub("-","",date))

  dir_name = paste0("./data/data_for_prediction/",grid_file_name,"/")
  if (!dir.exists(dir_name)){
    dir.create(dir_name)
  }
  
  save(data, file = paste0(dir_name,date,"_events.RData"))
  
}



# different distance functions to use
sqrt_dist_in_km <- function(m,dist){
  dist[dist<1] = 1000
  1 / sqrt(m * (dist/1000))
  
}

dist_in_km <- function(m,dist){
  dist[dist<1] = 1000
  1 / (m * (dist/1000))
  
}

sqrt_dist <- function(m,dist){
  dist[dist<1] = 1
  1 / sqrt(m * (dist))
  
}

calculate_weight <- function(start_point, # 
                             end_point, # neighbour of start point
                             intermediaries = NULL, # and values between start_point and endpoint to be considered when calculating traveltime mean
                             travel_time,
                             bol_distance, # if distance should be considered 
                             border_distance, # if True uses st_distance, if touching dist = 0, else centroid distance
                             coords = NULL, # only if border_distance = F
                             FUN = FUN
                             ){
  

  # average of traveltime distance
  m <- sapply(seq_along(end_point),function(x) base::sum(travel_time[start_point],
                                                         travel_time[start_point],
                                                         travel_time[intermediaries[[x]]],na.rm =T) / (length(travel_time[intermediaries[[x]]]) + 2))  
  
  
  if(bol_distance){

    if(border_distance){
      dist = st_distance(data[start_point,],data[end_point,])
      dist = units::drop_units(dist)
    } else{ # centroid distance
      # Planar Euclidean distance from precomputed centroids, instead of st_distance()
      # NB: only valid if `data` is in a projected CRS. If it's lon/lat, keep st_distance().
      euclid_dist <- function(i, j) sqrt((coords[i,1]-coords[j,1])^2 + (coords[i,2]-coords[j,2])^2)
      dist <- euclid_dist(start_point,end_point)
    }
    
    w <- FUN(m,dist)
    
  }else{

    w <- 1 / m
    
  }
  
  w[w > 1] <- 1
  stopifnot(!any(is.na(w)))
  
  return(w)
}

# prepare adj_matrix
prepare_adj_matrix_for_prediction <- function(frontline_data,
                                              N,
                                              date = NULL,
                                              snap = 0,
                                              bol_second_degree_neighbours = TRUE,
                                              bol_distance = TRUE,
                                              border_distance = FALSE,
                                              FUN = dist_in_km,
                                              name_of_grid = name_of_grid){
  
  
  library(units)
  
  if (is.null(date)){
    date <- paste0(unique(frontline_data$year_mnth)[1], "01")
    tm <- zoo::as.yearmon(as.Date(date, format = "%Y%m%d"), format = "%Y%M")
  } else if (class(date)!="yearmon"){
    tm <- zoo::as.yearmon(as.Date(date, format = "%Y%m%d"), format = "%Y%M")
  }else{
    tm <- date
  }
  

  data <- frontline_data[which(frontline_data$time ==tm),]
  rm(frontline_data)
  stopifnot(nrow(data) == N)
  
  data <- cbind(data, st_coordinates(st_centroid(data$geometry)))
  data$mix_time_mean_decay <- log(data$mix_time_mean) - min(log(data$mix_time_mean))
  data <- data[order(data$cell_id), ]
  
  # Plain vectors -> no sf/data.frame dispatch overhead inside loops

  decay  <- data$mix_time_mean_decay
  
  data_adj <- poly2nb(data, queen = TRUE, snap = snap)
  stopifnot(dim(data_adj)[1] == N)
  bol_nghbr <- vapply(data_adj, function(x) x[1] != 0, logical(1))
  cat(paste0("In Total ", sum(!bol_nghbr), " cells have no neighbours!"))
  
  
  if(!border_distance & bol_distance){ # then centroid distance
    # Planar Euclidean distance from precomputed centroids, instead of st_distance()
    # NB: only valid if `data` is in a projected CRS. If it's lon/lat, keep st_distance().
    euclid_dist <- function(i, j) sqrt((coords[i,1]-coords[j,1])^2 + (coords[i,2]-coords[j,2])^2)
    coords = st_coordinates(data)
    
  }

  if (!bol_second_degree_neighbours) {
    
    weights_neightbours_decay <- vector("list", length(data_adj))
    
    for (id_n_set in seq_along(data_adj)) {
      
      n_set <- data_adj[[id_n_set]]
      
      if (identical(n_set, 0L)) {
        weights_neightbours_decay[[id_n_set]] <- numeric(0)
        next
      }
      
      w = calculate_weight(start_point = id_n_set,
                          end_point = n_set, # neighbour of start point
                          intermediaries = NULL, # and values between start_point and endpoint to be considered when calculating traveltime mean
                          travel_time = decay,
                          bol_distance = bol_distance, # if distance should be considered 
                          border_distance = border_distance, # if True uses st_distance, if touching dist = 0, else centroid distance
                          coords = coords, # only if border_distance = F
                          FUN = FUN
      )

      weights_neightbours_decay[[id_n_set]] <- w
      
    }
    cat("all weights have been calculated")
    data_adj_nb <- nb2listw(data_adj, zero.policy = TRUE)
    data_adj_nb$weights <- weights_neightbours_decay
    #data_mat_w_decay <- listw2mat(data_adj_nb)
    data_mat_w_decay <- sphet::listw2dgCMatrix(data_adj_nb, zero.policy = TRUE)

  } else { # if second degree neighbours
    
    data_mat <- nb2mat(data_adj, zero.policy = TRUE)
    data_mat_2 <- (data_mat %*% data_mat) + data_mat
    diag(data_mat_2) <- 0
    data_adj_all <- mat2listw(data_mat_2, zero.policy = TRUE, style = "W")
    weights_all_neightbours <- vector("list", length(data_adj_all$neighbours))
    
    # compute each row's first-degree set once, reuse instead of recomputing `which()` twice per pair
    first_deg_list <- lapply(seq_len(nrow(data_mat)), function(i) which(data_mat[i, ] > 0))

    
    for (start_point in seq_along(data_adj_all$neighbours)) {

      all_neighbours <- data_adj_all$neighbours[[start_point]]
      
      if (all(all_neighbours == 0)) {
        weights_all_neightbours[[start_point]] <- 0
        next
      }
      
      first_deg_sp <- first_deg_list[[start_point]]
      weights <- numeric(length(all_neighbours))
      names(weights)<-as.character(all_neighbours)
      second_degree_neighbours = all_neighbours[!all_neighbours %in% first_deg_sp]
      
      # first neighbours ####
      
      n_set = first_deg_sp # first degree endpoints
      
      w = calculate_weight(start_point = start_point,
                           end_point = n_set, # neighbour of start point
                           intermediaries = NULL, # and values between start_point and endpoint to be considered when calculating traveltime mean
                           travel_time = decay,
                           bol_distance = bol_distance, # if distance should be considered 
                           border_distance = border_distance, # if True uses st_distance, if touching dist = 0, else centroid distance
                           coords = coords, # only if border_distance = F
                           FUN = FUN
      )

      weights[as.character(first_deg_sp)] <- w
      
      # second degree neighbours #########
      if (length(second_degree_neighbours)>1){
        
        n_set = second_degree_neighbours
        intermediates_per_endpoint = lapply(n_set,function(x) {contenders = which(data_mat[,n_set ] > 0); contenders[contenders %in% which(data_mat[start_point, ] > 0)]} )
        
        
        w = calculate_weight(start_point = start_point,
                             end_point = n_set, # neighbour of start point
                             intermediaries = intermediates_per_endpoint, # and values between start_point and endpoint to be considered when calculating traveltime mean
                             travel_time = decay,
                             bol_distance = bol_distance, # if distance should be considered 
                             border_distance = border_distance, # if True uses st_distance, if touching dist = 0, else centroid distance
                             coords = coords, # only if border_distance = F
                             FUN = FUN
        )
        
        weights[as.character(second_degree_neighbours)] <- w
        
      }

      
      # combine first and seconde degree neighbours ###############
      weights_all_neightbours[[start_point]] = weights
     
    }
    cat("all weights have been calculated")
    
    data_adj_nb <- nb2listw(data_adj_all$neighbours, zero.policy = TRUE)
    data_adj_nb$weights <- weights_all_neightbours
    data_mat_w_decay <- listw2mat(data_adj_nb)
    
  }
  
  
  cat("All weights have been added to the matrix")
  # file naming ##########
  if(bol_second_degree_neighbours){
    degree = "second"
  }else{
    degree = "first"
  }
  
  grid_name = gsub(".shp","",name_of_grid)
  
  saveRDS(data_mat_w_decay,
          paste0("./data/data_for_prediction/mat_w_mixedtime_neighbour",degree,"_distance_", bol_distance,"_",border_distance,"_",grid_name,".RData"))

}

