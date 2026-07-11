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

prepare_adj_matrix_for_prediction <- function(frontline_data,
                                              N,
                                              date=NULL,
                                              snap = 0,
                                              second_degree_neighbours = FALSE){
  
  if (is.null(date)){
    date = paste0(unique(frontline_data$year_mnth)[1],"01")
  }

  tm = zoo::as.yearmon(as.Date(date, format = "%Y%m%d"),format = "%Y%M")
  data = frontline_data%>%
    filter(time == tm)
  stopifnot(nrow(data)==N)
  
  
  data = cbind(data,st_coordinates(st_centroid(data$geometry)))
  data$mix_time_mean_decay = log(data$mix_time_mean) - min(log(data$mix_time_mean))
  data = data[order(data$cell_id),]
  
  # I cant remove cells with no neighbours, as they would change the ordering in the
  # events dataset. So all cells need to stay put.
  data_adj <- poly2nb(data, queen =T,snap = snap)
  stopifnot(dim(data_adj)[1]==N)
  bol_nghbr = unlist(lapply(data_adj, function(x){if(x[[1]]!=0){TRUE}else{FALSE}}))
  cat(paste0("In Total ",sum(!bol_nghbr)," cells have no neighbours!"))

  # start calculating the neighbours
  if(!second_degree_neighbours) {
    
    
    weights_neightbours_decay <- vector("list", length(data_adj))
    for (id_n_set in seq_along(data_adj)) {
      
      n_set <- data_adj[[id_n_set]]
      
      weights <- numeric(length(n_set))
      
      for (id_n in seq_along(n_set)) {
        
        n <- n_set[id_n]
        
        mean = mean(
          data[c(id_n_set, n), ]$mix_time_mean_decay,
          na.rm = TRUE
        )
        
        weights[id_n] <- 1/(mean)
        if (weights[id_n] >1){
          weights[id_n] = 1
        }
        stopifnot(!is.na(weights[id_n]))
      }
      
      weights_neightbours_decay[[id_n_set]] <- weights
    }
    
    data_adj_nb = nb2listw(data_adj,zero.policy = T)
    data_adj_nb$weights = weights_neightbours_decay
    data_mat_w_decay  = listw2mat(data_adj_nb)
    saveRDS(data_mat_w_decay, "./data/data_for_prediction/data_mat_w_mixed_time_first_neighbour.RData")
    
  }
  else {

    data_mat = nb2mat(data_adj, zero.policy = T)
    data_mat_2 = data_mat %*% data_mat
    diag(data_mat_2) <- 0
    
    data_adj_second = mat2listw(data_mat_2,zero.policy = T,style = "M")
    
    weights_second_neightbours_decay <- vector("list", length(data_adj_second))
    
    for (start_point in seq_along(data_adj_second$neighbours)) {
      
      all_neighbours <- data_adj_second$neighbours[[start_point]]
      
      weights <- numeric(length(all_neighbours))
      
      for (end_point_idx in seq_along(all_neighbours)) {
        
        end_point <- all_neighbours[end_point_idx]
        
        # if first degree neighbour
        if (end_point %in% which(data_mat[start_point, ] > 0)){
          
          mean = mean(
            data[c(start_point, end_point), ]$mix_time_mean_decay,
            na.rm = TRUE
          )
          
          weights[end_point_idx] <- 1/(mean)
          if (weights[end_point_idx] >1){
            weights[end_point_idx] = 1
          }
          stopifnot(!is.na(weights[end_point_idx]))
          
          
        } 
        else { # if second degree neighbour
          
          intermediates <- which(data_mat[start_point, ] > 0 & data_mat[, end_point] > 0)
          
          mean = mean(
            data[c(start_point, end_point,intermediates), ]$mix_time_mean_decay,
            na.rm = TRUE
          )
          
          weights[end_point_idx] <- 1/(mean)^2
          if (weights[end_point_idx] >1){
            weights[end_point_idx] = 1
          }
          stopifnot(!is.na(weights[end_point_idx]))
          
        }
        
        weights_second_neightbours_decay[[start_point]] <- weights
      }
    }
    
    data_adj_nb = nb2listw(data_adj_second$neighbours,zero.policy = T)
    data_adj_nb$weights = weights_second_neightbours_decay
    data_mat_w_decay  = listw2mat(data_adj_nb)
    saveRDS(data_mat_w_decay, "./data/data_for_prediction/data_mat_w_mixed_time_second_neighbour.RData")
    
    
  }



}
