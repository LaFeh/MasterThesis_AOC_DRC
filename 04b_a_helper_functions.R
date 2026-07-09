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

prepare_adj_matrix_for_prediction <- function(frontline_data,N){
  
  
  date = unique(frontline_data$year_mnth)[1]
  tm = zoo::as.yearmon(as.Date(date_to_predict),format = "%Y%M")
  data = frontline_data%>%
    filter(time == tm)
  stopifnot(nrow(data)==N)
  
  
  data = cbind(data,st_coordinates(st_centroid(data$geometry)))
  data$mix_time_mean_decay = log(data$mix_time_mean) - min(log(data$mix_time_mean))
  data = data[order(data$cell_id),]
  
  # I cant remove cells with no neighbours, as they would change the ordering in the
  # events dataset. So all cells need to stay put.
  data_adj <- poly2nb(data, queen =T,snap = 3)
  stopifnot(dim(data_adj)[1]==N)
  bol_nghbr = unlist(lapply(data_adj, function(x){if(x[[1]]!=0){TRUE}else{FALSE}}))
  cat(paste0("In Total ",sum(!bol_nghbr)," cells have no neighbours!"))

  
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
  
  saveRDS(data_mat_w_decay, "./data/data_for_prediction/data_mat_w_mixed_time_decay.RData")
  
  
  
  
}


