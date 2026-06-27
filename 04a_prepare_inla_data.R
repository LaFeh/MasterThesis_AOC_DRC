

library(sf)
library(dplyr)
library(spdep)

setwd("D:/DRC/gaussian_process_AOC")
load("./data/frontline_data_all_previous_mnths_controle_num.RData")


tm = zoo::as.yearmon(as.Date("2025-02-01"),format = "%Y%M")
data = frontline_data_controle_num_all_previous_time%>%
  filter(time == tm)

# data dependent var:
data$control_binom = data$control
data[which(data$control_binom ==0.5),]$control_binom = 0
mean(data$control_binom,na.rm =T)

data = cbind(data,st_coordinates(st_centroid(data$geometry)))



min_max_scaler <- function(x, min_target = 0, max_target = 1) {
  # Fehler abfangen, wenn Eingabe numerisch sein muss
  if (!is.numeric(x)) {
    stop("Die Eingabe muss numerisch sein.")
  }
  
  # Min und Max der Eingabedaten bestimmen
  min_val <- min(x, na.rm = TRUE)
  max_val <- max(x, na.rm = TRUE)
  
  # Sonderfall: Wenn alle Werte identisch sind (Division durch 0 verhindern)
  if (min_val == max_val) {
    return(rep(min_target, length(x)))
  }
  
  # Transformation auf das Zielintervall
  x_scaled <- (x - min_val) / (max_val - min_val)
  x_scaled <- x_scaled * (max_target - min_target) + min_target
  
  return(x_scaled)
}

data$mix_time_mean_scaled = min_max_scaler(log(data$mix_time_mean))
data$mix_time_mean_scaled = 1-data$mix_time_mean_scaled


#data$mix_time_mean_decay = 1/(data$mix_time_mean^2)
data$mix_time_mean_decay = log(data$mix_time_mean) - min(log(data$mix_time_mean))
plot(data[,c("mix_time_mean")])

data_adj <- poly2nb(data, queen =T)
summary(data_adj)

bol_nghbr = unlist(lapply(data_adj, function(x){if(x[[1]]!=0){TRUE}else{FALSE}}))
which(!bol_nghbr)
data = data[bol_nghbr,]

data_adj <- poly2nb(data, queen =T, snap = 3)


# weights_neightbours_basic <- vector("list", length(data_adj))
# 
# for (id_n_set in seq_along(data_adj)) {
#   
#   n_set <- data_adj[[id_n_set]]
#   
#   weights <- numeric(length(n_set))
#   
#   for (id_n in seq_along(n_set)) {
#     
#     n <- n_set[id_n]
#     
#     weights[id_n] <- mean(
#       data[c(id_n_set, n), ]$mix_time_mean_scaled,
#       na.rm = TRUE
#     )
#     stopifnot(!is.na(weights[id_n]))
#   }
#   
#   weights_neightbours_basic[[id_n_set]] <- weights
# }


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


data$ID <- 1:nrow(data)

data_mat_b<- nb2mat(data_adj, style = "B")
saveRDS(data_mat_b, "./data/inla_data/data_mat_b.RData")


data_adj_nb = nb2listw(data_adj)
# data_adj_nb$weights = weights_neightbours_basic
# data_mat_w  = listw2mat(data_adj_nb)
# saveRDS(data_mat_w, "./data/inla_data/data_mat_w_mixed_time.RData")


data_adj_nb$weights = weights_neightbours_decay
data_mat_w_decay  = listw2mat(data_adj_nb)

saveRDS(data_mat_w_decay, "./data/inla_data/data_mat_w_mixed_time_decay.RData")

saveRDS(data, "./data/inla_data/data_prepared_for_inla.RData")


