#gaussian process frontline data
library(sf)
library(dplyr)
library(kernlab)

setwd("D:/DRC/gaussian_process_AOC")
load("./data/frontline_data_2_mnths.RData")


tm = zoo::as.yearmon(as.Date("2024-08-01"),format = "%Y%M")
data = frontline_data%>%
  filter(time == tm)
data = frontline_data

data$controle_bol = NA
data[which(data$controle=="non-state actor"),]$controle_bol = 1
data[which(data$controle=="government"),]$controle_bol = 0

data = cbind(data,st_coordinates(st_centroid(data$geometry)))

train = data%>%filter(!is.na(controle))

train_coords = st_drop_geometry(train)



predictors = c("X", "Y","building_count","building_area","mean_mix_time","r1h","time_step")
predictors = c("X", "Y","min_dist_to_rwa","lg_mix_time_mean","landcover_mean","r1h")
model <- gausspr(
  as.matrix(train_coords[, predictors]),
  as.factor(train_coords$controle_bol),
  type = "classification",
  kernel = "rbfdot"
)



pred = data%>%filter(is.na(controle_bol))
pred_coords = st_drop_geometry(pred)
pred$probability_ones = predict(model,as.matrix(pred_coords[,predictors]), type="probabilities")[,"1"]


#plot(pred%>%filter(time_step %in% c(21,22,23)))



library(ggplot2)
qu_cutoff = 0.5


library(ggnewscale)

ggplot() +
  # First layer (continuous)
  geom_sf(data = pred, aes(fill = probability_ones)) +
  scale_fill_viridis_c(name = "Probability") +
  
  ggnewscale::new_scale_fill() +
  
  # Second layer (binary)
  geom_sf(data = train# %>%filter(time_step %in% c(21,22,23))
          , aes(fill = factor(controle_bol)), colour = "black") +
  scale_fill_manual(values = c("0" = "black", "1" = "white"),
                    name = "Observed")


ggplot() +
  # First layer (continuous)
  geom_sf(data = pred, aes(fill = log(landcover_mean))) +
  scale_fill_viridis_c(name = "Probability")

