#gaussian process frontline data
library(sf)
library(dplyr)
library(kernlab)

setwd("D:/DRC/gaussian_process_AOC")
load("./data/frontline_data_all_previous_mnths_controle_num.RData")


tm = zoo::as.yearmon(as.Date("2023-08-01"),format = "%Y%M")
data = frontline_data_controle_num_all_previous_time%>%
  filter(time == tm)



# box_orig =st_bbox(data)
# box_orig["xmin"] = box_orig["xmin"] +(2/5)*(box_orig["xmax"]-box_orig["xmin"])
# box_orig["ymax"] = box_orig["ymax"] -(2/5)*(box_orig["ymax"]-box_orig["ymin"])
# data_cropped = st_crop(data, box_orig)
# data = data_cropped

data = cbind(data,st_coordinates(st_centroid(data$geometry)))
pred_idx = which(is.na(data$controle_num))

train = data%>%filter(!is.na(controle_num))

data[which(is.na(data$controle_num)),]$controle_num = 0
train =data

train_coords = st_drop_geometry(train)



predictors = c("X", "Y","building_count","building_area","mix_time_mean","r1h","time_step","min_dist_to_rwa")
predictors = c("X", "Y","building_area","mix_time_mean","r1h","min_dist_to_rwa")
predictors = c("X", "Y","min_dist_to_rwa","lg_mix_time_mean")
#predictors = c("X", "Y")
model <- gausspr(
  as.matrix(train_coords[, predictors]),
  train_coords$controle,
  type = "regression",
  kernel = "rbfdot"
)



pred = data%>%filter(is.na(controle_num))
pred = data[pred_idx,]
pred_coords = st_drop_geometry(pred)

pred$predicted_probabilities = predict(model,as.matrix(pred_coords[,predictors]), type="probabilities")[,1]

#plot(pred%>%filter(time_step %in% c(21,22,23)))



library(ggplot2)
qu_cutoff = 0.5


library(ggnewscale)




ggplot() +
  # Second layer (binary)
  geom_sf(data = train# %>%filter(time_step %in% c(21,22,23))
          , aes(fill = controle_num), colour = "black") +
  scale_fill_viridis_c(name = "Observed", option ="magma") +
  ggnewscale::new_scale_fill() +

  # First layer (continuous)
  geom_sf(data = pred, aes(fill = predicted_probabilities)) +
  scale_fill_viridis_c(name = "Probability") 
  



ggplot() +
  # First layer (continuous)
  geom_sf(data = pred, aes(fill = total_fatalities)) +
  scale_fill_viridis_c(name = "Probability")

ggplot() +
  # First layer (continuous)
  geom_sf(data = pred, aes(fill = log(landcover_mean))) +
  scale_fill_viridis_c(name = "Probability")

