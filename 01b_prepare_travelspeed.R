# travelspeed data
setwd("D:/DRC/gaussian_process_AOC")

library(sf)
library(dplyr)
library(terra)


# ============================================================
# 1. walk time
# ============================================================


tif_path <-'./data/GRID3_COD_walk_travel_time_friction_surface_v1/GRID3_COD_walk_travel_time_friction_surface_v1.tif' 
walk_time=rast(tif_path)


grid = read_sf("./data/grid_surface.shp")
grid = st_transform(grid,st_crs(walk_time))

 # crop raster to grid bounding box
walk_time_crop <- crop(
  walk_time,
  vect(grid),
  filename = "./data/walk_crop.tif",
  overwrite = TRUE
)


library(exactextractr)

grid$walk_time_mean <- exact_extract(
  walk_time_crop,
  grid,
  'mean'
)

grid$lg_walk_time_mean = log(grid$walk_time_mean)

# ============================================================
# 1.2 write walking data
# ============================================================

grid = st_drop_geometry(grid)
data.table::fwrite(grid[,c("cell_id","walk_time_mean","lg_walk_time_mean")],"./data/grid_walk_time.csv")

# ============================================================
# 2. travel speed (motorized)
# ============================================================

## start with mixed travel speed
setwd("D:/DRC/gaussian_process_AOC")

library(sf)
library(dplyr)
library(terra)
library(exactextractr)

# Clean the streams away from surface friction dataset
clean_stream = TRUE

tif_path <-'./data/GRID3_COD_mix_travel_time_friction_surface_v1/GRID3_COD_mix_travel_time_friction_surface_v1.tif' 
mix_time=rast(tif_path)

grid = read_sf("./data/grid_surface.shp")
grid = st_transform(grid,st_crs(mix_time))

# crop raster to grid bounding box
mix_time_crop <- crop(
  mix_time,
  vect(grid),
  filename = "./data/mix_crop.tif",
  overwrite = TRUE
)

if (clean_stream){ # clean all streams -> values above 2
  
  mix_time_crop_wo_streams = mix_time_crop
  vals = values(mix_time_crop_wo_streams)
  values(mix_time_crop_wo_streams) <- ifelse(vals > 2, NA,vals)
  summary <- exactextractr::exact_extract(mix_time_crop_wo_streams, grid, 'mean')
  grid$mix_time_mean <- summary
  
  waters = grid[which(grid$surface=="water" ),]
  summary_waters <- exactextractr::exact_extract(mix_time_crop, waters, 'mean')
  grid[which(grid$surface=="water" ),]$mix_time_mean <- summary_waters
  
}else{
  summary <- exactextractr::exact_extract(mix_time_crop, grid, 'mean')
  grid$mix_time_mean <- summary
}

grid$lg_mix_time_mean = log(grid$mix_time_mean)



# 2 seems to be already very good
#threshold <- 2
# 1 is incorporating a bit too  much
# threshold <- 1
# plot(mix_time_crop)
# 
# # 770 1st quartile
# threshold <- 2
# vals = values(mix_time_crop)
# highlight <- mix_time_crop
# #highlight[vals < threshold ] <- NA
# # yes2 is generally a good cutoff, things between    and 2 , are not rivers but sth in between
# highlight[((vals < 1) | (vals >2)) ] <- NA
# 
# highlight = terra::project(highlight, "EPSG:4326")
#highlight[((vals < threshold) & (vals > 10))] <- NA



# center = st_transform(st_centroid(st_combine(grid)),4326)
# coords <- st_coordinates(center)
# 

# grid_wo_water = grid[which(grid$surface!="water"),]
# pal <- leaflet::colorNumeric(
#   palette = "viridis",
#   domain = grid_wo_water$lg_mix_time_mean,
#   na.color = "transparent"
# )
# pal_rev <- leaflet::colorNumeric(
#   palette = "viridis",
#   domain = grid_wo_water$lg_mix_time_mean,
#   na.color = "transparent",
#   reverse = T
# )
# 
# 
# library(leaflet)
# leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
#   addTiles() %>%
#   addPolygons(
#     data = st_transform(grid_wo_water, 4326),
#     fillColor = ~pal(grid_wo_water$lg_mix_time_mean),
#     fillOpacity = 1,
#     color = "black",
#     weight = 1
#   ) %>%
#   addLegend(
#     pal = pal_rev,
#     values = sort(grid_wo_water$lg_mix_time_mean,decreasing =T),
#     title = "Avg. Log Travel Time in s/m",
#     position = "bottomright",
#     labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
#   )# %>%
#   # setView(
  #   lng = coords[1],
  #   lat = coords[2],
  #   zoom = 8
  # )


# ============================================================
# 2.1 interpolate missing mix time
# ============================================================

# missing mix_time mean is always in water (in the big lakes). therefore fille it up with the value for water, of the lake 
# we have data for.

trimmed_lakes = read_sf("./data/Congo_relevant_lakes.shp")
trimmed_lakes = st_transform(trimmed_lakes,st_crs(grid))

# this is the lake with values for mixed_time
trimmed_lakes = trimmed_lakes[which(trimmed_lakes$name =="Lac Kivu"),]
lac_kivu_mixed_time <- exactextractr::exact_extract(mix_time_crop, trimmed_lakes, 'mean')
lac_kivu_mixed_time = mean(lac_kivu_mixed_time)
grid[which(is.na(grid$mix_time_mean)),]$mix_time_mean = lac_kivu_mixed_time

grid$lg_mix_time_mean  = log(grid$mix_time_mean)



# ============================================================
# 2.2 write data
# ============================================================

grid = st_drop_geometry(grid)
data.table::fwrite(grid[,c("cell_id","mix_time_mean","lg_mix_time_mean")],"./data/grid_mix_time.csv")


# ============================================================
# 3 investigate other dataset
# Problem here is that rivers are considered -> motorized -> boat travel
# is possible
# ============================================================
# 
# setwd("D:/DRC/gaussian_process_AOC")
# 
# library(sf)
# library(dplyr)
# library(terra)
# 
# 
# 
# tif_path <-'./data/2020_motorized_friction_surface/2020_motorized_friction_surface.geotiff' 
# motorized=rast(tif_path)
# 
# grid = read_sf("./data/grid_surface.shp")
# grid = st_transform(grid,st_crs(motorized))
# 
# # crop raster to grid bounding box
# motorized_crop <- crop(
#   motorized,
#   vect(grid)
# )
# 
# summary <- exactextractr::exact_extract(motorized_crop, grid, 'mean')
# grid$motorized_speed = summary
# grid$lg_motorized_speed = log(summary)
# # plot(motorized_crop)
# # plot(grid[,"motorized_speed"])
# # plot(grid[,"lg_motorized_speed"])
# # plot(grid[which(grid$surface=="water"),"lg_motorized_speed"])
# # plot(grid[,"lg_motorized_speed"])
# # plot(grid[which(grid$name=="Nord-Kivu"),"motorized_speed"])
# 
# 
# motorized_crop_small <- crop(
#   motorized_crop,
#   vect(grid[which(grid$name=="Nord-Kivu"),])
# )
# 
# 
# mix_time_crop_small <- crop(
#   mix_time_crop,
#   vect(grid[which(grid$name=="Nord-Kivu"),]),
# )
# 
# 
# plot(log(mix_time_crop_small))
# plot(motorized_crop_small)
# plot(grid[which(grid$name=="Nord-Kivu"),"lg_mix_time_mean"])
# plot(grid[which(grid$name=="Nord-Kivu"),"lg_motorized_speed"])
# 
# 
# tif_path <-'./data/202001_Global_Walking_Only_Friction_Surface_2019/202001_Global_Walking_Only_Friction_Surface_2019.tif' 
# walking_speed=rast(tif_path)
# 
# 
# grid = st_transform(grid,st_crs(walking_speed))
# 
# # crop raster to grid bounding box
# walking_speed_crop <- crop(
#   walking_speed,
#   vect(grid)
# )
# plot(walking_speed_crop)
# 
# diff <- motorized_crop - walking_speed_crop
# diff[diff< 0.048] <- NA
# plot(diff)
# 
# plot(grid[which(grid$surface=="water"),],add =T ,col ="red")
# 
# 
# plot(diff)
