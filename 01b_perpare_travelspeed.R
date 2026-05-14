# settlement data
setwd("D:/DRC/gaussian_process_AOC")

library(sf)
library(dplyr)
library(terra)

tif_path <-'./data/GRID3_COD_walk_travel_time_friction_surface_v1/GRID3_COD_walk_travel_time_friction_surface_v1.tif' 
walk_time=rast(tif_path)


grid = read_sf("./data/grid_surface.shp")
# thsi is much faster than the other way
grid = st_transform(grid,st_crs(walk_time))
# transform grid to raster CRS


 # crop raster to grid bounding box
walk_time_crop <- crop(
  walk_time,
  vect(grid),
  #filename = "./data/walk_crop.tif",
  #overwrite = TRUE
)
rm(walk_time)




library(exactextractr)

grid$walk_time_mean <- exact_extract(
  walk_time_crop,
  grid,
  'mean'
)

grid$lg_walk_time_mean = log(grid$walk_time_mean)

##########################
bbox = st_as_sf(st_as_sfc(st_bbox(grid[440:443,])))
walk_time_crop <- crop(
  walk_time,
  bbox,
  #filename = "./data/walk_crop.tif",
  #overwrite = TRUE
)
plot(walk_time_crop)
# 
# plot(grid[435:445,]$geometry,add =T)
# tile = tiles[2419]
# grid$mean_walk_time <- sapply(tiles, function(tile) {
#   walk_time_tile=rast(tile)
#   mean_walk_time = mean(values(walk_time_tile),na.rm=T)
#   return(mean_walk_time)
# })

##################
grid = st_drop_geometry(grid)


data.table::fwrite(grid[,c("cell_id","walk_time_mean","lg_walk_time_mean")],"./data/grid_walk_time.csv")


#write_sf(grid,"./data/grid_walk_time.shp",overwrite = T)

## start with mixed travel speed
setwd("D:/DRC/gaussian_process_AOC")

library(sf)
library(dplyr)
library(terra)
library(exactextractr)

tif_path <-'./data/GRID3_COD_mix_travel_time_friction_surface_v1/GRID3_COD_mix_travel_time_friction_surface_v1.tif' 
mix_time=rast(tif_path)



grid = read_sf("./data/grid_surface.shp")

# thsi is much faster than the other way
grid = st_transform(grid,st_crs(mix_time))
# transform grid to raster CRS

# crop raster to grid bounding box
mix_time_crop <- crop(
  mix_time,
  vect(grid),
  filename = "./data/mix_crop.tif",
  overwrite = TRUE
)
####
# bbox = st_as_sf(st_as_sfc(st_bbox(grid[440:443,])))
# mix_time_crop_plot <- crop(
#   walk_time,
#   bbox,
# )
# plot(mix_time_crop_plot)

####

summary <- exactextractr::exact_extract(mix_time_crop, grid, 'mean')

grid$mix_time_mean <- summary

grid$lg_mix_time_mean  = log(grid$mix_time_mean)
plot(grid[,c("lg_mix_time_mean")])

grid = st_drop_geometry(grid)


data.table::fwrite(grid[,c("cell_id","mix_time_mean","lg_mix_time_mean")],"./data/grid_mix_time.csv")



plot(grid[,c("mix_time_mean")])
grid$lg_bldng_r = log(grid$bldng_r)
plot(grid[,c("lg_bldng_r")])

#########


grid_mix_time = data.table::fread("./data/grid_mix_time.csv",sep =",")

grid_walk_time = data.table::fread("./data/grid_walk_time.csv",sep =",")

grid = left_join(grid_mix_time,grid_walk_time, by ="cell_id")
