# travelspeed data

# ============================================================
# 2. travel speed (motorized)
# ============================================================

## start with mixed travel speed
#setwd("D:/DRC/gaussian_process_AOC")

library(sf)
library(dplyr)
library(terra)
library(exactextractr)

# Clean the streams away from surface friction dataset
clean_stream = T

if(clean_stream){
  tif_path <-'./data/travel_time_friction_surface_removed_streams.tif' 
} else {
  tif_path <-'./data/GRID3_COD_mix_travel_time_friction_surface_v1/GRID3_COD_mix_travel_time_friction_surface_v1.tif' 
}

mix_time=rast(tif_path)

grid = read_sf(paste0("./data/",name_of_grid))

grid = st_transform(grid,st_crs(mix_time))

# crop raster to grid bounding box
mix_time_crop <- crop(
  mix_time,
  vect(grid),
  filename = "./data/mix_crop.tif",
  overwrite = TRUE
)
rm(mix_time)



summary <- exactextractr::exact_extract(mix_time_crop, grid, 'mean')
rm(mix_time_crop)
grid$mix_time_mean <- summary
grid$lg_mix_time_mean = log(grid$mix_time_mean)


length(unique(grid$cell_id))==nrow(grid)


####################
# travel speed weiss
####################


library(sf)
library(dplyr)
library(terra)
library(exactextractr)

tif_path <-'./data/2020_motorized_friction_surface.geotiff' 
mix_time_weiss=rast(tif_path)

#grid = read_sf(paste0("./data/",name_of_grid))

grid = st_transform(grid,st_crs(mix_time_weiss))

# crop raster to grid bounding box
mix_time_crop_weiss <- crop(
  mix_time_weiss,
  vect(grid),
  filename = "./data/mix_crop_weiss.tif",
  overwrite = TRUE
)
rm(mix_time_weiss)



summary_weiss <- exactextractr::exact_extract(mix_time_crop_weiss, grid, 'mean')
grid$mix_time_mean_weiss <- summary_weiss
grid$lg_mix_time_mean_weiss = log(grid$mix_time_mean_weiss)

#grid$diff_weiss_lg_time = grid$lg_mix_time_mean_weiss-grid$lg_mix_time_mean


length(unique(grid$cell_id))==nrow(grid)

#########################
## travel speed GRUFS  ##
#########################


library(sf)
library(dplyr)
library(terra)
library(exactextractr)

tif_path <-'./data/2020_motorized_friction_surface.geotiff' 
mix_time_grufs=rast(tif_path)

#grid = read_sf(paste0("./data/",name_of_grid))

grid = st_transform(grid,st_crs(mix_time_grufs))

# crop raster to grid bounding box
mix_time_crop_grufs <- crop(
  mix_time_grufs,
  vect(grid),
  filename = "./data/mix_crop_grufs.tif",
  overwrite = TRUE
)
rm(mix_time_grufs)

lg_mix_time_crop_grufs=log(mix_time_crop_grufs)

summary_grufs <- exactextractr::exact_extract(mix_time_crop_grufs, grid, 'mean')
grid$mix_time_mean_grufs <- summary_grufs
grid$lg_mix_time_mean_grufs = log(grid$mix_time_mean_grufs)


# 
# plot(grid[,c("lg_mix_time_mean_grufs")])

# ggplot()+geom_sf(data = grid, aes(fill =lg_mix_time_mean_grufs),lwd = 0)+ggsave("grufs.png")
# ggplot()+geom_sf(data = grid, aes(fill =lg_mix_time_mean),lwd = 0)+ggsave("ciesing.png")

grid = st_drop_geometry(grid)

data.table::fwrite(grid[,c("cell_id","mix_time_mean","lg_mix_time_mean",
                           "mix_time_mean_weiss","lg_mix_time_mean_weiss",
                           "mix_time_mean_grufs","lg_mix_time_mean_grufs")],"./data/grid_mix_time.csv")

# ggplot()+geom_spatraster(data = lg_mix_time_crop_grufs)+ggsave("GRUFS_raw.png")
#grid$diff = grid$mix_time_mean_weiss-grid$mix_time_mean
#grid$lg_diff = log(grid$mix_time_mean_weiss-grid$mix_time_mean)
#grid = grid%>%left_join(grid2,by=c("cell_id"))
#plot(grid[,c("lg_diff")])

# boxplot(grid$mix_time_mean, grid$mix_time_mean_weiss)
# 
# ggplot(grid)+
#   geom_sf(aes(fill=lg_mix_time_mean_weiss),lwd = 0)
# 
# ggplot(grid)+
#   geom_sf(aes(fill=lg_mix_time_mean),lwd = 0)
