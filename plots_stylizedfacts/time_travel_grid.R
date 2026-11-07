# time travel grid
library(dplyr)
library(maptiles)
library(ggplot2)

grid = read_sf(paste0("./data/grid_surface_3000_water_park_street.shp"))

#### mix time walk time ########################
grid_mix_time = data.table::fread("./data/grid_mix_time.csv",sep =",")
grid = left_join(grid,grid_mix_time,by ="cell_id")%>%filter(name =="Nord-Kivu")

rm(grid_mix_time)

bbox = st_boundary(grid)

basemap <- get_tiles(
  x = bbox,
  provider = "Esri.WorldGrayCanvas"
)


ggplot() +
  tidyterra::geom_spatraster_rgb(data = basemap) +
  geom_sf(data =grid, aes(fill = lg_mix_time_mean),lwd = 0)+
  # geom_sf(
  #   data = cod_boundaries%>%st_transform("EPSG:3857"),
  #   fill = NA,
  #   color = "white",
  #   linewidth = 0.5
  # ) +
  scale_fill_viridis_c(
    option = "magma",
    name = "log Travel Time\nin s/m",
    na.value = NA
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "right",
    strip.background = element_blank(),
  ) #+ ggsave(filename = "~/MasterThesis_AOC_DRC/no_stream.png")
# time travel with streams:


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

grid = read_sf(paste0("./data/","grid_surface_3000_water_park_street.shp"))%>%filter(name =="Nord-Kivu")

grid = st_transform(grid,st_crs(mix_time))

# crop raster to grid bounding box
mix_time_crop <- crop(
  mix_time,
  vect(grid)
)
rm(mix_time)

summary <- exactextractr::exact_extract(mix_time_crop, grid, 'mean')
grid$mix_time_mean <- summary
grid$lg_mix_time_mean = log(grid$mix_time_mean)

rm(grid_mix_time)

bbox = st_boundary(grid)

basemap <- get_tiles(
  x = bbox,
  provider = "Esri.WorldGrayCanvas"
)


ggplot() +
  tidyterra::geom_spatraster_rgb(data = basemap) +
  geom_sf(data =grid, aes(fill = lg_mix_time_mean),lwd = 0)+
  # geom_sf(
  #   data = cod_boundaries%>%st_transform("EPSG:3857"),
  #   fill = NA,
  #   color = "white",
  #   linewidth = 0.5
  # ) +
  scale_fill_viridis_c(
    option = "magma",
    name = "log Travel Time\nin s/m",
    na.value = NA
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "right",
    strip.background = element_blank(),
  ) + ggsave(filename="~/MasterThesis_AOC_DRC/stream.png")

##


grid_mix_time =data.table::fread("./data/grid_mix_time.csv")
grid = left_join(grid,grid_mix_time,by ="cell_id")%>%filter(name =="Nord-Kivu")

plot(grid[,c("lg_mix_time_mean.x")])
plot(grid[,c("lg_mix_time_mean_weiss.y")])
