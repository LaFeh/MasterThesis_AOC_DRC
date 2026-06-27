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
  vect(grid)
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

plot(log(mix_time_crop))

####
# bbox = st_as_sf(st_as_sfc(st_bbox(grid[440:443,])))
# mix_time_crop_plot <- crop(
#   walk_time,
#   bbox,
# )
#plot(log(mix_time_crop))

####

summary <- exactextractr::exact_extract(mix_time_crop, grid, 'mean')


grid$mix_time_mean <- summary


#plot(grid[,c("mix_time_mean")])
# plot(grid[,c("lg_mix_time_mean")])
# 
center = st_transform(st_centroid(st_combine(grid)),4326)
coords <- st_coordinates(center)

grid$lg_mix_time_mean = log(grid$mix_time_mean)

pal <- leaflet::colorNumeric(
  palette = "viridis",
  domain = grid$lg_mix_time_mean,
  na.color = "transparent"
)
pal_rev <- leaflet::colorNumeric(
  palette = "viridis",
  domain = grid$lg_mix_time_mean,
  na.color = "transparent",
  reverse = T
)

library(leaflet)
leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  addTiles() %>%
  addPolygons(
    data = st_transform(grid, 4326),
    fillColor = ~pal(grid$lg_mix_time_mean),
    fillOpacity = 1,
    color = "black",
    weight = 1
  ) %>%
  addLegend(
    pal = pal_rev,
    values = sort(grid$lg_mix_time_mean,decreasing =T),
    title = "Avg. Log Travel Time in s/m",
    position = "bottomright",
    labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
  ) %>%
  setView(
    lng = coords[1],
    lat = coords[2],
    zoom = 8
  )




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

plot(grid[,c("lg_mix_time_mean")])


# ============================================================
# 2.2 write data
# ============================================================

grid = st_drop_geometry(grid)
data.table::fwrite(grid[,c("cell_id","mix_time_mean","lg_mix_time_mean")],"./data/grid_mix_time.csv")



# grid_mix_time = data.table::fread("./data/grid_mix_time.csv",sep =",")
# grid_walk_time = data.table::fread("./data/grid_walk_time.csv",sep =",")
# grid = left_join(grid_mix_time,grid_walk_time, by ="cell_id")
