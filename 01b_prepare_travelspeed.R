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
clean_stream = TRUE

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
grid$mix_time_mean <- summary
grid$lg_mix_time_mean = log(grid$mix_time_mean)



library(sf)
library(leaflet)

# Keep only rows where mix_time_mean is NA
grid_na <- grid[is.na(grid$mix_time_mean), ]
st_geometry(grid_na)
# Plot
leaflet() |>
  addTiles() |>
  addPolygons(
    sf::st_transform(grid_na, 4326),
    color = "red",
    weight = 1,
    fillColor = "red",
    fillOpacity = 0.6,
    popup = ~paste("Row:", seq_len(nrow(grid_na)))
  )


# graphics::boxplot(lg_mix_time_mean~surface,grid)
# graphics::boxplot(mix_time_mean~surface,grid)

# 
# library(leaflet)
# leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
#   addTiles() %>%
#   addPolygons(
#     data = st_transform(grid, 4326),
#     fillColor = ~pal(grid$lg_mix_time_mean),
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

# library(leaflet)
# leaflet(options = leafletOptions(zoomControl = T)) %>%
#   addTiles() %>%
#   addPolygons(
#     data = st_transform(grid[which(grid$name =="Nord-Kivu"),], 4326),
#     color = "black",
#     weight = 1
#   ) %>%
#   addRasterImage(project(log(mix_time_crop),"EPSG:4326"),
#                  opacity =0.5)

# ============================================================
# 2.2 write data
# ============================================================
length(unique(grid$cell_id))==nrow(grid)

grid = st_drop_geometry(grid)
data.table::fwrite(grid[,c("cell_id","mix_time_mean","lg_mix_time_mean")],"./data/grid_mix_time.csv")
