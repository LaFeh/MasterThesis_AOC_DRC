# travelspeed data

# %%
library(sf)
library(dplyr)
library(terra)

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

tif_path <-'./data/GRID3_COD_mix_travel_time_friction_surface_v1/GRID3_COD_mix_travel_time_friction_surface_v1.tif' 
mix_time= rast("D:/DRC/gaussian_process_AOC/data/GRID3_COD_walk_travel_time_friction_surface_v1/GRID3_COD_walk_travel_time_friction_surface_v1.tif")
#mix_time=rast(tif_path)

grid = read_sf("./data/grid_surface.shp")
grid = st_transform(grid,st_crs(mix_time))

# crop raster to grid bounding box
mix_time_crop <- crop(
  mix_time,
  vect(grid),
  filename = "./data/mix_crop.tif",
  overwrite = TRUE
)
rm(mix_time)

if (clean_stream){ # clean all streams -> values above 2
  

  gpkg_path <- "./data/congo-democratic-republic-260530-free.gpkg/congo-democratic-republic.gpkg"

  water3 <- st_read(gpkg_path, layer = "gis_osm_waterways_free") |>
    st_transform(st_crs(grid)) |>  st_crop(st_bbox(grid))%>%filter(fclass =="stream")


  
  water3 <- st_buffer(water3,30)
  streams <- vect(water3)
  
  # Save where the original NAs are
  orig_na <- is.na(mix_time_crop)
  # Mask out polygons (set cells inside polygons to NA)
  r_masked <- mask(mix_time_crop,streams, inverse = TRUE)
  
  # Cells that became NA because of masking
  masked_na <- is.na(r_masked) & !orig_na
  
  r_fill <- r_masked

  target <- which(values(masked_na))
  vals <- values(r_fill)

# %%
  repeat {
    
    gc()
    remaining <- target[is.na(vals[target])]
    if (length(remaining) == 0) break
    print(length(remaining))
    
    adj <- adjacent(r_fill, remaining, directions = 8, pairs = TRUE)
    
    # neighbour values
    nbr_vals <- vals[adj[,2]]
    
    # mean of neighbours for each target cell
    m <- tapply(nbr_vals, adj[,1], mean, na.rm = TRUE)
    
    #vals[as.integer(names(m))] <- m
    gc()
    
    vals[as.integer(names(m))] <- m
    

  }
  
  values(mix_time_crop) <-  vals

summary <- exactextractr::exact_extract(mix_time_crop, grid, 'mean')
  grid$mix_time_mean <- summary

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
grid$lg_mix_time_mean  = log(grid$mix_time_mean)


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
