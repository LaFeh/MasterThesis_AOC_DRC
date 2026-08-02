# 00_02 clean travelspeed of streams


# travelspeed data

library(sf)
library(dplyr)
library(terra)
library(exactextractr)

# Clean the streams away from surface friction dataset
clean_stream = TRUE

tif_path <-'./data/GRID3_COD_mix_travel_time_friction_surface_v1/GRID3_COD_mix_travel_time_friction_surface_v1.tif' 
mix_time=rast(tif_path)

grid = read_sf(paste0("./data/base_provinces.shp"))
grid = st_transform(grid,st_crs(mix_time))

# crop raster to grid bounding box
mix_time_crop <- crop(
  mix_time,
  vect(grid),
  filename = "./data/mix_crop.tif",
  overwrite = TRUE
)

rm(mix_time)

if (clean_stream){
  
  gpkg_path <- "./data/congo-democratic-republic-260530-free.gpkg/congo-democratic-republic.gpkg"
  
  water2 <- st_read(gpkg_path, layer = "gis_osm_waterways_free") |>
    st_transform(st_crs(grid)) |>
    st_crop(st_bbox(grid)) |> 
    dplyr::filter(fclass =="drain" | fclass == "stream")
  
  #st_crs(water2)$units
  water2 <- st_buffer(water2,dist = 30/2)
  streams <- vect(water2)
  
  # Save where the original NAs are
  orig_na <- is.na(mix_time_crop)
  # Mask out polygons (set cells inside polygons to NA)
  r_masked <- mask(mix_time_crop,streams, inverse = TRUE)
  
  # Cells that became NA because of masking
  masked_na <- is.na(r_masked) & !orig_na
  
  
  r_fill <- r_masked
  target <- which(values(masked_na))
  
  
  repeat {
    
    vals <- values(r_fill)
    
    remaining <- target[is.na(vals[target])]
    if (length(remaining) == 0) break
    print(length(remaining))
    
    adj <- adjacent(r_fill, remaining, directions = 8, pairs = TRUE)
    
    # neighbour values
    nbr_vals <- vals[adj[,2]]
    
    # mean of neighbours for each target cell
    m <- tapply(nbr_vals, adj[,1], mean, na.rm = TRUE)
    
    vals[as.integer(names(m))] <- m
    
    values(r_fill) <- vals
  }
}


# ============================================================
# 2.2 write data
# ============================================================

writeRaster(r_fill,"./data/travel_time_friction_surface_removed_streams.tif",overwrite = T)

