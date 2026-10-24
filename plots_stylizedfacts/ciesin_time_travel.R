# ciesin_time_travel


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