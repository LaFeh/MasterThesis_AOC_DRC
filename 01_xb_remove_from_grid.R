#01_xb_remove_from_grid


remove_from_grid <- function(grid,to_be_removed){
  
  library(sf)
  library(terra)
  

  intersects_idx  <- unique(unlist(st_intersects(to_be_removed, grid)))
  grid_touches    <- grid[intersects_idx, ]
  grid_no_touch   <- grid[-intersects_idx, ]
  cat("grid divided into part that is close to any street and part that is not")
  

  
  # Use a consistent precision BEFORE conversion
  #grid_touches <- st_snap_to_grid(grid_touches, size = 0.01)
  #to_be_removed <- st_snap_to_grid(to_be_removed, size = 0.01)
  
  #grid_touches <- st_make_valid(grid_touches)
  #to_be_removed <- st_make_valid(to_be_removed)
  
  grid_touches_terra <- terra::vect(grid_touches)
  to_be_removed_terra <- terra::vect(to_be_removed)
  
  # dissolve removal geometries
  to_be_removed_terra <- terra::aggregate(
    to_be_removed_terra,
    dissolve = TRUE
  )
  
  cat("dataset to be removed is not aggregated")
  
  grid_without_terra <- terra::erase(
    grid_touches_terra,
    to_be_removed_terra
  )
  cat("dataset to be removed is removed from grid")
  
  grid_without <- st_as_sf(grid_without_terra)
  
  grid_without <- rbind(
    grid_no_touch,
    grid_without
  )
  
  grid_without <- grid_without[!st_is_empty(grid_without), ]
  
  # # Convert to terra, make valid, erase
  # grid_touches_terra <- makeValid(vect(grid_touches))
  # to_be_removed_terra        <- makeValid(vect(to_be_removed))
  # to_be_removed_terra        <- disagg(to_be_removed_terra)
  # to_be_removed_terra        <- aggregate(to_be_removed_terra, dissolve = TRUE)
  # to_be_removed_terra        <- makeValid(to_be_removed_terra)
  # 
  # grid_without_terra <- erase(grid_touches_terra, to_be_removed_terra)
  # grid_without_terra <- disagg(grid_without_terra)
  # # Convert back and recombine
  # grid_without <- st_as_sf(grid_without_terra)
  # grid_without <- rbind(grid_no_touch, grid_without)
  # grid_without <- grid_without[!st_is_empty(grid_without), ]
  
  return(grid_without)

  
}
