create_grid_name <- function(add_streets,add_nationalparks,add_waterways,cell_size){
  


  if(add_streets){
    grid_name_street = "_street"
  }else {
    grid_name_street = ""
  }
  
  if(add_nationalparks){
    grid_name_park = "_park"
  }else {
    grid_name_park = ""
  }
  
  if(add_waterways){
    grid_name_water = "_water"
  }else {
    grid_name_water = ""
  }
  
  name_of_grid = paste0("grid_surface_",cell_size,
                        grid_name_water,
                        grid_name_park,
                        grid_name_street,".shp")
  
  
  
  
  return(name_of_grid)
}