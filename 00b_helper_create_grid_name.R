create_grid_name <- function(add_streets,
                             add_nationalparks,
                             add_waterways,
                             cell_size,street_size){
  


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
  
  if(add_streets){
    if(street_size !=""){
      street_size = paste0("_",street_size)
    }
  }else {
    street_size =""
  }



  name_of_grid = paste0("grid_surface_",cell_size,
                        grid_name_water,
                        grid_name_park,
                        grid_name_street,street_size,".shp")
  
  
  
  
  return(name_of_grid)
}


create_final_data_name <- function(grid_name,
                                   quality){
  
  stopifnot(quality %in%c(T,F))
  
  
  final_data_name = paste0(gsub(".shp","",grid_name),"_",
                      quality)
  
  return(final_data_name)

  }


create_adjacency_name <- function(
                                  adj_traveltime,
                                  adj_neighbour_degree,
                                  adj_distance,
                                  adj_border
                                  ){
  
  
  stopifnot(adj_traveltime %in%c("ciesin","weiss","grufs"))
  stopifnot(adj_neighbour_degree %in%c("second","first"))
  stopifnot(adj_distance %in%c(T,F))
  stopifnot(adj_border %in%c(T,F))
  
  
  adjacency_name = paste0(adj_traveltime,"_",
                      adj_neighbour_degree,"_",
                      adj_distance,"_",
                      adj_border)
  
  
  
  return(adjacency_name)
}

create_model_name <- function(estimation_technique,
                              cov_title){
  
  
  stopifnot(estimation_technique %in%c("fxd","ma","free"))
  
  
  model_name = paste0(estimation_technique,"_",
                      cov_title)
  


  return(model_name)
}




