#05a run model

library(Matrix)
source("./00b_helper_create_grid_name.R")
source("./05_model/05a_a_helper_run_model.R")
source("./05_model/05a_b_helper_parameter_grid.R")

source("./04b_a_helper_functions.R")

## prepare data






for (model_name in names(parameter_grid)){
  
  
  
  print(model_name)
  name_parameter_grid = model_name
  get_parameter_from_grid(parameter_grid,name_parameter_grid)
  
  
  covariates_list = create_covariates_list()
  model_covariates = covariates_list[[model_covariates_title]]
  
  
  model_other_name = ""
  #directory for saving all the output

  output_path = paste0("./05_model/model_",name_parameter_grid)
  dir.create(output_path)
  dir.create(paste0(output_path,"/plots"))
  
  
  
  name_of_grid = create_grid_name(add_waterways = grid_add_waterways,
                                  add_nationalparks = grid_add_nationalparks,
                                  add_streets = grid_add_streets,
                                  cell_size = grid_cell_size)
  
  name_of_grid_clean = gsub(".shp","",name_of_grid)
  
  path_of_adjacency_matrix = paste0("./data/data_for_prediction/mat_w_mixedtime_neighbour",
                                    model_degree_of_neighbour,
                                    "_distance_", model_bol_distance,
                                    "_",model_bol_border,"_",name_of_grid_clean,".RData")
  
  
  path_of_eigenvalue = paste0("./05_model/eigvalues_mat_w_mixedtime_",
                              model_degree_of_neighbour,
                              "_distance_", model_bol_distance,
                              "_",name_of_grid_clean,".RData")
  

  
  
  # if(file.exists(paste0(output_path,"/plots/202405_phi.png"))){
  #   next()
  # }
  
  
  ##########################################

  name_of_grid_file_name = gsub(".shp","",name_of_grid)
  data_file_name  = paste0("./data/data_for_prediction/",name_of_grid_file_name,"/",model_dates_to_run,"_events.RData")

  if (!any(file.exists(data_file_name))){
    
    
    data_path_to_be_read = paste0("./data/frontline_data_all_mnths_",name_of_grid_file_name,".RData")
    load(data_path_to_be_read)
    
    frontline_data = frontline_data_controle_num_all_previous_time
    frontline_data = frontline_data[order(frontline_data$cell_id),]
    rm(frontline_data_controle_num_all_previous_time)
    
    frontline_data$control_binom = frontline_data$control
    frontline_data$control_binom = ifelse(frontline_data$control_binom ==0.5,0,frontline_data$control_binom)
    
    all_dates = model_dates_to_run
    all_dates =  paste0(all_dates, "01")
    all_dates = as.Date(all_dates,format = "%Y%m%d")
    N = length(unique(frontline_data$cell_id))
    
    for(date in all_dates){
      print(as.Date(date))
      prepare_data_for_prediction(frontline_data,date,N,name_of_grid_file_name)
    }
  
  }
  
  load(data_file_name)


  
  
  
  if(!file.exists(path_of_adjacency_matrix)){
    
    message(paste0(path_of_adjacency_matrix," doesnt exist! It is being calcuated now"))
    prepare_adj_matrix_for_prediction(data,
                                      N = length(unique(data$cell_id)),
                                      date = zoo::as.yearmon(model_dates_to_run[1],format = "%Y%m"),
                                      snap = 10,
                                      bol_second_degree_neighbours = ifelse(model_degree_of_neighbour=="first",FALSE,TRUE),
                                      bol_distance = model_bol_distance,
                                      border_distance = model_bol_border,
                                      FUN = dist_in_km,
                                      name_of_grid)
    
    message("adjacency matris was successfully calculated!")
  }
  
  data_mat_w <- readRDS(path_of_adjacency_matrix)

  run_model_wrapper(data,
                    data_mat_w,
                    model_covariates,
                    date = model_dates_to_run,
                    output_path = output_path,
                    path_of_eigenvalue = path_of_eigenvalue)
  
  
  
  fileConn <- file(paste0(output_path, "/settings.txt"), open = "a")
  writeLines("\n\n\n", fileConn)
  writeLines(as.character(parameter_grid[name_parameter_grid]), fileConn)
  close(fileConn)
  
  
  
  
}






  


