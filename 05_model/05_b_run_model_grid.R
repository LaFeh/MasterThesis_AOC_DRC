#05a run model

library(Matrix)
source("./00b_helper_create_grid_name.R")
source("./05_model/05a_a_helper_run_model_loro.R")
source("./05_model/05a_b_helper_parameter_grid.R")

source("./04b_a_helper_functions.R")

## prepare data

# Set to TRUE to run leave-one-region-out CV (restricted to observed regions)
# for every model in the grid. This is the recommended way to compare models
# across the grid (different rho, different covariate sets, etc.) since it
# evaluates genuine held-out predictive performance rather than an
# in-sample fit criterion. It refits the model once per observed region, so
# expect this to multiply total runtime by roughly length(obs_idx) per model.
RUN_LORO_CV <- FALSE

# collects one row of CV summary stats per grid model, for the final
# cross-model comparison table
grid_cv_summary <- list()



all_months = c(paste0("0",1:9),10:12)
all_years = c(2024,2025)
all_dates= c(paste0(all_years[1], all_months),paste0(all_years[2], all_months))
#parameter_grid = parameter_grid[grepl("estimaterho",names(parameter_grid))]
#parameter_grid = parameter_grid[names(parameter_grid) != "wostreets_first_degree_no_dist_cov_lead_events_fatalities_estimaterho"]
for (model_name in names(parameter_grid)){
  
  
  
  print(model_name)
  name_parameter_grid = model_name
  get_parameter_from_grid(parameter_grid,name_parameter_grid)
  
  
  covariates_list = create_covariates_list()
  model_covariates = covariates_list[[model_covariates_title]]
  
  
  model_other_name = ""
  #directory for saving all the output

  
  
  
  name_of_grid = create_grid_name(add_waterways = grid_add_waterways,
                                  add_nationalparks = grid_add_nationalparks,
                                  add_streets = grid_add_streets,
                                  cell_size = grid_cell_size)
  
  name_of_grid_clean = gsub(".shp","",name_of_grid)
  
  
  output_path = paste0("./05_model/model_",name_parameter_grid,"_",name_of_grid_clean)
  dir.create(output_path)
  dir.create(paste0(output_path,"/plots"))
  
  path_of_adjacency_matrix = paste0("./data/data_for_prediction/mat_w_mixedtime_neighbour",
                                    model_degree_of_neighbour,
                                    "_distance_", model_bol_distance,
                                    "_",model_bol_border,"_",name_of_grid_clean,".RData")
  
  
  path_of_eigenvalue = paste0("./05_model/eigvalues_mat_w_mixedtime_",
                              model_degree_of_neighbour,
                              "_distance_", model_bol_distance,
                              "_",name_of_grid_clean,".RData")
  
  
  
 
  
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
  
  estimate_rho = list("model_rho" =  model_rho,
                      "model_logit_rho_prior_mean" = model_logit_rho_prior_mean,
                      "model_logit_rho_prior_sd" = model_logit_rho_prior_sd
                      )
  estimate_rho = estimate_rho[sapply(estimate_rho,function(x) !is.null(x))]
  
  run_model_wrapper(data,
                    data_mat_w,
                    model_covariates,
                    estimate_rho = estimate_rho,
                    date = model_dates_to_run,
                    output_path = output_path,
                    path_of_eigenvalue = path_of_eigenvalue,
                    run_if_exists = FALSE)
  

  
  fileConn <- file(paste0(output_path, "/settings.txt"), open = "a")
  writeLines("\n\n\n", fileConn)
  writeLines(as.character(parameter_grid[name_parameter_grid]), fileConn)
  close(fileConn)
  
  
  
  
}




