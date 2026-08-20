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
  
  run_model_wrapper_loro(data,
                    data_mat_w,
                    model_covariates,
                    date = model_dates_to_run,
                    output_path = output_path,
                    path_of_eigenvalue = path_of_eigenvalue,
                    do_loro_cv = RUN_LORO_CV)
  
  
  # pull the CV summary that run_model_wrapper() / run_model() just saved
  # (if requested) and stash it for the final cross-model table
  if (RUN_LORO_CV) {
    cv_file <- paste0(output_path,"/",as.character(model_dates_to_run),"_loro_cv.RData")
    if (file.exists(cv_file)) {
      load(cv_file)   # loads loro_df, cv_summary
      grid_cv_summary[[model_name]] <- data.frame(
        model_name      = model_name,
        n_folds         = cv_summary$n_folds,
        n_converged     = cv_summary$n_converged,
        elpd            = cv_summary$elpd,
        mean_log_score  = cv_summary$mean_log_score,
        brier           = cv_summary$brier,
        auc             = ifelse(is.null(cv_summary$auc), NA, cv_summary$auc),
        baseline_elpd            = cv_summary$baseline_elpd,
        baseline_mean_log_score  = cv_summary$baseline_mean_log_score,
        baseline_brier           = cv_summary$baseline_brier,
        log_score_gain_vs_baseline = cv_summary$log_score_gain_vs_baseline,
        brier_skill_score          = cv_summary$brier_skill_score
      )
    } else {
      warning(paste("No LORO-CV file found for", model_name))
    }
  }
  
  
  fileConn <- file(paste0(output_path, "/settings.txt"), open = "a")
  writeLines("\n\n\n", fileConn)
  writeLines(as.character(parameter_grid[name_parameter_grid]), fileConn)
  close(fileConn)
  
  
  
  
}


# ── Cross-model comparison table ──────────────────────────────────────────────
# Rank grid models by held-out predictive performance. Higher ELPD / mean log
# score is better; lower Brier is better.
if (RUN_LORO_CV && length(grid_cv_summary) > 0) {
  
  grid_cv_table <- do.call(rbind, grid_cv_summary)
  grid_cv_table <- grid_cv_table[order(-grid_cv_table$elpd), ]
  
  print(grid_cv_table)
  
  write.csv(grid_cv_table, "./05_model/grid_loro_cv_comparison.csv", row.names = FALSE)
  save(grid_cv_table, file = "./05_model/grid_loro_cv_comparison.RData")
  
  cat("\nBest model by LORO-CV ELPD:", grid_cv_table$model_name[1], "\n")
}



