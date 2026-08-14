#o5b_helper_parameter_grid

create_covariates_list<-function(){
  
  
  covariates_list = list()
  covariates_list[["noIntercept"]] = as.formula("~-1")
  covariates_list[["Intercept"]] = as.formula("~1")
  covariates_list[["total_lag_lead_events_fatalities"]] =  as.formula("~ total_events+total_events_lag+total_events_lead+total_fatalities+total_fatalities_lead+total_fatalities_lag")
  covariates_list[["lead_events_fatalities"]] =  as.formula("~ total_events_lead+total_fatalities_lead")
  
  
  
  return(covariates_list)
}


parameter_grid <- list()

add_to_parameter_grid <- function(
    name,
    grid_add_streets,
    grid_add_nationalparks,
    grid_add_waterways,
    grid_cell_size,
    model_dates_to_run,
    model_degree_of_neighbour,
    model_bol_distance,
    model_bol_border,
    model_covariates_title,
    model_rho
) {
  
  list(
    name = name,
    grid = list(
      add_streets = grid_add_streets,
      add_nationalparks = grid_add_nationalparks,
      add_waterways = grid_add_waterways,
      cell_size = grid_cell_size
    ),
    model = list(
      dates_to_run = model_dates_to_run,
      degree_of_neighbour = model_degree_of_neighbour,
      bol_distance = model_bol_distance,
      bol_border = model_bol_border,
      covariates_title = model_covariates_title,
      rho = model_rho
    )
  )
}

parameter_grid <- list(
  streets_first_degree_no_dist_noIntercept = add_to_parameter_grid(
    name = "streets_first_degree_no_dist",
    grid_add_streets = TRUE,
    grid_add_nationalparks = TRUE,
    grid_add_waterways = TRUE,
    grid_cell_size = 5000,
    model_dates_to_run = c("202405"),
    model_degree_of_neighbour = "first",
    model_bol_distance = FALSE,
    model_bol_border = FALSE,
    model_covariates_title = "NoIntercept",
    model_rho = 0.6
  ),
  streets_first_degree_no_dist = add_to_parameter_grid(
    name = "streets_first_degree_no_dist",
    grid_add_streets = TRUE,
    grid_add_nationalparks = TRUE,
    grid_add_waterways = TRUE,
    grid_cell_size = 5000,
    model_dates_to_run = c("202405"),
    model_degree_of_neighbour = "first",
    model_bol_distance = FALSE,
    model_bol_border = FALSE,
    model_covariates_title = "Intercept",
    model_rho = 0.6
  ),
  streets_scnd_degree_dist = add_to_parameter_grid(
    name = "streets_scnd_degree_dist",
    grid_add_streets = TRUE,
    grid_add_nationalparks = TRUE,
    grid_add_waterways = TRUE,
    grid_cell_size = 5000,
    model_dates_to_run = c("202405"),
    model_degree_of_neighbour = "second",
    model_bol_distance = TRUE,
    model_bol_border = FALSE,
    model_covariates_title = "Intercept",
    
    model_rho = 0.6
  ),
  streets_first_degree_dist = add_to_parameter_grid(
    name = "streets_scnd_degree_dist",
    grid_add_streets = TRUE,
    grid_add_nationalparks = TRUE,
    grid_add_waterways = TRUE,
    grid_cell_size = 5000,
    model_dates_to_run = c("202405"),
    model_degree_of_neighbour = "first",
    model_bol_distance = TRUE,
    model_bol_border = FALSE,
    model_covariates_title = "Intercept",
    
    model_rho = 0.6
  ),
  wo_streets_scnd_degree_dist = add_to_parameter_grid(
    name = "wo_streets_scnd_degree_dist",
    grid_add_streets = F,
    grid_add_nationalparks = TRUE,
    grid_add_waterways = TRUE,
    grid_cell_size = 5000,
    model_dates_to_run = c("202405"),
    model_degree_of_neighbour = "second",
    model_bol_distance = TRUE,
    model_bol_border = FALSE,
    model_covariates_title = "Intercept",
    
    model_rho = 0.6
  ),
  wo_streets_first_degree_no_dist = add_to_parameter_grid(
    name = "wo_streets_first_degree_no_dist",
    grid_add_streets = F,
    grid_add_nationalparks = TRUE,
    grid_add_waterways = TRUE,
    grid_cell_size = 5000,
    model_dates_to_run = c("202405"),
    model_degree_of_neighbour = "first",
    model_bol_distance = FALSE,
    model_bol_border = FALSE,
    model_covariates_title = "Intercept",
    model_rho = 0.6
  ),
  wo_streets_first_degree_no_dist_cov_total_lag_lead_events_fatalities = add_to_parameter_grid(
    name = "wo_streets_first_degree_dist_cov_total_lag_lead_events_fatalities",
    grid_add_streets = F,
    grid_add_nationalparks = TRUE,
    grid_add_waterways = TRUE,
    grid_cell_size = 5000,
    model_dates_to_run = c("202405"),
    model_degree_of_neighbour = "first",
    model_bol_distance = TRUE,
    model_bol_border = FALSE,
    model_covariates_title = "total_lag_lead_events_fatalities",
    model_rho = 0.6
  ),
  streets_first_degree_no_dist_cov_total_lag_lead_events_fatalities = add_to_parameter_grid(
    name = "streets_first_degree_no_dist_cov_total_lag_lead_events_fatalities",
    grid_add_streets = TRUE,
    grid_add_nationalparks = TRUE,
    grid_add_waterways = TRUE,
    grid_cell_size = 5000,
    model_dates_to_run = c("202405"),
    model_degree_of_neighbour = "second",
    model_bol_distance = TRUE,
    model_bol_border = FALSE,
    model_covariates_title = "total_lag_lead_events_fatalities",
    model_rho = 0.6
  ),
  streets_scnd_degree_no_dist_cov_total_lag_lead_events_fatalities = add_to_parameter_grid(
    name = "streets_first_degree_no_dist_cov_total_lag_lead_events_fatalities",
    grid_add_streets = F,
    grid_add_nationalparks = TRUE,
    grid_add_waterways = TRUE,
    grid_cell_size = 5000,
    model_dates_to_run = c("202405"),
    model_degree_of_neighbour = "second",
    model_bol_distance = TRUE,
    model_bol_border = F,
    model_covariates_title = "total_lag_lead_events_fatalities",
    model_rho = 0.6
  ),
  wo_streets_first_degree_dist_cov_lead_events_fatalities = add_to_parameter_grid(
    name = "wo_streets_first_degree_dist_cov_lead_events_fatalities",
    grid_add_streets = F,
    grid_add_nationalparks = TRUE,
    grid_add_waterways = TRUE,
    grid_cell_size = 5000,
    model_dates_to_run = c("202405"),
    model_degree_of_neighbour = "first",
    model_bol_distance = TRUE,
    model_bol_border = FALSE,
    model_covariates_title = "lead_events_fatalities",
    model_rho = 0.6
  ),
  streets_first_degree_no_dist_cov_lead_events_fatalities = add_to_parameter_grid(
    name = "streets_first_degree_no_dist_cov_lead_events_fatalities",
    grid_add_streets = TRUE,
    grid_add_nationalparks = TRUE,
    grid_add_waterways = TRUE,
    grid_cell_size = 5000,
    model_dates_to_run = c("202405"),
    model_degree_of_neighbour = "second",
    model_bol_distance = TRUE,
    model_bol_border = FALSE,
    model_covariates_title = "lead_events_fatalities",
    model_rho = 0.6
  ),
  streets_first_degree_no_dist_cov_lead_events_fatalities = add_to_parameter_grid(
    name = "streets_first_degree_no_dist_cov_lead_events_fatalities",
    grid_add_streets = F,
    grid_add_nationalparks = TRUE,
    grid_add_waterways = TRUE,
    grid_cell_size = 5000,
    model_dates_to_run = c("202405"),
    model_degree_of_neighbour = "second",
    model_bol_distance = TRUE,
    model_bol_border = F,
    model_covariates_title = "lead_events_fatalities",
    model_rho = 0.6
  )
)

get_parameter_from_grid <- function(grid,name){
  
  coefs = grid[[name]]
  coefs_model = coefs[["model"]]
  coefs_grid = coefs[["grid"]]
  
  grid_add_streets <<- coefs_grid$add_streets
  grid_add_nationalparks <<- coefs_grid$add_nationalparks
  grid_add_waterways <<- coefs_grid$add_waterways
  grid_cell_size <<- coefs_grid$cell_size
  model_dates_to_run <<- coefs_model$dates_to_run
  model_degree_of_neighbour <<- coefs_model$degree_of_neighbour
  model_bol_distance <<- coefs_model$bol_distance
  model_bol_border <<- coefs_model$bol_border
  model_covariates_title <<- coefs_model$covariates_title
  model_rho <<- coefs_model$rho
  
}

