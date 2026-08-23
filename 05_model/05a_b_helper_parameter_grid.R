#o5b_helper_parameter_grid

create_covariates_list<-function(){
  
  
  covariates_list = list()
  covariates_list[["noIntercept"]] = as.formula("~0")
  covariates_list[["noIntercept_distrwa"]] = as.formula("~0+min_dist_to_rwa")
  covariates_list[["Intercept"]] = as.formula("~1")
  covariates_list[["total_lag_lead_events_fatalities"]] =  as.formula("~ total_events+total_events_lag+total_events_lead+total_fatalities+total_fatalities_lead+total_fatalities_lag")
  covariates_list[["total_lag_lead_battles_remotev_fatalities"]] =  as.formula("~ events_battles+events_battles_lag+events_battles_lead+
  events_remote_violence+events_remote_violence_lag+events_remote_violence_lead+
                                                                            total_fatalities+total_fatalities_lead+total_fatalities_lag")
  
  covariates_list[["lead_events_fatalities"]] =  as.formula("~ total_events_lead+total_fatalities_lead")
  covariates_list[["total_lag_lead_events_fatalities_distrwa"]] =  as.formula("~ min_dist_to_rwa + total_events+total_events_lag+total_events_lead+total_fatalities+total_fatalities_lead+total_fatalities_lag")
  covariates_list[["total_lag_lead_battles_remotev_fatalities_distrwa"]] =  as.formula("~ min_dist_to_rwa + events_battles+events_battles_lag+events_battles_lead+
  events_remote_violence+events_remote_violence_lag+events_remote_violence_lead+
                                                                            total_fatalities+total_fatalities_lead+total_fatalities_lag")
  
  covariates_list[["lead_events_fatalities_distrwa"]] =  as.formula("~ min_dist_to_rwa + total_events_lead+total_fatalities_lead")
  
  
  
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
    grid_cell_size = 3000,
    model_dates_to_run = c("202411"),
    model_degree_of_neighbour = "first",
    model_bol_distance = FALSE,
    model_bol_border = FALSE,
    model_covariates_title = "noIntercept",
    model_rho = 0.6
  ),
  streets_first_degree_no_dist_noIntercept_distrwa = add_to_parameter_grid(
    name = "streets_first_degree_no_dist_noIntercept_distrwa",
    grid_add_streets = TRUE,
    grid_add_nationalparks = TRUE,
    grid_add_waterways = TRUE,
    grid_cell_size = 3000,
    model_dates_to_run = c("202411"),
    model_degree_of_neighbour = "first",
    model_bol_distance = FALSE,
    model_bol_border = FALSE,
    model_covariates_title = "noIntercept_distrwa",
    model_rho = 0.6
  ),
  streets_first_degree_no_dist = add_to_parameter_grid(
    name = "streets_first_degree_no_dist",
    grid_add_streets = TRUE,
    grid_add_nationalparks = TRUE,
    grid_add_waterways = TRUE,
    grid_cell_size = 3000,
    model_dates_to_run = c("202411"),
    model_degree_of_neighbour = "first",
    model_bol_distance = FALSE,
    model_bol_border = FALSE,
    model_covariates_title = "Intercept",
    model_rho = 0.6
  ),
  # streets_scnd_degree_dist = add_to_parameter_grid(
  #   name = "streets_scnd_degree_dist",
  #   grid_add_streets = TRUE,
  #   grid_add_nationalparks = TRUE,
  #   grid_add_waterways = TRUE,
  #   grid_cell_size = 3000,
  #   model_dates_to_run = c("202411"),
  #   model_degree_of_neighbour = "second",
  #   model_bol_distance = TRUE,
  #   model_bol_border = FALSE,
  #   model_covariates_title = "Intercept",
  #   
  #   model_rho = 0.6
  # ),
  # streets_first_degree_dist = add_to_parameter_grid(
  #   name = "streets_scnd_degree_dist",
  #   grid_add_streets = TRUE,
  #   grid_add_nationalparks = TRUE,
  #   grid_add_waterways = TRUE,
  #   grid_cell_size = 3000,
  #   model_dates_to_run = c("202411"),
  #   model_degree_of_neighbour = "first",
  #   model_bol_distance = TRUE,
  #   model_bol_border = FALSE,
  #   model_covariates_title = "Intercept",
  #   
  #   model_rho = 0.6
  # ),
  # wo_streets_scnd_degree_dist = add_to_parameter_grid( # too much spread in weird directions.
  #   name = "wo_streets_scnd_degree_dist",
  #   grid_add_streets = F,
  #   grid_add_nationalparks = TRUE,
  #   grid_add_waterways = TRUE,
  #   grid_cell_size = 3000,
  #   model_dates_to_run = c("202411"),
  #   model_degree_of_neighbour = "second",
  #   model_bol_distance = TRUE,
  #   model_bol_border = FALSE,
  #   model_covariates_title = "Intercept",
  #   
  #   model_rho = 0.6
  # ),
  # wo_streets_first_degree_no_dist = add_to_parameter_grid( # is spreading too much, more spread than model with street with same settings.
  #   name = "wo_streets_first_degree_no_dist",
  #   grid_add_streets = F,
  #   grid_add_nationalparks = TRUE,
  #   grid_add_waterways = TRUE,
  #   grid_cell_size = 3000,
  #   model_dates_to_run = c("202411"),
  #   model_degree_of_neighbour = "first",
  #   model_bol_distance = FALSE,
  #   model_bol_border = FALSE,
  #   model_covariates_title = "Intercept",
  #   model_rho = 0.6
  # ),
  # wo_streets_first_degree_dist_cov_total_lag_lead_events_fatalities = add_to_parameter_grid(
  #   name = "wo_streets_first_degree_dist_cov_total_lag_lead_events_fatalities",
  #   grid_add_streets = F,
  #   grid_add_nationalparks = TRUE,
  #   grid_add_waterways = TRUE,
  #   grid_cell_size = 3000,
  #   model_dates_to_run = c("202411"),
  #   model_degree_of_neighbour = "first",
  #   model_bol_distance = TRUE,
  #   model_bol_border = FALSE,
  #   model_covariates_title = "total_lag_lead_events_fatalities",
  #   model_rho = 0.6
  # ),
  streets_first_degree_no_dist_cov_total_lag_lead_events_fatalities = add_to_parameter_grid(
    name = "streets_first_degree_no_dist_cov_total_lag_lead_events_fatalities",
    grid_add_streets = TRUE,
    grid_add_nationalparks = TRUE,
    grid_add_waterways = TRUE,
    grid_cell_size = 3000,
    model_dates_to_run = c("202411"),
    model_degree_of_neighbour = "first",
    model_bol_distance = F,
    model_bol_border = FALSE,
    model_covariates_title = "total_lag_lead_events_fatalities",
    model_rho = 0.6
  ),
  streets_first_degree_no_dist_cov_total_lag_lead_events_fatalities_distrwa = add_to_parameter_grid(
    name = "streets_first_degree_no_dist_cov_total_lag_lead_events_fatalities_distrwa",
    grid_add_streets = TRUE,
    grid_add_nationalparks = TRUE,
    grid_add_waterways = TRUE,
    grid_cell_size = 3000,
    model_dates_to_run = c("202411"),
    model_degree_of_neighbour = "first",
    model_bol_distance = F,
    model_bol_border = FALSE,
    model_covariates_title = "total_lag_lead_events_fatalities_distrwa",
    model_rho = 0.6
  ),
  # streets_first_degree_dist_cov_total_lag_lead_events_fatalities = add_to_parameter_grid(
  #   name = "streets_first_degree_dist_cov_total_lag_lead_events_fatalities",
  #   grid_add_streets = F,
  #   grid_add_nationalparks = TRUE,
  #   grid_add_waterways = TRUE,
  #   grid_cell_size = 3000,
  #   model_dates_to_run = c("202411"),
  #   model_degree_of_neighbour = "second",
  #   model_bol_distance = TRUE,
  #   model_bol_border = F,
  #   model_covariates_title = "total_lag_lead_events_fatalities",
  #   model_rho = 0.6
  # ),
  # wo_streets_first_degree_dist_cov_lead_events_fatalities = add_to_parameter_grid(
  #   name = "wo_streets_first_degree_dist_cov_lead_events_fatalities",
  #   grid_add_streets = F,
  #   grid_add_nationalparks = TRUE,
  #   grid_add_waterways = TRUE,
  #   grid_cell_size = 3000,
  #   model_dates_to_run = c("202411"),
  #   model_degree_of_neighbour = "first",
  #   model_bol_distance = TRUE,
  #   model_bol_border = FALSE,
  #   model_covariates_title = "lead_events_fatalities",
  #   model_rho = 0.6
  # ),
  streets_first_degree_no_dist_cov_lead_events_fatalities = add_to_parameter_grid(
    name = "streets_first_degree_no_dist_cov_lead_events_fatalities",
    grid_add_streets = TRUE,
    grid_add_nationalparks = TRUE,
    grid_add_waterways = TRUE,
    grid_cell_size = 3000,
    model_dates_to_run = c("202411"),
    model_degree_of_neighbour = "first",
    model_bol_distance = F,
    model_bol_border = FALSE,
    model_covariates_title = "lead_events_fatalities",
    model_rho = 0.6
  ),
  streets_first_degree_no_dist_cov_lead_events_fatalities_disrwa = add_to_parameter_grid(
    name = "streets_first_degree_no_dist_cov_lead_events_fatalities_disrwa",
    grid_add_streets = TRUE,
    grid_add_nationalparks = TRUE,
    grid_add_waterways = TRUE,
    grid_cell_size = 3000,
    model_dates_to_run = c("202411"),
    model_degree_of_neighbour = "first",
    model_bol_distance = F,
    model_bol_border = FALSE,
    model_covariates_title = "lead_events_fatalities_distrwa",
    model_rho = 0.6
  )#,
  # streets_first_degree_dist_cov_lead_events_fatalities = add_to_parameter_grid(
  #   name = "streets_first_degree_no_dist_cov_lead_events_fatalities",
  #   grid_add_streets = F,
  #   grid_add_nationalparks = TRUE,
  #   grid_add_waterways = TRUE,
  #   grid_cell_size = 3000,
  #   model_dates_to_run = c("202411"),
  #   model_degree_of_neighbour = "second",
  #   model_bol_distance = TRUE,
  #   model_bol_border = F,
  #   model_covariates_title = "lead_events_fatalities",
  #   model_rho = 0.6
  # # ) ,
  # streets_first_degree_dist_cov_total_lag_lead_battles_remotev_fatalities = add_to_parameter_grid(
  #   name = "streets_first_degree_dist_cov_total_lag_lead_battles_remotev_fatalities",
  #   grid_add_streets = F,
  #   grid_add_nationalparks = TRUE,
  #   grid_add_waterways = TRUE,
  #   grid_cell_size = 3000,
  #   model_dates_to_run = c("202411"),
  #   model_degree_of_neighbour = "second",
  #   model_bol_distance = TRUE,
  #   model_bol_border = F,
  #   model_covariates_title = "total_lag_lead_battles_remotev_fatalities",
  #   model_rho = 0.6
  # )
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

