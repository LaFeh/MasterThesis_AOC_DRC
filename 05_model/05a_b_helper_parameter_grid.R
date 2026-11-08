#o5b_helper_parameter_grid

create_covariates_list<-function(){
  
  
  covariates_list = list()
  #covariates_list[["noIntercept"]] = as.formula("~0")
  #covariates_list[["noIntercept_distrwa"]] = as.formula("~0+min_dist_to_rwa")
  
  covariates_list[["Intercept"]] = as.formula("~1")
  
  #covariates_list[["total_lag_lead_events_fatalities"]] =  as.formula("~ total_events+total_events_lag+total_events_lead+total_fatalities+total_fatalities_lead+total_fatalities_lag")
  #covariates_list[["total_lag_lead_battles_remotev_fatalities"]] =  as.formula("~ events_battles+events_battles_lag+events_battles_lead+
  #events_explosions_remote_violence+events_explosions_remote_violence_lag+events_explosions_remote_violence_lead+
  #                                                                          total_fatalities+total_fatalities_lead+total_fatalities_lag")
  
  #covariates_list[["lead_events_fatalities"]] =  as.formula("~ total_events_lead+total_fatalities_lead")
  #covariates_list[["total_lag_lead_events_fatalities_distrwa"]] =  as.formula("~ min_dist_to_rwa + total_events+total_events_lag+total_events_lead+total_fatalities+total_fatalities_lead+total_fatalities_lag")
  #covariates_list[["total_lag_lead_battles_remotev_fatalities_distrwa"]] =  as.formula("~ min_dist_to_rwa + events_battles+events_battles_lag+events_battles_lead+
  #events_explosions_remote_violence+events_explosions_remote_violence_lag+events_explosions_remote_violence_lead+
  #                                                                          total_fatalities+total_fatalities_lead+total_fatalities_lag")
  
  #covariates_list[["lead_events_fatalities_distrwa"]] =  as.formula("~ min_dist_to_rwa + total_events_lead+total_fatalities_lead")
  #covariates_list[["lag_events_fatalities"]] =  as.formula("~  total_events_lag+total_fatalities_lag")
  
  
  
  covariates_list[["lag_lead_rv_battles_civilians"]] =  as.formula("~  events_explosions_remote_violence_lag+events_explosions_remote_violence_lead+events_battles_lead+events_battles_lag+events_violence_against_civilians_lead++events_violence_against_civilians_lag")
  covariates_list[["lag_lead_rv_battles"]] =  as.formula("~  events_explosions_remote_violence_lag+events_explosions_remote_violence_lead+events_battles_lead+events_battles_lag")
  
  covariates_list[["lead_rv_battles"]] =  as.formula("~  events_explosions_remote_violence_lead+events_battles_lead")
  
  covariates_list[["total_lead_rv_battles_civilians"]] =  as.formula("~  events_explosions_remote_violence_lead+events_explosions_remote_violence+events_battles_lead+events_violence_against_civilians_lead+events_battles+ events_violence_against_civilians")
  covariates_list[["total_lead_rv_battles"]] =  as.formula("~  events_explosions_remote_violence_lead+events_battles_lead+events_battles+ events_explosions_remote_violence")
  covariates_list[["total_lead_battles"]] =  as.formula("~ events_battles_lead+events_battles")
  
  covariates_list[["total_rv_battles_civilians"]] =  as.formula("~events_explosions_remote_violence+events_battles+ events_violence_against_civilians")
  covariates_list[["lead_rv_battles_civilians"]] =  as.formula("~  events_explosions_remote_violence_lead+events_battles_lead+events_violence_against_civilians_lead")
  covariates_list[["lead_civilians"]] =  as.formula("~ events_violence_against_civilians_lead")
  
  
  covariates_list[["total_battles"]] =  as.formula("~events_battles")
  covariates_list[["lead_battles"]] =  as.formula(" ~events_battles_lead")
  
  
  covariates_list[["lag_rv_battles"]] =  as.formula("~  events_explosions_remote_violence_lag+events_battles_lag")
  
  
  
  
  return(covariates_list)
}

add_to_parameter_grid <- function(
    grid_add_streets,
    grid_add_nationalparks,
    grid_add_waterways,
    grid_cell_size,
    grid_street_size,
    
    adj_traveltime,
    adj_degree_of_neighbour,
    adj_bol_distance,
    adj_bol_border,
    
    model_dates_to_run,
    quality,
    model_covariates_title,
    model_rho,
    model_tau,
    model_logit_rho_prior_mean,
    model_logit_rho_prior_sd,
    model_estimation_strategy,
    model_travel_time
) {
  
  # Automatically generate grid name
  grid_name <- create_grid_name(
    grid_add_streets,
    grid_add_nationalparks,
    grid_add_waterways,
    grid_cell_size,
    grid_street_size
  )
  
  adjacency_name <- create_adjacency_name(
    adj_traveltime = adj_traveltime,
    adj_neighbour_degree = adj_degree_of_neighbour,
    adj_bol_distance,
    adj_bol_border
  )
  
  final_data_name <- create_final_data_name(grid_name,quality)
  
  # Automatically generate model name
  model_name <- create_model_name(
    estimation_technique = model_estimation_strategy,
    model_covariates_title
  )
  
  # Combine names
  full_name <- paste0(
    tools::file_path_sans_ext(final_data_name),
    "_",
    adjacency_name,"_",model_name
  )
  
  # Return parameter grid
  list(
    full_name = full_name,
    final_data_name = final_data_name,
    
    grid = list(
      grid_name = grid_name,
      add_streets = grid_add_streets,
      add_nationalparks = grid_add_nationalparks,
      add_waterways = grid_add_waterways,
      cell_size = grid_cell_size,
      street_size = grid_street_size
    ),
    
    adjacency = list(
      adjacency_name = adjacency_name,
      traveltime = adj_traveltime,
      degree_of_neighbour = adj_degree_of_neighbour,
      bol_distance = adj_bol_distance,
      bol_border = adj_bol_border
    ),
    
    model = list(
      model_name = model_name,
      dates_to_run = model_dates_to_run,
      covariates_title = model_covariates_title,
      rho = model_rho,
      tau = model_tau,
      estimation_strategy = model_estimation_strategy,
      prior_logit_rho_mean = model_logit_rho_prior_mean,
      prior_logit_rho_sd = model_logit_rho_prior_sd
    )
  )
}


get_parameter_from_grid <- function(grid,name){
  
  coefs = grid[[name]]
  coefs_model = coefs[["model"]]
  coefs_grid = coefs[["grid"]]
  coefs_adjacency= coefs[["adjacency"]]
  
  adjacency_name <<-coefs_adjacency$adjacency_name
  grid_add_streets <<- coefs_grid$add_streets
  grid_add_nationalparks <<- coefs_grid$add_nationalparks
  grid_add_waterways <<- coefs_grid$add_waterways
  grid_cell_size <<- coefs_grid$cell_size
  grid_street_size <<- coefs_grid$street_size
  model_dates_to_run <<- coefs_model$dates_to_run
  model_degree_of_neighbour <<- coefs_model$degree_of_neighbour
  model_bol_distance <<- coefs_model$bol_distance
  model_bol_border <<- coefs_model$bol_border
  model_covariates_title <<- coefs_model$covariates_title
  model_rho <<- coefs_model$rho
  model_logit_rho_prior_sd <<- coefs_model$prior_logit_rho_sd
  model_logit_rho_prior_mean <<- coefs_model$prior_logit_rho_mean
  model_estimation_strategy <<- coefs_model$estimation_strategy
  
}


parameter_grid <- list()

new_parameter <- add_to_parameter_grid(
  grid_add_streets = FALSE,
  grid_add_nationalparks = FALSE,
  grid_add_waterways = FALSE,
  grid_cell_size = 3000,
  grid_street_size = "",
  
  adj_traveltime = "ciesin",
  adj_degree_of_neighbour = "first",
  adj_bol_distance = FALSE,
  adj_bol_border = FALSE,
  
  model_dates_to_run = c("202411"),
  quality = TRUE,
  model_covariates_title = "Intercept",
  model_rho = NULL,
  model_tau = NULL,
  model_logit_rho_prior_sd = 1,
  model_logit_rho_prior_mean = 0,
  model_estimation_strategy = "free",
)
parameter_grid[[new_parameter$full_name]] <- new_parameter

new_parameter <- add_to_parameter_grid(
  grid_add_streets = T,
  grid_add_nationalparks = T,
  grid_add_waterways = T,
  grid_cell_size = 3000,
  grid_street_size = 3000,
  model_dates_to_run = c("202411"),
  adj_traveltime = "ciesin",
  adj_degree_of_neighbour = "first",
  adj_bol_distance = FALSE,
  adj_bol_border = FALSE,
  quality = TRUE,
  model_covariates_title = "Intercept",
  model_rho = NULL,
  model_tau = NULL,
  model_logit_rho_prior_sd = 1,
  model_logit_rho_prior_mean = 0,
  model_estimation_strategy = "free",
)
parameter_grid[[new_parameter$full_name]] <- new_parameter


new_parameter <- add_to_parameter_grid(
  grid_add_streets = F,
  grid_add_nationalparks = F,
  grid_add_waterways = F,
  grid_cell_size = 3000,
  grid_street_size = "",
  model_dates_to_run = c("202411"),
  adj_traveltime = "grufs",
  adj_degree_of_neighbour = "first",
  adj_bol_distance = FALSE,
  adj_bol_border = FALSE,
  quality = TRUE,
  model_covariates_title = "Intercept",
  model_rho = NULL,
  model_tau = NULL,
  model_logit_rho_prior_sd = 1,
  model_logit_rho_prior_mean = 0,
  model_estimation_strategy = "free",
)
parameter_grid[[new_parameter$full_name]] <- new_parameter
