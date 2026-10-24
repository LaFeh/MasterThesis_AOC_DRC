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


parameter_grid <- list()



add_to_parameter_grid <- function(
    name,
    grid_add_streets,
    grid_add_nationalparks,
    grid_add_waterways,
    grid_cell_size,
    grid_street_size,
    model_dates_to_run,
    model_degree_of_neighbour,
    model_bol_distance,
    model_bol_border,
    model_covariates_title,
    model_rho,
    model_logit_rho_prior_mean,
    model_logit_rho_prior_sd
) {
  
  list(
    name = name,
    grid = list(
      add_streets = grid_add_streets,
      add_nationalparks = grid_add_nationalparks,
      add_waterways = grid_add_waterways,
      cell_size = grid_cell_size,
      street_size = grid_street_size
    ),
    model = list(
      dates_to_run = model_dates_to_run,
      degree_of_neighbour = model_degree_of_neighbour,
      bol_distance = model_bol_distance,
      bol_border = model_bol_border,
      covariates_title = model_covariates_title,
      rho = model_rho,
      prior_logit_rho_mean = model_logit_rho_prior_mean,
      prior_logit_rho_sd = model_logit_rho_prior_sd
    )
  )
}

parameter_grid <- list(
  streets_first_degree_no_dist_lead_civilians_estimaterho = add_to_parameter_grid(
    name = "streets_first_degree_no_dist_lead_civilians_estimaterho",
    grid_add_streets = TRUE,
    grid_add_nationalparks = TRUE,
    grid_add_waterways = TRUE,
    grid_cell_size = 3000,
    grid_street_size = "",
    model_dates_to_run = c("202411"),
    model_degree_of_neighbour = "first",
    model_bol_distance = FALSE,
    model_bol_border = FALSE,
    model_covariates_title = "lead_civilians",
    model_rho = NULL,
    model_logit_rho_prior_sd = 1,
    model_logit_rho_prior_mean = 0
  ),
  streets_first_degree_no_dist_lead_battles_estimaterho = add_to_parameter_grid(
    name = "streets_first_degree_no_dist_lead_battles_estimaterho",
    grid_add_streets = TRUE,
    grid_add_nationalparks = TRUE,
    grid_add_waterways = TRUE,
    grid_cell_size = 3000,
    grid_street_size = "",
    model_dates_to_run = c("202411"),
    model_degree_of_neighbour = "first",
    model_bol_distance = FALSE,
    model_bol_border = FALSE,
    model_covariates_title = "lead_battles",
    model_rho = NULL,
    model_logit_rho_prior_sd = 1,
    model_logit_rho_prior_mean = 0
  ),
  streets_first_degree_no_dist_total_battles_estimaterho = add_to_parameter_grid(
    name = "streets_first_degree_no_dist_total_battles_estimaterho",
    grid_add_streets = TRUE,
    grid_add_nationalparks = TRUE,
    grid_add_waterways = TRUE,
    grid_cell_size = 3000,
    grid_street_size = "",
    model_dates_to_run = c("202411"),
    model_degree_of_neighbour = "first",
    model_bol_distance = FALSE,
    model_bol_border = FALSE,
    model_covariates_title = "total_battles",
    model_rho = NULL,
    model_logit_rho_prior_sd = 1,
    model_logit_rho_prior_mean = 0
  ),
  streets_first_degree_no_dist_total_rv_battles_civilians_estimaterho = add_to_parameter_grid(
    name = "streets_first_degree_no_dist_total_rv_battles_civilians_estimaterho",
    grid_add_streets = TRUE,
    grid_add_nationalparks = TRUE,
    grid_add_waterways = TRUE,
    grid_cell_size = 3000,
    grid_street_size = "",
    model_dates_to_run = c("202411"),
    model_degree_of_neighbour = "first",
    model_bol_distance = FALSE,
    model_bol_border = FALSE,
    model_covariates_title = "total_rv_battles_civilians",
    model_rho = NULL,
    model_logit_rho_prior_sd = 1,
    model_logit_rho_prior_mean = 0
  ),
  streets_first_degree_no_dist_total_lead_battles_estimaterho = add_to_parameter_grid(
    name = "streets_first_degree_no_dist_total_lead_battles_estimaterho",
    grid_add_streets = TRUE,
    grid_add_nationalparks = TRUE,
    grid_add_waterways = TRUE,
    grid_cell_size = 3000,
    grid_street_size = "",
    model_dates_to_run = c("202411"),
    model_degree_of_neighbour = "first",
    model_bol_distance = FALSE,
    model_bol_border = FALSE,
    model_covariates_title = "total_lead_battles",
    model_rho = NULL,
    model_logit_rho_prior_sd = 1,
    model_logit_rho_prior_mean = 0
  ),
  streets_first_degree_no_dist_total_lead_rv_battles_civilians_estimaterho = add_to_parameter_grid(
    name = "streets_first_degree_no_dist_total_lead_rv_battles_civilians_estimaterho",
    grid_add_streets = TRUE,
    grid_add_nationalparks = TRUE,
    grid_add_waterways = TRUE,
    grid_cell_size = 3000,
    grid_street_size = "",
    model_dates_to_run = c("202411"),
    model_degree_of_neighbour = "first",
    model_bol_distance = FALSE,
    model_bol_border = FALSE,
    model_covariates_title = "total_lead_rv_battles_civilians",
    model_rho = NULL,
    model_logit_rho_prior_sd = 1,
    model_logit_rho_prior_mean = 0
  ),
  streets_first_degree_no_dist_lag_rv_battles_estimaterho = add_to_parameter_grid(
    name = "streets_first_degree_no_dist_lag_rv_battles_estimaterho",
    grid_add_streets = TRUE,
    grid_add_nationalparks = TRUE,
    grid_add_waterways = TRUE,
    grid_cell_size = 3000,
    grid_street_size = "",
    model_dates_to_run = c("202411"),
    model_degree_of_neighbour = "first",
    model_bol_distance = FALSE,
    model_bol_border = FALSE,
    model_covariates_title = "lag_rv_battles",
    model_rho = NULL,
    model_logit_rho_prior_sd = 1,
    model_logit_rho_prior_mean = 0
  ),
  # streets_first_degree_no_dist_lag_events_fatalities_estimaterho = add_to_parameter_grid(
  #   name = "streets_first_degree_no_dist_lag_events_fatalities_estimaterho",
  #   grid_add_streets = TRUE,
  #   grid_add_nationalparks = TRUE,
  #   grid_add_waterways = TRUE,
  #   grid_cell_size = 3000,
  #   grid_street_size = "", #3000
  #   model_dates_to_run = c("202411"),
  #   model_degree_of_neighbour = "first",
  #   model_bol_distance = FALSE,
  #   model_bol_border = FALSE,
  #   model_covariates_title = "lag_events_fatalities",
  #   model_rho = NULL,
  #   model_logit_rho_prior_sd = 1,
  #   model_logit_rho_prior_mean = 0
  # ),
  # streets_6000_first_degree_no_dist_lag_lead_rv_battles_civilians_estimaterho = add_to_parameter_grid(
  #   name = "streets_6000_first_degree_no_dist_lag_lead_rv_battles_civilians_estimaterho",
  #   grid_add_streets = TRUE,
  #   grid_add_nationalparks = TRUE,
  #   grid_add_waterways = TRUE,
  #   grid_cell_size = 3000,
  #   grid_street_size = 6000,
  #   model_dates_to_run = c("202411"),
  #   model_degree_of_neighbour = "first",
  #   model_bol_distance = FALSE,
  #   model_bol_border = FALSE,
  #   model_covariates_title = "lag_lead_rv_battles_civilians",
  #   model_rho = NULL,
  #   model_logit_rho_prior_sd = 1,
  #   model_logit_rho_prior_mean = 0
  # ),
  # streets_6000_first_degree_no_dist_lag_lead_rv_battles_estimaterho = add_to_parameter_grid(
  #   name = "streets_6000_first_degree_no_dist_lag_lead_rv_battles_estimaterho",
  #   grid_add_streets = TRUE,
  #   grid_add_nationalparks = TRUE,
  #   grid_add_waterways = TRUE,
  #   grid_cell_size = 3000,
  #   grid_street_size = 6000,
  #   model_dates_to_run = c("202411"),
  #   model_degree_of_neighbour = "first",
  #   model_bol_distance = FALSE,
  #   model_bol_border = FALSE,
  #   model_covariates_title = "lag_lead_rv_battles",
  #   model_rho = NULL,
  #   model_logit_rho_prior_sd = 1,
  #   model_logit_rho_prior_mean = 0
  # ),
  # streets_6000_first_degree_no_dist_lead_rv_battles_estimaterho = add_to_parameter_grid(
  #   name = "streets_6000_first_degree_no_dist_lead_rv_battles_estimaterho",
  #   grid_add_streets = TRUE,
  #   grid_add_nationalparks = TRUE,
  #   grid_add_waterways = TRUE,
  #   grid_cell_size = 3000,
  #   grid_street_size = 6000,
  #   model_dates_to_run = c("202411"),
  #   model_degree_of_neighbour = "first",
  #   model_bol_distance = FALSE,
  #   model_bol_border = FALSE,
  #   model_covariates_title = "lead_rv_battles",
  #   model_rho = NULL,
  #   model_logit_rho_prior_sd = 1,
  #   model_logit_rho_prior_mean = 0
  # ),
  # streets_6000_first_degree_no_dist_lag_rv_battles_estimaterho = add_to_parameter_grid(
  #   name = "streets_6000_first_degree_no_dist_lag_rv_battles_estimaterho",
  #   grid_add_streets = TRUE,
  #   grid_add_nationalparks = TRUE,
  #   grid_add_waterways = TRUE,
  #   grid_cell_size = 3000,
  #   grid_street_size = 6000,
  #   model_dates_to_run = c("202411"),
  #   model_degree_of_neighbour = "first",
  #   model_bol_distance = FALSE,
  #   model_bol_border = FALSE,
  #   model_covariates_title = "lag_rv_battles",
  #   model_rho = NULL,
  #   model_logit_rho_prior_sd = 1,
  #   model_logit_rho_prior_mean = 0
  # ),
  streets_first_degree_no_dist_lag_lead_rv_battles_civilians_estimaterho = add_to_parameter_grid(
    name = "streets_first_degree_no_dist_lag_lead_rv_battles_civilians_estimaterho",
    grid_add_streets = TRUE,
    grid_add_nationalparks = TRUE,
    grid_add_waterways = TRUE,
    grid_cell_size = 3000,
    grid_street_size = "",
    model_dates_to_run = c("202411"),
    model_degree_of_neighbour = "first",
    model_bol_distance = FALSE,
    model_bol_border = FALSE,
    model_covariates_title = "lag_lead_rv_battles_civilians",
    model_rho = NULL,
    model_logit_rho_prior_sd = 1,
    model_logit_rho_prior_mean = 0
  ),
  streets_first_degree_no_dist_lead_rv_battles_civilians_estimaterho = add_to_parameter_grid(
    name = "streets_first_degree_no_dist_lead_rv_battles_civilians_estimaterho",
    grid_add_streets = TRUE,
    grid_add_nationalparks = TRUE,
    grid_add_waterways = TRUE,
    grid_cell_size = 3000,
    grid_street_size = "",
    model_dates_to_run = c("202411"),
    model_degree_of_neighbour = "first",
    model_bol_distance = FALSE,
    model_bol_border = FALSE,
    model_covariates_title = "lead_rv_battles_civilians",
    model_rho = NULL,
    model_logit_rho_prior_sd = 1,
    model_logit_rho_prior_mean = 0
  ),
  streets_first_degree_no_dist_lag_lead_rv_battles_estimaterho = add_to_parameter_grid(
    name = "streets_first_degree_no_dist_lag_lead_rv_battles_estimaterho",
    grid_add_streets = TRUE,
    grid_add_nationalparks = TRUE,
    grid_add_waterways = TRUE,
    grid_cell_size = 3000,

    grid_street_size = "",
    model_dates_to_run = c("202411"),
    model_degree_of_neighbour = "first",
    model_bol_distance = FALSE,
    model_bol_border = FALSE,
    model_covariates_title = "lag_lead_rv_battles",
    model_rho = NULL,
    model_logit_rho_prior_sd = 1,
    model_logit_rho_prior_mean = 0
  ),
  streets_first_degree_no_dist_lead_rv_battles_estimaterho = add_to_parameter_grid(
    name = "streets_first_degree_no_dist_lead_rv_battles_estimaterho",
    grid_add_streets = TRUE,
    grid_add_nationalparks = TRUE,
    grid_add_waterways = TRUE,
    grid_cell_size = 3000,
    grid_street_size = "",
    model_dates_to_run = c("202411"),
    model_degree_of_neighbour = "first",
    model_bol_distance = FALSE,
    model_bol_border = FALSE,
    model_covariates_title = "lead_rv_battles",
    model_rho = NULL,
    model_logit_rho_prior_sd = 1,
    model_logit_rho_prior_mean = 0
  ),
  streets_first_degree_no_dist_lag_rv_battles_estimaterho = add_to_parameter_grid(
    name = "streets_first_degree_no_dist_lag_rv_battles_estimaterho",
    grid_add_streets = TRUE,
    grid_add_nationalparks = TRUE,
    grid_add_waterways = TRUE,
    grid_cell_size = 3000,
    grid_street_size = "",
    model_dates_to_run = c("202411"),
    model_degree_of_neighbour = "first",
    model_bol_distance = FALSE,
    model_bol_border = FALSE,
    model_covariates_title = "lag_rv_battles",
    model_rho = NULL,
    model_logit_rho_prior_sd = 1,
    model_logit_rho_prior_mean = 0
  ),
  # streets_6000_first_degree_no_dist = add_to_parameter_grid(
  #   name = "streets_6000_first_degree_no_dist",
  #   grid_add_streets = TRUE,
  #   grid_add_nationalparks = TRUE,
  #   grid_add_waterways = TRUE,
  #   grid_cell_size = 3000,
  #   grid_street_size = 6000,
  #   model_dates_to_run = c("202411"),
  #   model_degree_of_neighbour = "first",
  #   model_bol_distance = FALSE,
  #   model_bol_border = FALSE,
  #   model_covariates_title = "Intercept",
  #   model_rho = NULL,
  #   model_logit_rho_prior_sd = 1,
  #   model_logit_rho_prior_mean = 0
  # ),
  streets_first_degree_no_dist = add_to_parameter_grid(
    name = "streets_first_degree_no_dist",
    grid_add_streets = TRUE,
    grid_add_nationalparks = TRUE,
    grid_add_waterways = TRUE,
    grid_cell_size = 3000,
    grid_street_size = "",
    model_dates_to_run = c("202411"),
    model_degree_of_neighbour = "first",
    model_bol_distance = FALSE,
    model_bol_border = FALSE,
    model_covariates_title = "Intercept",
    model_rho = NULL,
    model_logit_rho_prior_sd = 1,
    model_logit_rho_prior_mean = 0
  )
  # streets_first_degree_no_dist = add_to_parameter_grid(
  #   name = "streets_first_degree_no_dist",
  #   grid_add_streets = TRUE,
  #   grid_add_nationalparks = TRUE,
  #   grid_add_waterways = TRUE,
  #   grid_cell_size = 3000,
  #   grid_street_size = "",
  #   model_dates_to_run = c("202411"),
  #   model_degree_of_neighbour = "first",
  #   model_bol_distance = FALSE,
  #   model_bol_border = FALSE,
  #   model_covariates_title = "Intercept",
  #   model_rho = NULL,
  #   model_logit_rho_prior_sd = 1,
  #   model_logit_rho_prior_mean = 0
  # ),
  # streets_first_degree_no_dist_5000 = add_to_parameter_grid(
  #   name = "streets_first_degree_no_dist_5000",
  #   grid_add_streets = TRUE,
  #   grid_add_nationalparks = TRUE,
  #   grid_add_waterways = TRUE,
  #   grid_cell_size = 5000,
  #   grid_street_size = "",
  #   model_dates_to_run = c("202411"),
  #   model_degree_of_neighbour = "first",
  #   model_bol_distance = FALSE,
  #   model_bol_border = FALSE,
  #   model_covariates_title = "Intercept",
  #   model_rho = NULL,
  #   model_logit_rho_prior_sd = 1,
  #   model_logit_rho_prior_mean = 0
  # ),
  # streets_first_degree_no_dist_7000 = add_to_parameter_grid(
  #   name = "streets_first_degree_no_dist_7000",
  #   grid_add_streets = TRUE,
  #   grid_add_nationalparks = TRUE,
  #   grid_add_waterways = TRUE,
  #   grid_cell_size = 7000,
  #   grid_street_size = "",
  #   model_dates_to_run = c("202411"),
  #   model_degree_of_neighbour = "first",
  #   model_bol_distance = FALSE,
  #   model_bol_border = FALSE,
  #   model_covariates_title = "Intercept",
  #   model_rho = NULL,
  #   model_logit_rho_prior_sd = 1,
  #   model_logit_rho_prior_mean = 0
  # )
  # # streets_scnd_degree_dist = add_to_parameter_grid(
  # #   name = "streets_scnd_degree_dist",
  # #   grid_add_streets = TRUE,
  # #   grid_add_nationalparks = TRUE,
  # #   grid_add_waterways = TRUE,
  # #   grid_cell_size = 3000,
  # #   model_dates_to_run = c("202411"),
  # #   model_degree_of_neighbour = "second",
  # #   model_bol_distance = TRUE,
  # #   model_bol_border = FALSE,
  # #   model_covariates_title = "Intercept",
  # #   
  # #   model_rho = 0.6
  # # ),
  # # streets_first_degree_dist = add_to_parameter_grid(
  # #   name = "streets_scnd_degree_dist",
  # #   grid_add_streets = TRUE,
  # #   grid_add_nationalparks = TRUE,
  # #   grid_add_waterways = TRUE,
  # #   grid_cell_size = 3000,
  # #   model_dates_to_run = c("202411"),
  # #   model_degree_of_neighbour = "first",
  # #   model_bol_distance = TRUE,
  # #   model_bol_border = FALSE,
  # #   model_covariates_title = "Intercept",
  # #   
  # #   model_rho = 0.6
  # # ),


)

get_parameter_from_grid <- function(grid,name){
  
  coefs = grid[[name]]
  coefs_model = coefs[["model"]]
  coefs_grid = coefs[["grid"]]
  
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
  
}

