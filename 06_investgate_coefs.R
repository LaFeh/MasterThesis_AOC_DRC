#06 get covariates over itme per model:

# evaluate based on ipis map:



library(Matrix)
library(grid)
library(png)
library(dplyr)
library(ggplot2)
library(patchwork)

source("./00b_helper_create_grid_name.R")
source("./05_model/05a_a_helper_run_model.R")
source("./05_model/05a_b_helper_parameter_grid.R")
source("./04b_a_helper_functions.R")

model_dir = "model_quality_TRUE_streets_first_degree_no_dist_lead_rv_battles_estimaterho_grid_surface_3000_water_park_street"
#model_dir = "model_quality_TRUE_streets_first_degree_no_dist_grid_surface_3000_water_park_street"


model_dir = "model_quality_TRUE_streets_first_degree_no_dist_lag_lead_rv_battles_civilians_estimaterho_grid_surface_3000_water_park_street"
model_dir = "model_quality_TRUE_streets_first_degree_no_dist_lag_lead_rv_battles_estimaterho_grid_surface_3000_water_park_street"

parameter_model = parameter_grid[unlist(sapply(names(parameter_grid),function(x) grepl(x,model_dir[1])))]
cov_list = create_covariates_list()
covs = cov_list[(parameter_model[[1]]$model$covariates_title)]

model_dir_path = file.path("~/MasterThesis_AOC_DRC/05_model/",model_dir)
files <- list.files(model_dir_path)

files = files[grepl("_report\\.RData$", files)]


model_coefs <- list()


for (file_idx in seq_along(files)) {
  
  file <- files[file_idx]
  load(file.path(model_dir_path, file))
  
  tau <- exp(rep$value["tau"])
  rho <- plogis(rep$value["rho"])
  
  #phi_w <- rep$par.random
  mean_phi_w_plogis <- mean(plogis(rep$par.random))
  
  betas <- rep$par.fixed[names(rep$par.fixed) == "beta"]
  names(betas) <- c("intercept",all.vars(covs[[1]]))
  
  
  
  date =substr(file,1,6)
  
  all_coefs <- c(
    betas,
    tau = as.numeric(tau),
    rho = as.numeric(rho),
    mean_phi = as.numeric(mean_phi_w_plogis) 
  )
  
  model_coefs[[file_idx]] <- data.frame(
    covariate = names(all_coefs),
    coef = as.numeric(all_coefs),
    file = rep(model_dir, length(all_coefs)),
    date = rep(date, length(all_coefs)),
    row.names = NULL
  )
}

model_coefs <- do.call(rbind, model_coefs)


ggplot(model_coefs) +
  geom_line(aes(x = date, y = coef, group = covariate, color = covariate)) +
  facet_wrap(~ covariate, scales = "free_y") +
  labs(title = model_dir)+
  theme_minimal()
