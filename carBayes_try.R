library(CARBayes)

#05a run model

library(Matrix)
source("./00b_helper_create_grid_name.R")
source("./05_model/05a_a_helper_run_model.R")
source("./05_model/05a_b_helper_parameter_grid.R")

source("./04b_a_helper_functions.R")

## prepare data



name_parameter_grid = "wo_streets_first_degree_no_dist"
get_parameter_from_grid(parameter_grid,name_parameter_grid)


name_of_grid = create_grid_name(add_waterways = grid_add_waterways,
                                add_nationalparks = grid_add_nationalparks,
                                add_streets = grid_add_streets,
                                cell_size = grid_cell_size)


name_of_grid_file_name = gsub(".shp","",name_of_grid)
data_file_name  = paste0("./data/data_for_prediction/",name_of_grid_file_name,"/","202405","_events.RData")


path_of_adjacency_matrix = paste0("./data/data_for_prediction/mat_w_mixedtime_neighbour",
                                  model_degree_of_neighbour,
                                  "_distance_", model_bol_distance,
                                  "_",model_bol_border,"_",name_of_grid_clean,".RData")


data_mat_w <- readRDS(path_of_adjacency_matrix)
W <- as.matrix(data_mat_w)
W <- (W + t(W)) / 2    # enforce symmetry
diag(W) <- 0           # no self-loops

load(data_file_name)

data$control_binom = ifelse(data$control_binom ==0.5 | is.na(data$control_binom),0,data$control_binom)
data$intercept = 1

no_nghbr = which(rowSums(W)==0)
data = data[-no_nghbr,]
#data = data[order(data$cell_id),]
W = W[-no_nghbr,-no_nghbr]
model = S.CARleroux(formula = as.formula("control_binom~1"),family = "binomial",data = data,
                   trials = rep(1L, nrow(data)),burnin = 1500,n.sample = 5000,n.chains = 5,
                   W = W, rho = 0.6)

library(CARBayes)
library(coda)
library(ggplot2)
library(dplyr)
library(purrr)

# ---------------------------------------------------------
# 1. Fit the model (as you specified)
# ---------------------------------------------------------


print(model)

# ---------------------------------------------------------
# 2. Inspect structure FIRST — CARBayes's internal format
#    for multi-chain output varies by version, so check
#    before assuming shape
# ---------------------------------------------------------
str(model$samples, max.level = 1)
class(model$samples$beta)
class(model$samples$phi)

# If model$samples$beta is already an `mcmc.list` (one element per chain),
# use it directly below. If it's a single stacked matrix (chains
# concatenated), you'll need model$samples$beta split by chain length:
#   chain_len <- (n.sample - burnin) / thin   (adjust for your `thin`)
# and split rows accordingly. The code below assumes `mcmc.list` format,
# which is standard for n.chains > 1 in current CARBayes; adjust if
# str() shows otherwise.

# ---------------------------------------------------------
# 3. Convergence diagnostics across chains
# ---------------------------------------------------------
beta_mcmc_list <- coda::as.mcmc.list(model$samples$beta)
gelman.diag(beta_mcmc_list)          # R-hat for beta0 (the intercept)

phi_samples_list <- model$samples$phi  # list of chains, each iterations x n areas
mean_phi_mcmc_list <- mcmc.list(
  map(1:length(phi_samples_list), ~ mcmc(rowMeans(phi_samples_list[[.x]])))
)
gelman.diag(mean_phi_mcmc_list)      # R-hat for mean(phi) — key diagnostic

# ---------------------------------------------------------
# 4. Trace plots across chains with ggplot2
# ---------------------------------------------------------
mcmc_list_to_df <- function(mcmc_list, param_name = "value") {
  imap_dfr(seq_along(mcmc_list), function(i, ...) {
    chain_obj <- mcmc_list[[i]]
    data.frame(
      iteration = seq_len(length(chain_obj)),
      value     = as.numeric(chain_obj),
      chain     = paste0("chain", i)
    )
  }) %>% rename(!!param_name := value)
}

beta0_df <- mcmc_list_to_df(beta_mcmc_list, "beta0")

ggplot(beta0_df, aes(x = iteration, y = beta0, color = chain)) +
  geom_line(alpha = 0.7) +
  labs(title = "Trace plot: beta0 (intercept) across chains",
       x = "Iteration", y = "beta0") +
  theme_minimal()

mean_phi_df <- mcmc_list_to_df(mean_phi_mcmc_list, "mean_phi")

ggplot(mean_phi_df, aes(x = iteration, y = mean_phi, color = chain)) +
  geom_line(alpha = 0.7) +
  labs(title = "Trace plot: mean(phi) across chains",
       x = "Iteration", y = "mean(phi)") +
  theme_minimal()

# ---------------------------------------------------------
# 5. beta0 vs mean(phi) ridge check, by chain
# ---------------------------------------------------------
ridge_df <- imap_dfr(seq_along(beta_mcmc_list), function(i, ...) {
  data.frame(
    beta0    = as.numeric(beta_mcmc_list[[i]]),
    mean_phi = as.numeric(mean_phi_mcmc_list[[i]]),
    chain    = paste0("chain", i)
  )
})

ggplot(ridge_df, aes(x = beta0, y = mean_phi, color = chain)) +
  geom_point(alpha = 0.25, size = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "beta0 vs mean(phi), by chain") +
  theme_minimal()

ridge_df %>%
  group_by(chain) %>%
  summarise(cor_beta0_meanphi = cor(beta0, mean_phi))


gelman.diag(mean_phi_mcmc_list)
gelman.diag(beta_mcmc_list)



if (is.list(model$samples$phi) && !is.matrix(model$samples$phi)) {
  phi_pooled <- do.call(rbind, model$samples$phi)
} else {
  phi_pooled <- model$samples$phi
}

phi_mean  <- colMeans(phi_pooled)
phi_sd    <- apply(phi_pooled, 2, sd)
phi_lower <- apply(phi_pooled, 2, quantile, 0.025)
phi_upper <- apply(phi_pooled, 2, quantile, 0.975)

# ---------------------------------------------------------
# 2. Attach results directly onto the sf data (no join needed)
# ---------------------------------------------------------
# IMPORTANT: row order in `data` must match the order used to build W
# and fit the model — CARBayes does not reorder your data internally.

stopifnot(nrow(data) == length(phi_mean))

map_results <- data %>%
  mutate(
    fitted_prob = model$fitted.values,
    phi_mean    = phi_mean,
    phi_sd      = phi_sd,
    phi_lower   = phi_lower,
    phi_upper   = phi_upper,
    phi_sig     = case_when(
      phi_lower > 0 ~ "Above baseline",
      phi_upper < 0 ~ "Below baseline",
      TRUE          ~ "Not distinguishable"
    )
  )

# ---------------------------------------------------------
# 3. Map: posterior mean of spatial random effect (phi)
# ---------------------------------------------------------
p_phi <- ggplot(map_results) +
  geom_sf(aes(fill = phi_mean), color = "white", linewidth = 0.1) +
  scale_fill_viridis_c(name = "phi\n(posterior mean)") +
  labs(title = "Spatial random effect (phi) by area") +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank())

p_phi

# ---------------------------------------------------------
# 4. Map: fitted probability
# ---------------------------------------------------------
p_fitted <- ggplot(map_results) +
  geom_sf(aes(fill = fitted_prob), color = "white", linewidth = 0.1) +
  scale_fill_viridis_c(name = "Fitted\nprobability", limits = c(0, 1)) +
  labs(title = "Fitted probability by area") +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank())

p_fitted

# ---------------------------------------------------------
# 5. Map: uncertainty in phi (posterior SD)
# ---------------------------------------------------------
p_sd <- ggplot(map_results) +
  geom_sf(aes(fill = phi_sd), color = "white", linewidth = 0.1) +
  scale_fill_viridis_c(name = "phi\n(posterior SD)", option = "magma") +
  labs(title = "Uncertainty in spatial random effect by area") +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank())

p_sd

# ---------------------------------------------------------
# 6. Map: areas where phi's 95% CrI excludes zero
# ---------------------------------------------------------
p_sig <- ggplot(map_results) +
  geom_sf(aes(fill = phi_sig), color = "white", linewidth = 0.1) +
  scale_fill_manual(
    name = "phi 95% CrI",
    values = c("Above baseline" = "firebrick",
               "Below baseline" = "steelblue",
               "Not distinguishable" = "grey85")
  ) +
  labs(title = "Areas where phi differs from baseline") +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank())

p_sig

# ---------------------------------------------------------
# 7. Side-by-side panel: phi + fitted probability
# ---------------------------------------------------------
p_phi + p_fitted

# All four together
(p_phi + p_fitted) / (p_sd + p_sig)