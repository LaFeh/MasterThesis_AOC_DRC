# compate Mat_w and mat_b

setwd("D:/DRC/gaussian_process_AOC")

load(file = "./tmb_try/leroux_with_priors_wo_constraint_mat_w.RData")
mat_w_rep = rep
load(file = "./tmb_try/leroux_with_priors_wo_constraint_mat_b.RData")
mat_b_rep = rep

data       <- readRDS("./data/inla_data/data_prepared_for_inla.RData")

fixed = summary(mat_b_rep, "fixed")    # beta, log_tau, logit_rho with SEs
print(fixed)
fixed = summary(mat_w_rep, "fixed")  
print(fixed)

plot(plogis(mat_w_rep$par.random),plogis(mat_b_rep$par.random),xlim =c(0,1),ylim=c(0,1))
abline(0.4,0.4,col = "red")


data$phi_w = mat_w_rep$par.random
data$phi_b = mat_b_rep$par.random
diff_threshold = summary(data$phi_w -data$phi_b)["3rd Qu."]

data$phi_w_plogis = plogis(mat_w_rep$par.random)
data$phi_b_plogis = plogis(mat_b_rep$par.random)


data$p_w = plogis(mat_w_rep$par.fixed["beta"]+mat_w_rep$par.random)
data$p_b = plogis(mat_b_rep$par.fixed["beta"]+mat_b_rep$par.random)

data$p_w   <- as.numeric(report_vals$p)
data$phi_mean   <- as.numeric(plogis(report_vals$phi))
data$eta_mean <- as.numeric(report_vals$eta)

library(ggplot2)
p <- ggplot2::ggplot(data[which(data$phi_w -data$phi_b>=diff_threshold),]) +
  geom_sf(aes(fill =  p_w)) +
  scale_fill_viridis_c() +
  theme_minimal()

p

library(ggplot2)
p <- ggplot2::ggplot(data[which(data$phi_w -data$phi_b>=diff_threshold),]) +
  geom_sf(aes(fill =  p_b)) +
  scale_fill_viridis_c() +
  theme_minimal()

p

library(dplyr)

plot_data <- data %>%
  filter(phi_w - phi_b >= diff_threshold) %>%
  select(geometry, phi_w, phi_b_plogis) %>%
  tidyr::pivot_longer(
    cols = c(phi_w_plogis, phi_b_plogis),
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    variable = recode(
      variable,
      phi_w_plogis = "weighted",
      phi_b_plogis = "binary"
    )
  )

ggplot(plot_data) +
  geom_sf(aes(fill = value)) +
  scale_fill_viridis_c() +
  facet_wrap(~ variable, ncol = 2) +
  theme_minimal()
