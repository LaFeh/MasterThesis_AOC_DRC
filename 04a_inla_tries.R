library("INLA")

library("spatstat")
library("sp")
library("sf")

library(sf)
library(dplyr)
library(kernlab)
library(spdep)

setwd("D:/DRC/gaussian_process_AOC")
load("./data/frontline_data_all_previous_mnths_controle_num.RData")


tm = zoo::as.yearmon(as.Date("2023-08-01"),format = "%Y%M")
data = frontline_data_controle_num_all_previous_time%>%
  filter(time == tm)

data = cbind(data,st_coordinates(st_centroid(data$geometry)))
hist(data$mix_time_mean)


min_max_scaler <- function(x, min_target = 0, max_target = 1) {
  # Fehler abfangen, wenn Eingabe numerisch sein muss
  if (!is.numeric(x)) {
    stop("Die Eingabe muss numerisch sein.")
  }
  
  # Min und Max der Eingabedaten bestimmen
  min_val <- min(x, na.rm = TRUE)
  max_val <- max(x, na.rm = TRUE)
  
  # Sonderfall: Wenn alle Werte identisch sind (Division durch 0 verhindern)
  if (min_val == max_val) {
    return(rep(min_target, length(x)))
  }
  
  # Transformation auf das Zielintervall
  x_scaled <- (x - min_val) / (max_val - min_val)
  x_scaled <- x_scaled * (max_target - min_target) + min_target
  
  return(x_scaled)
}
data$mix_time_mean_scaled = min_max_scaler(log(data$mix_time_mean))
data$mix_time_mean_scaled = 1-data$mix_time_mean_scaled
# plot(data[440:445,c("mix_time_mean_scaled")])
# 
#hist(data$mix_time_mean_scaled,na.rm =T)
# test_grid = st_join(data,data[which(data$mix_time_mean_scaled <=0.1),],st_intersects,left =FALSE)
# data[which(data$mix_time_mean_scaled <=0.1),c("mix_time_mean_scaled")]
# plot(data[which(data$mix_time_mean_scaled <=0.1),c("mix_time_mean_scaled")])
# 
# plot(grid[which(grid$walk_time_mean!= grid$mix_time_mean),c("mix_time_mean")])
# plot(grid[440:443,c("surface")])
# 
# bbox = st_as_sf(st_as_sfc(st_bbox(data[440,])))
# grid_test = st_join(data, bbox,st_intersects, left = FALSE)
# plot(grid_test[,c("mix_time_mean_scaled")])



data_adj <- poly2nb(data,snap =3)

data_adj <- poly2nb(data, snap = 3)

weights_neightbours <- vector("list", length(data_adj))

for (id_n_set in seq_along(data_adj)) {
  
  n_set <- data_adj[[id_n_set]]
  
  weights <- numeric(length(n_set))
  
  for (id_n in seq_along(n_set)) {
    
    n <- n_set[id_n]
    
    weights[id_n] <- mean(
      data[c(id_n_set, n), ]$mix_time_mean_scaled,
      na.rm = TRUE
    )
  }
  
  weights_neightbours[[id_n_set]] <- weights
}

#k = nb2listw(data_adj)
data_mat_b<- nb2mat(data_adj, style = "B")
data_mat_w<- nb2mat(data_adj, glist = weights_neightbours)

data$ID <- 1:nrow(data)

# plot(data[33:35,]$geometry)
# plot(data[c(34),]$geometry,col = "red",add =T)
# plot(data$geometry,add =T)
unique(unlist(data_adj[1:60]))
plot(data[unique(unlist(data_adj[30:60])),c("mix_time_mean_scaled")])
plot(data[unique(unlist(data_adj[55:60])),c("mix_time_mean_scaled")])
plot(data[unique(unlist(data_adj[30:60])),c("mix_time_mean")])

plot(data[,c("mix_time_mean_scaled")])


library(viridis)
plot(data[,c("mix_time_mean_scaled")], pal = viridis)
# model start
formula <- controle_num~1
data.bym <- inla(update(formula, . ~. +
                            f(ID, model = "bym", graph = data_mat_b)), 
                   data = as.data.frame(data),
                   control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, return.marginals.predictor=TRUE),
                   control.predictor = list(compute = TRUE)
)


data$BYM <- unlist(lapply(data.bym$marginals.fitted.values,function(x) {inla.emarginal(function(d) d,x)}))

formula <- controle_num~1

data.besagproper <- inla(update(formula, . ~. +
                          f(ID, model = "besagproper", graph = data_mat_b)), 
                 data = as.data.frame(data),
                 control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, 
                                        return.marginals.predictor=TRUE),
                 control.predictor = list(compute = TRUE)
)


data$besagproper <- unlist(lapply(data.besagproper$marginals.fitted.values,function(x) {inla.emarginal(function(d) d,x)}))

plot(data[,c("BYM")])
plot(data[,c("besagproper")])
plot(data[,c("controle_num")])
class





# binomial models

data$controle_binom = data$controle
data[which(data$controle_binom ==0.5),]$controle_binom = NA
formula <- controle_binom~lg_mix_time_mean + lg_landcover_mean+ fatalities_battles + events_remote_violence 
data.besagproper <- inla(update(formula, . ~. +
                                  f(ID, model = "besagproper", graph = data_mat_b)), 
                         data = as.data.frame(data),
                         family ="binomial",
                         control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, return.marginals.predictor=TRUE),
                         control.predictor = list(compute = TRUE)
)


data$besagproper <- unlist(lapply(data.besagproper$marginals.fitted.values,function(x) {inla.emarginal(function(d) d,x)}))
plot(data[,c("besagproper")])

data.besag<- inla(update(formula, . ~. +
                                  f(ID, model = "besag", graph = data_mat_b)), 
                         data = as.data.frame(data),
                         family ="binomial",
                         control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, return.marginals.predictor=TRUE),
                         control.predictor = list(compute = TRUE)
)


data$besag <- unlist(lapply(data.besag$marginals.fitted.values,function(x) {inla.emarginal(function(d) d,x)}))
plot(data[,c("besag")])

#####################################
## spatial conditional propagation ##
#####################################


formula <- controle_num~ 1

data$controle_binom = data$controle
data[which(data$controle_binom ==0.5),]$controle_binom = NA

data$controle_binom = ifelse(data$controle_binom>0.5,1,0)
data$scl_r1h = scale(data$r1h)
data$region_rain =data$ID
data$region_mix_time=data$ID
data$region_landcover=data$ID
data$scl_mix_time_mean=data$mix_time_mean
data$scl_landcover_mean=data$landcover_mean
data$scl_r1h=data$r1h

formula <- controle_binom~ 1
formula_list_variables = list(
  "id" = update(formula, . ~. +f(ID, model = "besagproper", graph = data_mat_b)),
  "id_rain" = update(formula, . ~. +
           f(ID, model = "bym", graph = data_mat_b) +
           f(region_rain,scl_r1h, model = "besagproper", graph = data_mat_b)),
  "id_rain_mixtime" = update(formula, . ~. +
           f(ID, model = "bym", graph = data_mat_b) +
           f(region_rain,scl_r1h, model = "besagproper", graph = data_mat_b)+
           f(region_mix_time,scl_mix_time_mean, model = "bym", graph = data_mat_b)),
  "id_mixtime" = update(formula, . ~. +
                               f(ID, model = "besagproper", graph = data_mat_b) +
                               f(region_mix_time,scl_mix_time_mean, model = "bym", graph = data_mat_b)),
  
  "id_rain_landcover" =update(formula, . ~. +
                                f(ID, model = "besagproper", graph = data_mat_b) +
                                f(region_rain,scl_r1h, model = "besagproper", graph = data_mat_b)+
                                f(region_landcover,scl_landcover_mean, model = "bym", graph = data_mat_b)),
  "id_rain_mixtime_landcover" =update(formula, . ~. +
           f(ID, model = "bym", graph = data_mat_b) +
           f(region_rain,scl_r1h, model = "besagproper", graph = data_mat_b)+
           f(region_mix_time,scl_mix_time_mean, model = "bym", graph = data_mat_b)+
           f(region_landcover,scl_landcover_mean, model = "bym", graph = data_mat_b))
  )


pdf("D:/DRC/gaussian_process_AOC/plots/model_comparison/model_comparison_inla_variables.pdf",
    width = 8, height = 10)

library(ggplot2)
library(grid)
library(gridExtra)


text_grob <- textGrob(
  "original data",
  x = 0, hjust = 0,
  gp = gpar(fontsize = 12)
)

p <- ggplot(data) +
  geom_sf(aes(fill = controle_binom)) +
  scale_fill_viridis_c() +
  theme_minimal()

grid.arrange(
  text_grob,
  p,
  ncol = 1,
  heights = c(1, 4)
)

for (f_idx in seq_along(formula_list_variables)) {
  
  f <- formula_list_variables[[f_idx]]
  model_name <- names(formula_list_variables)[f_idx]
  
  # ---- Fit model ----
  data.bym <- inla(
    f,
    data = as.data.frame(data),
    control.fixed = list(
      mean.intercept = 0,
      prec.intercept = 1000
    ),
    family = "binomial",
    control.compute = list(
      dic = TRUE, waic = TRUE, cpo = TRUE,
      return.marginals.predictor = TRUE
    ),
    control.predictor = list(
      compute = TRUE,
      link = 1
    )
  )
  
  # ---- Extract fitted values ----
  data$bym <- data.bym$summary.fitted.values$mean
  
  # ---- Extract key stats (DON'T use summary() directly) ----
  dic  <- round(data.bym$dic$dic, 2)
  waic <- round(data.bym$waic$waic, 2)
  
  text_summary <- paste0(
    "Model: ", model_name, "\n\n",
    "DIC: ", dic, "\n",
    "WAIC: ", waic
  )
  
  text_grob <- textGrob(
    text_summary,
    x = 0, hjust = 0,
    gp = gpar(fontsize = 12)
  )
  
  # ---- Plot ----
  p <- ggplot(data) +
    geom_sf(aes(fill = bym)) +
    scale_fill_viridis_c() +
    theme_minimal()
  
  # ---- Draw page (THIS is the key step) ----
  grid.arrange(
    text_grob,
    p,
    ncol = 1,
    heights = c(1, 4)
  )
}

dev.off()

###############################
# with mat w
####################################

formula <- controle_num~ 1

data$controle_binom = data$controle
data[which(data$controle_binom ==0.5),]$controle_binom = NA

data$controle_binom = ifelse(data$controle_binom>0.5,1,0)
data$scl_r1h = scale(data$r1h)
data$region_rain =data$ID
#data$region_mix_time=data$ID
data$region_landcover=data$ID
#data$scl_mix_time_mean=data$mix_time_mean
data$scl_landcover_mean=data$landcover_mean
data$scl_r1h=data$r1h

formula <- controle_binom~ 1
formula_list_variables = list(
  "id" = update(formula, . ~. +f(ID, model = "besagproper", graph = data_mat_w)),
  "id_rain" = update(formula, . ~. +
                       f(ID, model = "bym", graph = data_mat_w) +
                       f(region_rain,scl_r1h, model = "besagproper", graph = data_mat_w)),
  "id_rain_landcover" =update(formula, . ~. +
                                f(ID, model = "besagproper", graph = data_mat_b) +
                                f(region_rain,scl_r1h, model = "besagproper", graph = data_mat_w)+
                                f(region_landcover,scl_landcover_mean, model = "bym", graph = data_mat_w))
)


pdf("D:/DRC/gaussian_process_AOC/plots/model_comparison/model_comparison_inla_variables_mat_w.pdf",
    width = 8, height = 10)

library(ggplot2)
library(grid)
library(gridExtra)


text_grob <- textGrob(
  "original data",
  x = 0, hjust = 0,
  gp = gpar(fontsize = 12)
)

p <- ggplot(data) +
  geom_sf(aes(fill = controle_binom)) +
  scale_fill_viridis_c() +
  theme_minimal()

grid.arrange(
  text_grob,
  p,
  ncol = 1,
  heights = c(1, 4)
)

for (f_idx in seq_along(formula_list_variables)) {
  
  f <- formula_list_variables[[f_idx]]
  model_name <- names(formula_list_variables)[f_idx]
  
  # ---- Fit model ----
  data.bym <- inla(
    f,
    data = as.data.frame(data),
    control.fixed = list(
      mean.intercept = 0,
      prec.intercept = 1000
    ),
    family = "binomial",
    control.compute = list(
      dic = TRUE, waic = TRUE, cpo = TRUE,
      return.marginals.predictor = TRUE
    ),
    control.predictor = list(
      compute = TRUE,
      link = 1
    )
  )
  
  # ---- Extract fitted values ----
  data$bym <- data.bym$summary.fitted.values$mean
  
  # ---- Extract key stats (DON'T use summary() directly) ----
  dic  <- round(data.bym$dic$dic, 2)
  waic <- round(data.bym$waic$waic, 2)
  
  text_summary <- paste0(
    "Model: ", model_name, "\n\n",
    "DIC: ", dic, "\n",
    "WAIC: ", waic
  )
  
  text_grob <- textGrob(
    text_summary,
    x = 0, hjust = 0,
    gp = gpar(fontsize = 12)
  )
  
  # ---- Plot ----
  p <- ggplot(data) +
    geom_sf(aes(fill = bym)) +
    scale_fill_viridis_c() +
    theme_minimal()
  
  # ---- Draw page (THIS is the key step) ----
  grid.arrange(
    text_grob,
    p,
    ncol = 1,
    heights = c(1, 4)
  )
}

dev.off()


####### model kind

#######--define Model type besagproper is the best dor the sparid dim------------------

formula <- controle_binom~ 1
formula_list_model_type = list(
  "id_iid" = update(formula, . ~. +f(ID, model = "iid", graph = data_mat_b)),
  "id_besagproper" = update(formula, . ~. +f(ID, model = "besagproper", graph = data_mat_b)),
  "id_besag" = update(formula, . ~. +f(ID, model = "besag", graph = data_mat_b)),
  "id_bym" = update(formula, . ~. +f(ID, model = "bym", graph = data_mat_b))
)



pdf("D:/DRC/gaussian_process_AOC/plots/model_comparison/model_comparison_inla_model_typ.pdf",
    width = 8, height = 10)

library(ggplot2)
library(grid)
library(gridExtra)


text_grob <- textGrob(
  "original data",
  x = 0, hjust = 0,
  gp = gpar(fontsize = 12)
)

p <- ggplot(data) +
  geom_sf(aes(fill = controle_binom)) +
  scale_fill_viridis_c() +
  theme_minimal()

grid.arrange(
  text_grob,
  p,
  ncol = 1,
  heights = c(1, 4)
)

for (f_idx in seq_along(formula_list_model_type)) {
  
  f <- formula_list_model_type[[f_idx]]
  model_name <- names(formula_list_model_type)[f_idx]
  
  # ---- Fit model ----
  data.bym <- inla(
    f,
    data = as.data.frame(data),
    control.fixed = list(
      mean.intercept = 0,
      prec.intercept = 1000
    ),
    family = "binomial",
    control.compute = list(
      dic = TRUE, waic = TRUE, cpo = TRUE,
      return.marginals.predictor = TRUE
    ),
    control.predictor = list(
      compute = TRUE,
      link = 1
    )
  )
  
  # ---- Extract fitted values ----
  data$bym <- data.bym$summary.fitted.values$mean
  
  # ---- Extract key stats (DON'T use summary() directly) ----
  dic  <- round(data.bym$dic$dic, 2)
  waic <- round(data.bym$waic$waic, 2)
  
  text_summary <- paste0(
    "Model: ", model_name, "\n\n",
    "DIC: ", dic, "\n",
    "WAIC: ", waic
  )
  
  text_grob <- textGrob(
    text_summary,
    x = 0, hjust = 0,
    gp = gpar(fontsize = 12)
  )
  
  # ---- Plot ----
  p <- ggplot(data) +
    geom_sf(aes(fill = bym)) +
    scale_fill_viridis_c() +
    theme_minimal()
  
  # ---- Draw page (THIS is the key step) ----
  grid.arrange(
    text_grob,
    p,
    ncol = 1,
    heights = c(1, 4)
  )
}

dev.off()





######################
## spatial temporal ##
######################
# are not really working yet, i dont think i want to model time per se
# TODO integrate the outcome of the model over all timeseries in the model for each individual timestep
library(spdep)

load("./data/grid_timeseries.RData")
data_st = grid_cntrl_mnth %>%filter(name=="Nord-Kivu")

data_st$r1h_group <- cut(data_st$r1h,
                         breaks = quantile(data_st$r1h, probs = seq(0,1,0.25),na.rm =T),
                         include.lowest = TRUE)




data_for_adjmat= data_st[which(!duplicated(data_st$cell_id)),]
data_for_adjmat = data_for_adjmat[data_for_adjmat$cell_id %in% sort(data_for_adjmat$cell_id),]
data_for_adjmat$ID = 1:nrow(data_for_adjmat)
data_adj_for_join = data_for_adjmat[,c("ID","cell_id")]
data_st = sf::st_drop_geometry(data_st)
data_st = left_join(data_st,data_adj_for_join,by=c("cell_id"))
data_st$cell_id =data_st$ID

data_st$cell_id_r1h <- interaction(data_st$cell_id, data_st$r1h_group, drop = TRUE)
data_st$cell_id_r1h <- as.numeric(factor(data_st$cell_id_r1h))

data_adj <- poly2nb(data_for_adjmat)
data_mat_b<- nb2mat(data_adj, style = "B")


formula <- controle_num~r1h



formula <- controle_num~r1h


data.bym.st <- inla(update(formula, . ~.  +
                             f(cell_id, model = "bym2", graph = data_mat_b)
                           +
                             f(time_step, model = "rw1")
                             ),
                    data = as.data.frame(data_st),
                    control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE,
                                           return.marginals.predictor=TRUE),
                    control.predictor = list(compute = TRUE)
)


#data_st$bym <- unlist(lapply(data.bym.st$marginals.fitted.values,function(x) {inla.emarginal(function(d) d,x)}))

data_st$bym = data.bym.st$summary.fitted.values$mean
data_st = st_as_sf(data_st)
data_st_plot = data_st%>%filter(name =="Nord-Kivu" & time_step %in% c(32,42))
data_st_plot$time_step = as.factor(data_st_plot$time_step)

library(ggplot2)

ggplot(data_st_plot) +
  geom_sf(aes(fill = bym)) +
  facet_wrap(~ time_step) +
  scale_fill_viridis_c() +
  theme_minimal()


data_st$bym = data.bym.st$summary.fitted.values$mean

#####################
## survival models ##
#####################

data(cancer, package="survival")
veteran


veteran$trt <- as.factor(veteran$trt)
levels(veteran$trt) <- c("standard", "test")
veteran$prior <- as.factor(veteran$prior)
levels(veteran$prior) <- c("No", "Yes")

veteran$time.m <- round(veteran$time / 30, 3)


sinla.vet <- inla.surv(veteran$time.m, veteran$status)


train = data%>%filter(!is.na(controle_num))

data[which(is.na(data$controle_num)),]$controle_num = 0
train =data

train_coords = st_drop_geometry(train)





test = inla(formula = controle_num ~ lg_landcover_mean +min_dist_to_rwa + mix_time_mean +
              f(cell_id, model = "rw2d",nrow = ,ncol = ),
     data = train, family = "gaussian")

