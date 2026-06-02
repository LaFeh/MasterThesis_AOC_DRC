library(viridis)
library(INLA)
library(ggplot2)


setwd("D:/DRC/gaussian_process_AOC")
data = readRDS("./data/inla_data/data_prepared_for_inla.RData")

data_mat_b = readRDS("./data/inla_data/data_mat_b.RData")
data_mat_w = readRDS("./data/inla_data/data_mat_w_mixed_time.RData")

### compare model priors and neighbourhood matrices




## ---- plots for comparison --------------------

plot(data[,c("lg_mix_time_mean")], pal = viridis)


#####################
## model prior and nb
####################

######## bayesCar##########################

library(CARBayes)

data$intercept =1

base_formula <- as.formula(controle_binom~1)
model.spatial <- S.CARbym(as.formula(base_formula), data = data,family="binomial",trials =rep(1,nrow(data)),
                                    W = data_mat_w, burnin = 20000, n.sample = 100000, thin = 10)
data$carbym = model.spatial$fitted.values
model.frame(base_formula, data = data)

CARBayes::

p <- ggplot(data) +
  geom_sf(aes(fill =  carbym)) +
  scale_fill_viridis_c() +
  theme_minimal()

p

model.spatial_b <- S.CARbym(as.formula(base_formula), data = data,family="binomial",trials =rep(1,nrow(data)),
                          W = data_mat_b, burnin = 20000, n.sample = 100000, thin = 10)


data$carbym_b = model.spatial_b$fitted.values

p <- ggplot(data) +
  geom_sf(aes(fill =  carbym_b)) +
  scale_fill_viridis_c() +
  theme_minimal()

p
data[which(is.na(data$lg_mix_time_mean)),"lg_mix_time_mean"] = 0
base_formula <- as.formula(controle_binom~1+lg_mix_time_mean)
formula_list_model_type = list(
  "id_car_bym_b" =  'S.CARbym(as.formula(base_formula), data = data,family="binomial",trials =rep(1,nrow(data)),
                            W = data_mat_b, burnin = 2000, n.sample = 10000, thin = 5)',
  "id_car_bym_w" =  'S.CARbym(as.formula(base_formula), data = data,family="binomial",trials =rep(1,nrow(data)),
                            W = data_mat_w, burnin = 2000, n.sample = 10000, thin = 5)',
  "id_car_leroux_b" =  'S.CARleroux(as.formula(base_formula), data = data,family="binomial",trials =rep(1,nrow(data)),
                            W = data_mat_b, burnin = 2000, n.sample = 10000, thin = 5)',
  "id_car_leroux_w" =  'S.CARleroux(as.formula(base_formula), data = data,family="binomial",trials =rep(1,nrow(data)),
                            W = data_mat_w, burnin = 2000, n.sample = 10000, thin = 5)'
)


data.model <- eval(parse_expr(formula_list_model_type["id_car_leroux_b"][[1]]))

data.model$mcmc.info
data.model$


pdf("D:/DRC/gaussian_process_AOC/plots/model_comparison/model_comparison_CARBayes_model_prior_nb.pdf",
    width = 8, height = 10)

library(ggplot2)
library(grid)
library(gridExtra)

library(rlang)

# initial plot with added data
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
  

  model_name <- names(formula_list_model_type)[f_idx]
  
  # ---- Fit model ----
  data.model <- eval(parse_expr(formula_list_model_type[f_idx][[1]]))
  
  # ---- Extract fitted values ----
  data[,model_name] <- data.model$fitted.values
  
  text_summary = model.spatial$summary.results
  
  text_grob <- textGrob(
    text_summary,
    x = 0, hjust = 0,
    gp = gpar(fontsize = 12)
  )
  
  # ---- Plot ----
  p <- ggplot(data) +
    geom_sf(aes(fill =  .data[[model_name]])) +
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


#######--define Model type besagproper is the best dor the sparid dim------------------
base_formula <- as.formula(controle_binom~ 1)

# model generic1
## we will use replicated samples in our testing
nrep = 5
## make life easy; use dense matrix algebra
d = 1.0
tau = 1.0

graph = system.file("demodata/germany.graph", package="INLA")
g = inla.read.graph(graph)
Q = matrix(0, g$n, g$n)
diag(Q) = tau * (d + g$nnbs)
for(i in 1:g$n) {
  if (g$nnbs[i] > 0) {
    Q[i, g$nbs[[i]]] = -tau
    Q[g$nbs[[i]], i] = -tau
  }
}
R = chol(Q) ## ’chol’ returns the upper triangular
## simulate data with replications
y = c()

rno = rnorm(g$n)
for(i in 1:nrep) {
  y = c(y, backsolve(R, rno))
}

i = rep(1:g$n, nrep)
replicate = rep(1:nrep, each = g$n)

formula = y ~ f(i, model="besagproper", 
                replicate=replicate,
                Cmatrix = Q,
                hyper = list(diag = list(param = c(1, 1)))) -1
## use ’exact’ observations, so we fix the noise precisin to a high
## value
r = inla(formula,
         data = data.frame(y, i, replicate),
         family = "gaussian",
         control.family = list(
           hyper = list(
             prec = list(
               initial = 10,
               fixed=TRUE))))







###

W = data_mat_b
W = W/apply(W,1,sum)
eigen_data_mat_b = eigen(data_mat_b)$values
eigen_data_mat_w = eigen(data_mat_w)$values

#W = W/max(eigen_data_mat_b)

fit <- inla(
  controle_binom ~ 
    f(ID,
      model = "generic1",
      Cmatrix = W, constr=TRUE,
      hyper = list(
        prec = list(
          prior="pc.prec",
          param=c(1,0.01)
        ))),
  family = "binomial",
  data = data
)


data$generic1 = fit$summary.fitted.values$mean

p <- ggplot(data) +
  geom_sf(aes(fill =  generic1)) +
  scale_fill_viridis_c() +
  theme_minimal()

p

# model slm
betaprec <- .0001

X = as.matrix(model.matrix(base_formula,data))
Q.beta = Diagonal(n=ncol(X), betaprec)
zero.variance = list(prec=list(initial = 25, fixed=TRUE))

lw <- mat2listw(data_mat_w,style ="W")
e = eigenw(lw)
re.idx = which(abs(Im(e)) < 1e-6)
rho.max = 1/max(Re(e[re.idx]))
rho.min = 1/min(Re(e[re.idx]))
rho = mean(c(rho.min, rho.max))

## Priors on the hyperparameters
hyper = list(
  prec = list(
    prior = "loggamma",
    param = c(0.01, 0.01)), 
  rho = list(
    initial=0,
    prior = "logitbeta",
    param = c(1,1)))

f(ID, model="slm",
  args.slm=list(
    rho.min = rho.min,
    rho.max = rho.max,
    W=data_mat_w,
    X=as.matrix(X),
    Q.beta=Q.beta))

slmm1 <- inla(formula = controle_binom ~ 1 +
                 f(ID, model="slm",
                   args.slm=list(
                     rho.min = rho.min,
                     rho.max = rho.max,
                     W=data_mat_w,
                     X=as.matrix(X),
                     Q.beta=Q.beta),
                   hyper=hyper),
               data=data, 
               family="binomial",
               control.compute=list(dic=TRUE, cpo=TRUE)
)
data$slmm1 = slmm1$summary.fitted.values$mean
data$linear_pred_prob <- plogis(slmm1$summary.linear.predictor$mean)


slmm1$summary.fitted.values$mean ==slmm1$summary.linear.predictor
p <- ggplot(data) +
  geom_sf(aes(fill =  linear_pred_prob)) +
  scale_fill_viridis_c() +
  theme_minimal()


base_formula <- as.formula(controle_binom~ 1)
formula = controle_binom~ 1
formula_list_model_type = list(
  # "id_iid_matb" = update(formula, . ~. +f(ID, model = "iid", graph = data_mat_b)),
  # "id_besagproper_matb" = update(formula, . ~. +f(ID, model = "besagproper", graph = data_mat_b)),
  # "id_besag_matb" = update(formula, . ~. +f(ID, model = "besag", graph = data_mat_b)),
  # "id_bym_matb" = update(formula, . ~. +f(ID, model = "bym", graph = data_mat_b)),
  # "id_slm_matb" = f(base_formula + ID,
  #               model = "slm",
  #               args.slm = list(
  #                 W = data_mat_b,
  #                 X = X,
  #                 Q.beta = Q.beta,
  #                 rho.min = -0.99,
  #                 rho.max = 0.99
  #               )),
  "id_iid_matw" = update(formula, . ~. +f(ID, model = "iid", graph = data_mat_w)),
  "id_besagproper_matw" = update(formula, . ~. +f(ID, model = "besagproper", graph = data_mat_w)),
  "id_besagproper2_matw" = update(formula, . ~. +f(ID, model = "besagproper2", graph = data_mat_w)),
  "id_besag_matw" = update(formula, . ~. +f(ID, model = "besag", graph = data_mat_w)),
  "id_bym_matw" = update(formula, . ~. +f(ID, model = "bym", graph = data_mat_w))
  # "id_slm_matw" = f(base_formula + ID,
  #                   model = "slm",
  #                   args.slm = list(
  #                     W = data_mat_w,
  #                     X = X,
  #                     Q.beta = Q.beta,
  #                     rho.min = -0.99,
  #                     rho.max = 0.99))
)

pdf("D:/DRC/gaussian_process_AOC/plots/model_comparison/model_comparison_inla_model_prior_nb.pdf",
    width = 8, height = 10)

library(ggplot2)
library(grid)
library(gridExtra)



# initial plot with added data
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
  data.model <- inla(
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
  data[,model_name] <- data.model$summary.fitted.values$mean
  
  # ---- Extract key stats (DON'T use summary() directly) ----
  dic  <- round(data.model$dic$dic, 2)
  waic <- round(data.model$waic$waic, 2)
  
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
    geom_sf(aes(fill =  .data[[model_name]])) +
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

## plot compare besagproper mat_b and mat_w
library(patchwork)

p1 <- ggplot(data) +
  geom_sf(aes(fill =id_besagproper_matb)) +
  scale_fill_viridis_c() +
  ggtitle("id_besagproper_matb") +
  theme_minimal()

p2 <- ggplot(data) +
  geom_sf(aes(fill = id_besagproper_matw)) +
  scale_fill_viridis_c() +
  ggtitle("id_besagproper_matw") +
  theme_minimal()

(p1 + p2) + plot_layout(guides = "collect")


all(data$id_besagproper_matb == data$id_besagproper_matw)
identical(data_mat_b@x, data_mat_w@x)

INLA::inla.read.graph(data_mat_w)$cc


#####################################
## spatial conditional propagation ##
#####################################

## data preprocessing

data$controle_binom = ifelse(data$controle_binom>0.5,1,0)
data$scl_r1h = scale(data$r1h)
data$region_rain =data$ID
data$region_mix_time=data$ID
data$region_landcover=data$ID
data$scl_mix_time_mean=data$mix_time_mean
data$scl_landcover_mean=data$landcover_mean
data$scl_r1h=data$r1h


formula <- controle_num~ 1



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

