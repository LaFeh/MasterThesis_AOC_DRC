#INLAR LEROUX
#install.packages("INLABMA")
library(INLABMA)

setwd("D:/DRC/gaussian_process_AOC")
data = readRDS("./data/inla_data/data_prepared_for_inla.RData")

data_mat_b = readRDS("./data/inla_data/data_mat_b.RData")
data_mat_w = readRDS("./data/inla_data/data_mat_w_mixed_time.RData")


# test= data[which(data$controle_binom==1),]
test[4,"cell_id"]
 
data_mat_w[4,3] = 0
data_mat_w[4,13] = 1

data[c(4,which(data_mat_w[4,]!=0)),"mix_time_mean_scaled"]
data$idx = data$ID

data[1287,"controle_binom"]=0
data_mat_w[1287,1:1000] = 1
data_mat_w[1:1000,1287] = 1

data_mat_w[1287,1001:dim(data_mat_w)[1]] = 0.001
data_mat_w[1001:dim(data_mat_w)[1],1287] = 0.001
diag(data_mat_w) = 0

leroux_inla.model = leroux.inla(as.formula(controle_binom ~ 1), d=data, data_mat_w, lambda = 0.85)



data$leroux_inla = leroux_inla.model$summary.fitted.values$mean
#save(leroux.model,file = "D:/DRC/gaussian_process_AOC/leroux_model.RData")


#data$leroux.model = leroux.model$fitted.values

p <- ggplot(data) +
  geom_sf(aes(fill =  leroux_inla)) +
  scale_fill_viridis_c() +
  theme_minimal()

p
