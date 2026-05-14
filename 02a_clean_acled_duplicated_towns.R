# 02a - deal with duplicated names

library(dplyr)
library(data.table)
library(R.utils)
library(sf)

setwd("D:/DRC/gaussian_process_AOC")

duplicated_towns = readRDS("./data/duplicated_towns.rds")

load(file = "./data/acled_conflict_mnth.RData")

load(file="./data/acled_territory_mnth.RData")

duplicated_towns = st_as_sf(duplicated_towns,coords =c("longitude","latitude"), crs = 4326)

duplicated_towns = st_transform(duplicated_towns,st_crs(acled_territory_mnth))
## 1. territory 
## use geoprecision and double names
# geoprecision: 1, 2, or 3; with 1 being the most precise.

#table(acled_territory_mnth$geo_prcsn)

territory_low_precision = acled_territory_mnth%>%filter(geo_prcsn ==1)
test = territory_low_precision[territory_low_precision$location %in% unlist(duplicated_towns$all_names),]

grid = read_sf("./data/grid_surface.shp")
grid = st_transform(grid,st_crs(acled_territory_mnth))
towns_ndumba = duplicated_towns%>%filter(name=="Ndumba")
plot(grid$geometry)
plot(towns_ndumba$geometry,col="red",add =T)

sake = duplicated_towns%>%filter(name =="Sake")
ndumba = duplicated_towns%>%filter(name =="Ndumba")

#plot(sake$geometry,col="blue",add =T)
plot(c(ndumba$geometry,sake$geometry),col=c(rep("red",6),rep("blue",4)))
plot(grid$geometry,add =T)
plot(sake$geometry,col="blue",add =T)
plot(ndumba$geometry,col="red",add =T)
st_crs(test)==st_crs(grid)

plot(duplicated_towns%>%filter(name=="Ndumba")%>%select(geometry))
# 2. conflict

conflict_low_precision = acled_territory_mnth%>%filter(geo_prcsn %in% c(1))
test = conflict_low_precision[conflict_low_precision$location %in% unlist(duplicated_towns$all_names),]


plot(grid$geometry)
plot(test$geometry,col="red",add =T)


st_crs(test)==st_crs(grid)


