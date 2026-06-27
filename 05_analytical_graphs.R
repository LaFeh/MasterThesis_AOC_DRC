library("INLA")

library("spatstat")
library("sp")
library("sf")

library(sf)
library(dplyr)
library(kernlab)
library(spdep)
library(grid)

setwd("D:/DRC/gaussian_process_AOC")
load("./data/frontline_data_all_previous_mnths_controle_num.RData")


tm = zoo::as.yearmon(as.Date("2023-08-01"),format = "%Y%M")
data = frontline_data_controle_num_all_previous_time%>%
  filter(time == tm)

data = cbind(data,st_coordinates(st_centroid(data$geometry)))

data_adj <- poly2nb(data,snap =3)
data_mat_b<- nb2mat(data_adj, style = "B")

data$ID <- 1:nrow(data)


p_landcover <- ggplot(data) +
  geom_sf(aes(fill = lg_landcover_mean)) +
  scale_fill_viridis_c() +
  theme_minimal()
p_mix_time <- ggplot(data) +
  geom_sf(aes(fill = lg_mix_time_mean)) +
  scale_fill_viridis_c() +
  theme_minimal()
p_walk_time <- ggplot(data) +
  geom_sf(aes(fill = lg_walk_time_mean)) +
  scale_fill_viridis_c() +
  theme_minimal()

data$lg_building_area = log(data$building_area)
p_building_area <- ggplot(data) +
  geom_sf(aes(fill = lg_building_area)) +
  scale_fill_viridis_c() +
  theme_minimal()

data$lg_building_count = log(data$building_count)
p_building_count <- ggplot(data) +
  geom_sf(aes(fill = lg_building_count)) +
  scale_fill_viridis_c() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

grid.arrange(p_landcover,p_mix_time,p_walk_time,p_building_area,
             p_building_count, nrow = 2)



################# plots for presentation

library(sf)
library(dplyr)


setwd("D:/DRC/gaussian_process_AOC")
load("./data/frontline_data_all_previous_mnths_controle_num.RData")
grid = read_sf("./data/grid_surface.shp")
grid = grid%>%filter(name =="Nord-Kivu")

tm = zoo::as.yearmon(as.Date("2025-02-01"),format = "%Y%M")
data = frontline_data_controle_num_all_previous_time%>%
  filter(time == tm)


data$control_binom = data$control
data[which(data$control_binom ==0.5),]$control_binom = 0
data = data[which(!is.na(data$control_binom)),]
data = data[which(data$control_binom==1),]
center = st_transform(st_centroid(st_combine(grid)),4326)
coords <- st_coordinates(center)

rm(frontline_data_controle_num_all_previous_time)
gc()

library(leaflet)

leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  addTiles() %>%
  addPolygons(
    data = st_transform(grid,4326),
    fillColor = "purple",
    fillOpacity = 0.4,
    color = "black",
    weight = 1
  )%>%
  addPolygons(data = st_transform(data,4326),
              fillColor = "yellow",
              fillOpacity = 1,
              color = "black",
              weight = 1) %>%
  setView(
    lng = coords[1],
    lat = coords[2],
    zoom = 8
  )













library(sf)
library(leaflet)


drc = read_sf("./data/geoBoundaries-COD.geojson")
relevant_regions_all = read_sf("./data/Congo_relevant_provinces.shp")
relevant_regions = unique(relevant_regions_all[which(relevant_regions_all$name %in% c("Ituri","Sud-Kivu","Nord-Kivu")),])

rwa = read_sf("./data/rwa_geoBoundaries.shp")

grid = read_sf("./data/grid_surface.shp")
grid_wth_names = read_sf("./data/grid_with_names.geojson")
         
water = read_sf("./data/hotosm_cod_waterways_polygons_geojson/hotosm_cod_waterways_polygons_geojson.geojson")

water = st_transform(water,st_crs(grid))
# crop raster to grid bounding box
water_crop <- st_crop(
  water,
  vect(grid),
  #filename = "./data/water_crop.tif",
  overwrite = TRUE
)





leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  addTiles() %>%  # OpenStreetMap
  addPolygons(
    data = st_transform(drc,4326),
    fillColor = "grey",
    fillOpacity = 0.1,
    color = "black",
    weight = 1
  )%>%  # OpenStreetMap
  addPolygons(
    data = st_transform(rwa,4326),
    fillColor = "grey",
    fillOpacity = 0.1,
    color = "black",
    weight = 1
  )%>%  # OpenStreetMap
  addPolygons(
    data = st_transform(relevant_regions,4326),
    fillColor = "purple",
    fillOpacity = 0.4,
    color = "black",
    weight = 1
  )


leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
  addTiles() %>%  # OpenStreetMap
  addPolygons(
    data = st_transform(drc,4326),
    fillColor = "grey",
    fillOpacity = 0.1,
    color = "black",
    weight = 1
  )%>%  # OpenStreetMap
  addPolygons(
    data = st_transform(rwa,4326),
    fillColor = "grey",
    fillOpacity = 0.1,
    color = "black",
    weight = 1
  )%>%  # OpenStreetMap
  addPolygons(
    data = st_transform(grid_wth_names,4326),
    fillColor = "purple",
    fillOpacity = 0.4,
    color = "black",
    weight = 1
  )%>%  # OpenStreetMap
  addPolygons(
    data = st_transform(relevant_regions,4326),
    fillColor = "purble",
    fillOpacity = 0,
    color = "black",
    weight = 1
  )



################# comparison of rho and tau values
setwd("D:/DRC/gaussian_process_AOC")

load("./tmb_try/leroux_fit_inla_mean_adjusted_matw.RData")
fixed_w = fixed

load("./tmb_try/leroux_fit_inla_mean_adjusted_matb.RData")
fixed_b = fixed

hist(rep[names(rep$value)=="rho"])
hist(,add =T )
summary(rep,"random")
unique(names(rep$value))
k = rep[names(rep$value)=="rho"]
mean(k)

k = rep$value
k[ which(names(k)=="rho")
k[c(1,2,3,4,5,6)]
###########