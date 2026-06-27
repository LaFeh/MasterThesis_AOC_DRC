# 06 compare my results with IPIS results
library(sf)
library(leaflet)

setwd("D:/DRC/gaussian_process_AOC")

load(file = "./tmb_try/leroux_with_priors_wo_constraint_mat_w.RData")
mat_w_rep = rep
data <- readRDS("./data/inla_data/data_prepared_for_inla.RData")

data$phi_w = mat_w_rep$par.random
data$phi_w_plogis = plogis(mat_w_rep$par.random)
data$p_w = plogis(mat_w_rep$par.fixed["beta"]+mat_w_rep$par.random)


#data_only_aoc= data[which(data$p_w>mean(data$p_w)),]
data_only_aoc= data[which(mat_w_rep$par.random>mean(mat_w_rep$par.random)),]
data_only_aoc = st_union(data_only_aoc)
data_only_aoc = st_boundary(data_only_aoc)

grid = read_sf("./data/grid_surface.shp")
grid_not_estimated = st_union(grid[which(grid$name != "Nord-Kivu"),])



ipis_map = read_sf("./data/IPIS_maps/2025/2025_02_feb_M23_aoi_ipis.gpkg")



center = st_transform(st_centroid(st_combine(data)),4326)
coords <- st_coordinates(center)



leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  addTiles() %>%  # OpenStreetMap
  addPolygons(
    data = st_transform(ipis_map,4326),
    fillColor = "purple",
    fillOpacity = 0.3,
    color = "black",
    weight = 1
  )%>%  addPolygons(
    data = st_transform(data_only_aoc,4326),
    fillColor = "orange",
    fillOpacity = 0.3,
    color = "black",
    weight = 1
   )%>%  addPolygons(
  #   data = st_transform(data[which(!is.na(data$control_binom)),],4326),
  #   fillColor = "yellow",
  #   fillOpacity = 0.6,
  #   color = "black",
  #   weight = 1
  # )%>%addPolygons(
    data = st_transform(grid_not_estimated,4326),
    fillColor = "white",
    fillOpacity = 0.6,
    color = "black",
    weight = 1
  )%>%
  setView(lng = coords[1], lat = coords[2],zoom = 8)



## without ipis and without acled_



leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  addTiles() %>%  # OpenStreetMap
  addPolygons(
    data = st_transform(data_only_aoc,4326),
    fillColor = "orange",
    fillOpacity = 0.3,
    color = "black",
    weight = 1
  )%>%  addPolygons(
    data = st_transform(grid_not_estimated,4326),
    fillColor = "white",
    fillOpacity = 0.6,
    color = "black",
    weight = 1
  )%>%
  setView(lng = coords[1], lat = coords[2],zoom = 8)




## without ipis estimation but results:
library(leaflet)
library(sf)
library(viridisLite)

# Create viridis color palette
pal <- colorNumeric(
  palette = viridis::viridis(256),
  domain = data$phi_w_plogis,
  na.color = "transparent",
  reverse = F
)

pal_rev <- leaflet::colorNumeric(
  palette = "viridis",
  domain = data$phi_w_plogis,
  na.color = "transparent",
  reverse = T
)

leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  addTiles() %>%  # OpenStreetMap
  addPolygons(
    data = st_transform(data, 4326),
    fillColor = ~pal(phi_w_plogis),
    fillOpacity = 1,
    color = "black",
    weight = 1
  ) %>%
  addPolygons(
    data = st_transform(grid_not_estimated, 4326),
    fillColor = "white",
    fillOpacity = 0.6,
    color = "black",
    weight = 1
  ) %>%
  addLegend(
    position = "bottomleft",
    pal = pal_rev,
    values = sort(data$phi_w_plogis,decreasing = T),
    labFormat = labelFormat(transform = function(x) sort(x, decreasing = T)),
    title = NULL,
    opacity = 1
  ) %>%
  setView(
    lng = coords[1],
    lat = coords[2],
    zoom = 8
  )

### binary


load(file = "./tmb_try/leroux_with_priors_wo_constraint_mat_b.RData")
mat_w_rep = rep
data       <- readRDS("./data/inla_data/data_prepared_for_inla.RData")

data$phi_w = mat_w_rep$par.random
data$p_w = plogis(mat_w_rep$par.fixed["beta"]+mat_w_rep$par.random)


data_only_aoc= data[which(data$p_w>mean(data$p_w)),]
data_only_aoc = st_union(data_only_aoc)

hist(data$p_w)
mean(data$p_w)

library(ggplot2)
p <- ggplot2::ggplot(data[which(data$p_w!=0),]) +
  geom_sf(aes(fill =  control_binom)) +
  scale_fill_viridis_c() +
  theme_minimal()

p


ipis_map = read_sf("./data/IPIS_maps/2025/2025_01_jan_M23_aoi_ipis.gpkg")





leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  addTiles() %>%  # OpenStreetMap
  addPolygons(
    data = st_transform(ipis_map,4326),
    fillColor = "yellow",
    fillOpacity = 0.2,
    color = "black",
    weight = 1
  )%>%  addPolygons(
    data = st_transform(data_only_aoc,4326),
    fillColor = "red",
    fillOpacity = 0.2,
    color = "black",
    weight = 1
  )%>%  addPolygons(
    data = st_transform(data[which(!is.na(data$control_binom)),],4326),
    fillColor = "yellow",
    fillOpacity = 0.6,
    color = "black",
    weight = 1
  )
