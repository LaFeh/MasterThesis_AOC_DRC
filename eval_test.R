
source("./00b_helper_create_grid_name.R")
source("./05_model/05a_a_helper_run_model.R")
source("./05_model/05a_b_helper_parameter_grid.R")
source("./04b_a_helper_functions.R")


cell_size = 3000 
gridname <- create_grid_name(
  add_streets = T,
  add_nationalparks = T,
  add_waterways = T,
  cell_size = cell_size
)

datex = "202510"

load(paste0("./data/data_for_prediction/",gsub(".shp","",gridname),"/",datex,"_events.RData"))


ipis = get_ipis_map("202510")
ipis =st_transform(ipis,st_crs(data))

plot(data[which(data$control ==1),]$geometry)
load("./data/acled_territory_mnth.RData")

data_sudkivu = data[which(data$name =="Sud-Kivu"),]
data_sudkivu_street = data_sudkivu[which(data$surface =="road"),]
library(ggplot2)
k <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = st_as_sf(data_sudkivu),
    fill = NA,
    color = "grey",
    #linewidth = 0.01,
    #alpha = 0.5
  ) +
  ggplot2::geom_sf(
    data = st_as_sf(acled_territory_mnth),
    fill = "blue"
  ) +
  ggplot2::geom_sf(data = st_as_sf(ipis), alpha = 0.5,fill ="red")+
  ggplot2::theme_minimal()#


library(ggplot2)
k <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = st_as_sf(data_sudkivu_street),
    fill = NA,
    color = "black",
  )
k


unique(data_sudkivu$surface)
all(st_is_empty(data_sudkivu_street))

load("~/MasterThesis_AOC_DRC/data/frontline_data_all_mnths_quality_TRUE_grid_surface_3000_water_park_street.RData")

grid = read_sf(paste0("./data/",gridname))


library(ggplot2)
library(sf)

k <- ggplot() +
  geom_sf(
    data = st_as_sf(data_sudkivu),
    fill = NA,
    color = "grey",
    linewidth = 0.2
  ) +
  geom_sf(
    data = st_as_sf(acled_territory_mnth),
    color = "blue",
    size = 1
  ) +
  theme_minimal()

k

name <- paste0(
  date,
  "_ipis_aoc_evaluation.png"
)

ggplot2::ggsave(
  file.path(
    model_dir,
    "plots",
    name
  ),
  ipis_aoc_evaluation
)
  