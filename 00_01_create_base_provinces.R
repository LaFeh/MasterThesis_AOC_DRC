# 00_01_create_base grid

setwd(here::here())

library(sf)
library(dplyr)
library(terra)

# ============================================================
# 1. LOAD AND PROJECT STUDY AREA
# ============================================================

relevant_regions <- read_sf("./data/congo_relevant_provinces/")
relevant_regions <- relevant_regions |>
  filter(name %in% c("Ituri", "Sud-Kivu", "Nord-Kivu")) |>
  distinct()

relevant_regions = st_transform(relevant_regions,sf::st_crs("ESRI:102022"))
drc_m <- relevant_regions |>
  vect()# |>

write_sf(relevant_regions,"./data/base_provinces.shp")
