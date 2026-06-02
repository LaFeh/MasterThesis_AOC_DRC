
library(sf)
library(leaflet)
#setwd("D:/DRC/gaussian_process_AOC/MTC_shiny_app/MTC_Maps")

drc = read_sf("./data/geoBoundaries-COD.geojson")

relevant_regions_all = read_sf("./data/Congo_relevant_provinces.shp")
relevant_regions = unique(relevant_regions_all[which(relevant_regions_all$name %in% c("Ituri","Sud-Kivu","Nord-Kivu")),])

rwa = read_sf("./data/rwa_geoBoundaries.shp")


