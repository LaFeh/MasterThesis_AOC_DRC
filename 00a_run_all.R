setwd("~/MasterThesis_AOC_DRC")
library(here)
getwd()

do_preparational_calculations = F

# settings for grid

add_streets = F
add_nationalparks =F
add_waterways = F

cell_size     <- 5000

# ============================================================
# 1. IMPLEMENT SETTINGS
# ============================================================


if(add_streets){
  grid_name_street = "_street"
}else {
  grid_name_street = ""
}

if(add_nationalparks){
  grid_name_park = "_park"
}else {
  grid_name_park = ""
}

if(add_waterways){
  grid_name_water = "_water"
}else {
  grid_name_water = ""
}

name_of_grid = paste0("grid_surface_",cell_size,
                      grid_name_water,
                      grid_name_park,
                      grid_name_street,".shp")

######## preparational calculations ##############


if(do_preparational_calculations){
  
  source("./00_01_create_base_provinces.R")
  source("./00_02_travelspeed_remove_streams.R")
  if(add_streets){
    source("./00_03_prepare_street_data.R")
  }
  
}


####### make base grid calculations ##########


rm(list = setdiff(ls(), c("name_of_grid",
                          "add_nationalparks",
                          "add_waterways",
                          "add_streets")))
gc()
source("./01_create_data.R")
rm(list = setdiff(ls(), "name_of_grid"))
gc()

source("./01a_prepare_settlements.R")
rm(list = setdiff(ls(), "name_of_grid"))
gc()


source("./01b_prepare_travelspeed.R")
rm(list = setdiff(ls(), "name_of_grid"))
gc()

source("./01e_prepare_acled.R")
rm(list = setdiff(ls(), "name_of_grid"))
gc()
# source("./01f_prepare_rain.R")
# rm(list = ls())
# gc()

source("./02_combine_data.R")
rm(list = setdiff(ls(), "name_of_grid"))
gc()

source("./04b_create_data_to_predict_all_months.R")
rm(list = ls())
gc()


