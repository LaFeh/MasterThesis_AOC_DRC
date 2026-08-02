setwd("~/MasterThesis_AOC_DRC")
library(here)
getwd()

# 1.1.1. made some changes that slow down the code considerably.
# remotes::install_version(
#   "sf",
#   version = "1.1-0",
#   repos = "https://cloud.r-project.org"
# )
if (packageVersion("sf") != "1.1.0"){
  stop("Please use packageversion 1.1.0 for sf")
}

# 1.9-1 supports different Coordinate systems EPSG:102022 doesnot work like this anymore
# remotes::install_version(
#   "terra",
#   version = "1.9-1",
#   repos = "https://cloud.r-project.org"
# )

if(packageVersion("terra") != "1.9.1"){
  stop("Please use package version 1.9.1 for terra")
}

do_preparational_calculation = F

# settings for grid
add_streets = T
if(add_streets){
  grid_name_street = "_street"
}else {
  grid_name_street = ""
}
cell_size     <- 5000
name_of_grid = paste0("grid_surface_",cell_size,grid_name_street,".shp")



######## preparational calculations ##############


if(do_preparational_calculations){
  
  source("./00_01_create_base_provinces.R")
  source("./00_02_travelspeed_remove_streams.R")
  if(add_streets){
    source("./00_03_prepare_street_data.R")
  }
  
}


####### make base grid calculations ##########


rm(list = setdiff(ls(), "name_of_grid"))
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