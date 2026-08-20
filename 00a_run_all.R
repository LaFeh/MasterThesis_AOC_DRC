setwd("~/MasterThesis_AOC_DRC")
library(here)
getwd()

do_preparational_calculations = T

# settings for grid

add_streets = F
add_nationalparks =T
add_waterways = T

cell_size     <- 3000

source("./00b_helper_create_grid_name.R")
name_of_grid = create_grid_name(add_waterways = add_waterways,
                 add_nationalparks = add_nationalparks,
                 add_streets = add_streets,
                 cell_size = cell_size)

# ============================================================
# 1. IMPLEMENT SETTINGS
# ============================================================


######## preparational calculations ##############


if(do_preparational_calculations){
  
  #source("./00_01_create_base_provinces.R")
  #source("./00_02_travelspeed_remove_streams.R")
  if(add_streets){
    source("./00_03_prepare_street_data.R")
    create_street_splitted(target_length = cell_size)
  }
  
}


####### make base grid calculations ##########


rm(list = setdiff(ls(), c("name_of_grid",
                          "add_nationalparks",
                          "add_waterways",
                          "add_streets","cell_size")))


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


