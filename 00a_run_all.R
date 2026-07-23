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

rm(list = ls())
gc()
source("./01_create_data.R")
rm(list = ls())
gc()

source("./01a_prepare_settlements.R")
rm(list = ls())
gc()
source("./01b_prepare_travelspeed.R")
rm(list = ls())
gc()
source("./01c_prepare_distance_to_rwa.R")
rm(list = ls())
gc()

source("./01e_prepare_acled.R")
rm(list = ls())
gc()
source("./01f_prepare_rain.R")
rm(list = ls())
gc()
source("./02_combine_data.R")
rm(list = ls())
gc()

source("./04b_create_data_to_predict_all_months.R")
rm(list = ls())
gc()