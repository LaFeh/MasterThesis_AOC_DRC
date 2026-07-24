if (!require(googlesheets4)) install.packages("googlesheets4")
if (!require(googledrive)) install.packages("googledrive")
if (!require(tidyverse)) install.packages("tidyverse")

library("googlesheets4")
library("googledrive")
library("tidyverse")

googledrive::drive_auth()

drive_download("https://drive.google.com/file/d/1q822hSX79qeJYezHSw5nSsRQ7c-FXz0L")

unzip("/home/laura/MasterThesis_AOC_DRC/data/GRID3_COD_mix_travel_time_friction_surface_v1.zip")
