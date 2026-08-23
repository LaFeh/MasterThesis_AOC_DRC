if (!require(googlesheets4)) install.packages("googlesheets4")
if (!require(googledrive)) install.packages("googledrive")
if (!require(tidyverse)) install.packages("tidyverse")

library("googlesheets4")
library("googledrive")
library("tidyverse")

googledrive::drive_auth()

drive_download("https://drive.google.com/file/d/1q822hSX79qeJYezHSw5nSsRQ7c-FXz0L")

unzip("~/MasterThesis_AOC_DRC/data/GRID3_COD_mix_travel_time_friction_surface_v1.zip")


drive_download("https://drive.google.com/file/d/1RbbGHL5s-Wpaimw0xBCx_tjj4wmZcU1T/")

unzip("~/MasterThesis_AOC_DRC/data/hotosm_cod_roads_osm_gpkg.zip")

drive_download("https://drive.google.com/file/d/1a1HqMPQrovrjBv-FrQSelUDkiIE9Va9u/")

unzip("~/MasterThesis_AOC_DRC/data/grid3_cod_settlement_extents_v4.zip")

drive_download("https://drive.google.com/file/d/1mbB5AK84PzdviOtMwH_xusZgwnYa2I1I/")

unzip("~/MasterThesis_AOC_DRC/data/grid3_cod_settlement_extents_v4.zip")


drive_download("https://drive.google.com/file/d/1yU9sFFqCdWaRnFbnWVQZ6Ndzb4fEPlaX/")


drive_download("https://drive.google.com/file/d/1ALlNgs-dIJjjVaXhyaYhWUdn10cn_7gw/")

unzip("~/MasterThesis_AOC_DRC/data/cod_admin_boundaries.shp.zip")
