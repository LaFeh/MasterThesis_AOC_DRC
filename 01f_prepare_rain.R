# prepare rain data
# 02 combine data

library(sf)
library(dplyr)
library(terra)
library(exactextractr)

setwd("D:/DRC/gaussian_process_AOC")

library("raster")


grid = read_sf("./data/grid_surface.shp")

years <- 2020:2025
months <- sprintf("%02d", 1:12)


rain_per_mnth = data.frame()
for(y in years){
  for (m in months){
    
    file_name_dest <- paste0(
      "chirps_",
      y, "_", m,
      ".tif.gz"
    )
    
    dest <- file.path("./data/chirps_monthly/", file_name_dest)
    
    tryCatch({rain = raster(dest)},error = function(e){next()})
      if (st_crs(grid)!=st_crs(rain)){
        grid=st_transform(grid,st_crs(rain))
      }
    
    
    # crop raster to grid bounding box
    rain_crop <- raster::crop(
      rain,
      grid,
      #filename = "./data/rain_crop.tif",
      #overwrite = TRUE
    )
    st_crs(grid) == st_crs(rain_crop)
    
    summary <- exact_extract(rain_crop, grid, 'mean')
    
    grid$rain_mean <- summary
    grid$lg_rain_mean <- log(summary)
    
    #plot(grid[,"rain_mean"])
    
    grid_wo_geom = st_drop_geometry(grid)
    grid_wo_geom$date = paste0(y,"-",m)
    rain_per_mnth = rbind(rain_per_mnth,grid_wo_geom)
    
  }
  
}

data.table::fwrite(rain_per_mnth, "./data/rain_per_month.csv")




# rain data
#https://data.humdata.org/dataset/cod-rainfall-subnational
#rainfall 1-month rolling aggregation [mm] (r1h)
# rain = data.table::fread("./data/cod-rainfall-subnat-full.csv")
# admin = read_sf("./data/cod_admin_boundaries.shp",layer = "cod_admin2")[,c("adm2_name","adm2_pcode","adm1_name","adm1_pcode")]
# 
# load("./data/cleaned_gdf_territory.RData")
# acled_territory_mnth = cleaned_gdf_territory
# 
# rain = rain[which(rain$adm_level==2 & rain$date >= min(min(acled_territory_mnth$event_date))),c("date","adm_level","PCODE","r1h","version")]
# rain$year = format(rain$date,"%Y")
# rain$month = format(rain$date,"%m")
# rain = rain%>%group_by(PCODE,year,month)%>%
#   slice_max(date, n = 1, with_ties = FALSE)
# rain$date = NULL
# 
# 
# # rain = left_join(rain,admin, by=c("PCODE" = "adm2_pcode"))
# rain$year_mnth = as.numeric(paste0(rain$year,rain$month))
# 
# # rain = rain%>%filter(adm1_pcode %in% c("CD54","CD61","CD62"))
# # rain_sf = st_as_sf(rain)
# 
# 
# #rain=rain[,c("adm_level","PCODE","r1h","year_mnth","geometry")]
# rain=rain[,c("adm_level","PCODE","r1h","year_mnth")]
# 
# data.table::fwrite(rain, "./data/rain.csv")
