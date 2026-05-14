# prepare rain data
# 02 combine data
setwd("D:/DRC/gaussian_process_AOC")

# rain data
#https://data.humdata.org/dataset/cod-rainfall-subnational
#rainfall 1-month rolling aggregation [mm] (r1h)
rain = data.table::fread("./data/cod-rainfall-subnat-full.csv")
admin = read_sf("./data/cod_admin_boundaries.shp",layer = "cod_admin2")[,c("adm2_name","adm2_pcode","adm1_name","adm1_pcode")]

load("./data/cleaned_gdf_territory.RData")
acled_territory_mnth = cleaned_gdf_territory

rain = rain[which(rain$adm_level==2 & rain$date >= min(min(acled_territory_mnth$event_date))),c("date","adm_level","PCODE","r1h","version")]
rain$year = format(rain$date,"%Y")
rain$month = format(rain$date,"%m")
rain = rain%>%group_by(PCODE,year,month)%>%slice_max(date, n = 1, with_ties = FALSE)%>%select(!date)


# rain = left_join(rain,admin, by=c("PCODE" = "adm2_pcode"))
rain$year_mnth = as.numeric(paste0(rain$year,rain$month))

# rain = rain%>%filter(adm1_pcode %in% c("CD54","CD61","CD62"))
# rain_sf = st_as_sf(rain)


#rain=rain[,c("adm_level","PCODE","r1h","year_mnth","geometry")]
rain=rain[,c("adm_level","PCODE","r1h","year_mnth")]

data.table::fwrite(rain, "./data/rain.csv")
