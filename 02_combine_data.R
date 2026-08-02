# 02 combine data

library(sf)
library(dplyr)



grid = read_sf(paste0("./data/",name_of_grid))

load(file = "./data/acled_conflict_mnth.RData")
load(file="./data/acled_territory_mnth.RData")
grid = st_transform(grid,st_crs(acled_territory_mnth))
acled_conflict_mnth = st_transform(acled_conflict_mnth,st_crs(acled_territory_mnth))


##### settlements ########################
grid_settlements = data.table::fread("./data/grid_settlements.csv")
grid_settlements = grid_settlements[,c("cell_id","building_count","building_area")]

grid = left_join(grid,grid_settlements,by ="cell_id")
grid[which(is.na(grid$building_count)),]$building_count = 0
grid[which(is.na(grid$building_area)),]$building_area = 0
rm(grid_settlements)

#### mix time walk time ########################
grid_mix_time = data.table::fread("./data/grid_mix_time.csv",sep =",")
grid = left_join(grid,grid_mix_time,by ="cell_id")
#plot(grid[which(is.na(grid$mix_time_mean)),])
grid = grid[-which(is.na(grid$mix_time_mean)),]
rm(grid_mix_time)



### acled data year mnth #######################
acled_territory_mnth$year_mnth = as.numeric(acled_territory_mnth$year_mnth)
acled_conflict_mnth$year_mnth = as.numeric(acled_conflict_mnth$year_mnth)

# rwa distance ###########################
# dist_rwa = data.table::fread("./data/distance_rwanda.csv")
# grid = left_join(grid,dist_rwa, by="cell_id")
# 


# 1st. create data per month year ####################################################

yrs = unique(substr(acled_conflict_mnth$year_mnth,1,4))
mnths = unique(substr(acled_conflict_mnth$year_mnth,5,7))

date_combinations = cross_join(as_tibble(yrs),as_tibble(mnths))

date_combinations$time_step = 1:nrow(date_combinations)
date_combinations$year_mnth = as.numeric(paste0(date_combinations$value.x,date_combinations$value.y))

grid_yr_mnth <- merge(grid, date_combinations[c("year_mnth","time_step")], by = NULL)

# acled territory

acled_territory_grid = st_join(acled_territory_mnth,grid,join = st_within, left = TRUE)

aoc_per_cell = acled_territory_grid%>%
  dplyr::group_by(year_mnth,cell_id)%>%
  dplyr::summarise(non_state_actor = sum(controle == "non-state actor"),
                   government = sum(controle == "government"),
                   controle_num = mean(controle_num)
                   )%>%
  mutate(control = if_else((non_state_actor+government)!=0, non_state_actor/(non_state_actor+government), 0.5))

aoc_per_cell = aoc_per_cell[,!colnames(aoc_per_cell) %in% c("non_state_actor","government")]
aoc_per_cell = st_drop_geometry(aoc_per_cell)

grid_cntrl_mnth = full_join(grid_yr_mnth,aoc_per_cell, by =c("cell_id","year_mnth"))

grid_cntrl_mnth[which(is.na(grid_cntrl_mnth$controle)),]$controle=0.5 #"neutral"

acled_conflict_mnth= st_join(acled_conflict_mnth,grid,join = st_within, left = FALSE)

acled_conflict_per_cell = acled_conflict_mnth%>%st_drop_geometry()%>%ungroup()%>%
  dplyr::group_by(year_mnth,cell_id)%>%
  dplyr::summarise(events_violence_civilian = sum(events_violence_civilian,na.rm =T),
                   events_battles = sum(events_Battles,na.rm =T),
                   events_strategic_developments = sum(events_strategic_developments,na.rm =T),
                   events_remote_violence = sum(events_remote_violence,na.rm =T),
                   fatalities_violence_civilian = sum(fatalities_violence_civilian,na.rm =T),
                   fatalities_battles = sum(fatalities_Battles,na.rm =T),
                   fatalities_strategic_developments = sum(fatalities_strategic_developments,na.rm =T),
                   fatalities_remote_violence = sum(fatalities_remote_violence,na.rm =T),
                   )

acled_conflict_per_cell$total_fatalities = apply(acled_conflict_per_cell[,grep("fatalities",colnames(acled_conflict_per_cell))],1,sum,na.rm =T)
acled_conflict_per_cell$total_events = apply(acled_conflict_per_cell[,grep("events",colnames(acled_conflict_per_cell))],1,sum,na.rm =T)

grid_cntrl_mnth = full_join(grid_cntrl_mnth,
                            acled_conflict_per_cell, by =c("cell_id","year_mnth"))

grid_cntrl_mnth[which(is.na(grid_cntrl_mnth$total_fatalities)),]$total_fatalities = 0
grid_cntrl_mnth[which(is.na(grid_cntrl_mnth$total_events)),]$total_events = 0

# join rain data #########################################

# rain = data.table::fread("./data/rain_per_month.csv")[,c("cell_id","rain_mean","lg_rain_mean","date")]
# rain$date = as.numeric(gsub("-","",rain$date))
# 
# grid_cntrl_mnth = left_join(grid_cntrl_mnth, rain, by =c("cell_id"="cell_id", "year_mnth" = "date"))
# 

# as year month dateformat
library(zoo)

grid_cntrl_mnth$year_mnth_date = as.yearmon(as.character(grid_cntrl_mnth$year_mnth),format = "%Y%m")
date_combinations$year_mnth_date = as.yearmon(as.character(date_combinations$year_mnth),format = "%Y%m")

#############################
## create time series data ##
#############################

# saving takes ages stopped for now
#save(grid_cntrl_mnth,file = "./data/grid_timeseries.RData")

##################################################################
# create the data frontline - fortschreibung des Gebiete
# - all previous months 
###################################################################

frontline_data_controle_num_all_previous_time = data.frame()

for (d in 1:nrow(date_combinations)){
  tm = date_combinations$year_mnth_date[d]
  print(tm)
  
  
  frnt_data_controle_num_all_previous_time = grid_cntrl_mnth%>%filter(year_mnth_date <= tm & name =="Nord-Kivu") %>%  
    group_by(geometry)%>%
    filter(!(is.na(controle_num) & any(!is.na(controle_num)))) %>%
    slice_max(year_mnth_date, n = 1, with_ties = FALSE) %>%
    ungroup()%>%mutate(time = tm)
  
  frontline_data_controle_num_all_previous_time = rbind(frontline_data_controle_num_all_previous_time,frnt_data_controle_num_all_previous_time)


  
}


if("road" %in% grid_cntrl_mnth$surface){
  data_to_be_saved_to = paste0("./data/frontline_data_all_previous_mnths_control_num_street.RData")
} else {
  data_to_be_saved_to = paste0("./data/frontline_data_all_previous_mnths_control_num.RData")
  
}
save(frontline_data_controle_num_all_previous_time,file = data_to_be_saved_to)
message("frontline_data_controle_num_all_previous_time is saved!")





##

########################
#create plots frontline
###########################

# for (d in 1:nrow(date_combinations)){
#   tm = date_combinations$year_mnth_date[d]
#   print(tm)
#   
#   plt_grid = grid_cntrl_mnth%>%filter(year_mnth_date <= tm & year_mnth_date >= (tm-2/12) & name =="Nord-Kivu") %>%
#     group_by(geometry)%>%
#     filter(!(controle == 0.5 & any(controle != 0.5))) %>%
#     slice_max(year_mnth_date, n = 1, with_ties = FALSE) %>%
#     ungroup()%>%mutate(time = tm)
#   
#   
#   
#   gp = ggplot2::ggplot(plt_grid) +
#     ggplot2::geom_sf(aes(fill = controle_num))#+
#   
#   
#   ggsave(paste0("./plots/frontline_mnth_year_change_aoc/",as.numeric(format(as.Date(tm), "%Y%m")),".png"),gp)
#   
#   
# }


