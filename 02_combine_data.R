# 02 combine data

library(sf)
library(dplyr)



grid = read_sf(paste0("./data/",name_of_grid))

load(file = "./data/acled_conflict_mnth.RData")
load(file="./data/acled_territory_mnth.RData")
grid = st_transform(grid,st_crs(acled_territory_mnth))
acled_conflict_mnth = st_transform(acled_conflict_mnth,st_crs(acled_territory_mnth))

if(quality_strict){
  acled_conflict_mnth = acled_conflict_mnth %>%filter((!poor_quality) & (!outlier))
  acled_territory_mnth = acled_territory_mnth %>%filter((!poor_quality) & (!outlier))
}

##### settlements ########################
# grid_settlements = data.table::fread("./data/grid_settlements.csv")
# grid_settlements = grid_settlements[,c("cell_id","building_count","building_area")]
# 
# grid = left_join(grid,grid_settlements,by ="cell_id")
# grid[which(is.na(grid$building_count)),]$building_count = 0
# grid[which(is.na(grid$building_area)),]$building_area = 0
# rm(grid_settlements)

#### mix time walk time ########################
grid_mix_time = data.table::fread("./data/grid_mix_time.csv",sep =",")
grid = left_join(grid,grid_mix_time,by ="cell_id")

#plot(grid[which(is.na(grid$mix_time_mean)),"geometry"])
grid = grid[-which(is.na(grid$mix_time_mean)),]
rm(grid_mix_time)



### acled data year mnth #######################
acled_territory_mnth$year_mnth = as.numeric(acled_territory_mnth$year_mnth)
acled_conflict_mnth$year_mnth = as.numeric(acled_conflict_mnth$year_mnth)

# rwa distance ###########################
dist_rwa = data.table::fread("./data/distance_rwanda.csv")
grid = left_join(grid,dist_rwa, by="cell_id")



# 1st. create data per month year ####################################################

yrs = unique(substr(acled_conflict_mnth$year_mnth,1,4))
mnths = unique(substr(acled_conflict_mnth$year_mnth,5,7))

min_date <- min(acled_territory_mnth$year_mnth)
min_date <- as.Date(paste0(min_date,"01"),format = "%Y%M%d")-base::months(4)
min_month = ifelse(lubridate::month(min_date)<10,paste0("0",lubridate::month(min_date)),lubridate::month(min_date))
min_date = paste0(lubridate::year(min_date),min_month)

date_combinations = cross_join(as_tibble(yrs),as_tibble(mnths))


date_combinations$time_step = 1:nrow(date_combinations)
date_combinations$year_mnth = as.numeric(paste0(date_combinations$value.x,date_combinations$value.y))

date_combinations = date_combinations[date_combinations$year_mnth > min_date,]
grid_yr_mnth <- merge(grid, date_combinations[c("year_mnth","time_step")], by = NULL)

# acled territory

acled_territory_grid = st_join(acled_territory_mnth,grid[,"cell_id"],join = st_within, left = TRUE)

aoc_per_cell = acled_territory_grid%>%
  dplyr::group_by(year_mnth,cell_id)%>%
  dplyr::summarise(non_state_actor = sum(controle == "non-state actor"),
                   government = sum(controle == "government"),
                   control_num = mean(control_num),
                   across(
                     c(starts_with("events_"), starts_with("fatalities_")),
                     list(
                       temp = ~sum(.x,na.rm = T)
                     ),
                     .names = "{.col}"
                   ))%>%
  mutate(control = if_else((non_state_actor+government)!=0, non_state_actor/(non_state_actor+government), 0.5))

aoc_per_cell = aoc_per_cell[,!colnames(aoc_per_cell) %in% c("non_state_actor","government")]
aoc_per_cell = st_drop_geometry(aoc_per_cell)


warning("following are NA ", which(is.na(aoc_per_cell$cell_id)))
aoc_per_cell = aoc_per_cell[which(!is.na(aoc_per_cell$cell_id)),]

grid_cntrl_mnth = full_join(grid_yr_mnth,aoc_per_cell, by =c("cell_id","year_mnth"))

acled_conflict_mnth= st_join(acled_conflict_mnth,grid[,"cell_id"],join = st_within, left = FALSE)


acled_conflict_per_cell = acled_conflict_mnth %>%
  st_drop_geometry() %>%
  ungroup() %>%
  dplyr::group_by(year_mnth, cell_id) %>%
  dplyr::summarise(
    across(
      c(starts_with("events_"), starts_with("fatalities_")),
      list(
        temp = ~sum(.x,na.rm = T)
      ),
      .names = "{.col}"
    ),
    .groups = "drop"
  ) %>%
  dplyr::arrange(cell_id, year_mnth) %>%
  dplyr::group_by(cell_id) %>%
  mutate(
    across(
      c(starts_with("events_"), starts_with("fatalities_")),
      list(
        lag = ~lag(.x),
        lead = ~lead(.x)
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  dplyr::ungroup()


# Current period
acled_conflict_per_cell$total_fatalities =
  rowSums(acled_conflict_per_cell[, grep("^fatalities_(?!.*_(lag|lead)$)",
                                         colnames(acled_conflict_per_cell),
                                         perl = TRUE)],
          na.rm = TRUE)

acled_conflict_per_cell$total_events =
  rowSums(acled_conflict_per_cell[, grep("^events_(?!.*_(lag|lead)$)",
                                         colnames(acled_conflict_per_cell),
                                         perl = TRUE)],
          na.rm = TRUE)

# Lag totals
acled_conflict_per_cell$total_fatalities_lag =
  rowSums(acled_conflict_per_cell[, grep("^fatalities_.*_lag$",
                                         colnames(acled_conflict_per_cell))],
          na.rm = TRUE)

acled_conflict_per_cell$total_events_lag =
  rowSums(acled_conflict_per_cell[, grep("^events_.*_lag$",
                                         colnames(acled_conflict_per_cell))],
          na.rm = TRUE)

# Lead totals
acled_conflict_per_cell$total_fatalities_lead =
  rowSums(acled_conflict_per_cell[, grep("^fatalities_.*_lead$",
                                         colnames(acled_conflict_per_cell))],
          na.rm = TRUE)

acled_conflict_per_cell$total_events_lead =
  rowSums(acled_conflict_per_cell[, grep("^events_.*_lead$",
                                         colnames(acled_conflict_per_cell))],
          na.rm = TRUE)






#acled_conflict_per_cell$total_fatalities = apply(acled_conflict_per_cell[,grep("fatalities",colnames(acled_conflict_per_cell))],1,sum,na.rm =T)
#acled_conflict_per_cell$total_events = apply(acled_conflict_per_cell[,grep("events",colnames(acled_conflict_per_cell))],1,sum,na.rm =T)

grid_cntrl_mnth = full_join(grid_cntrl_mnth,
                            acled_conflict_per_cell, by =c("cell_id","year_mnth"))

grid_cntrl_mnth <- grid_cntrl_mnth %>%
  dplyr::mutate(
    dplyr::across(
      matches("^(events_|fatalities_|total_events|total_fatalities)"),
      ~ ifelse(is.na(.x), 0, .x)
    )
  )

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
# 
frontline_data_control_num_all_previous_time = data.frame()

# for (d in 1:nrow(date_combinations)){
#   tm = date_combinations$year_mnth_date[d]
#   print(tm)
# 
# 
#   frnt_data_control_num_all_previous_time = grid_cntrl_mnth%>%filter(year_mnth_date <= tm )%>%# & name =="Nord-Kivu") %>%
#     group_by(cell_id)%>%
#     filter(!(is.na(control_num) & any(!is.na(control_num)))) %>%
#     slice_max(year_mnth_date, n = 1, with_ties = FALSE) %>%
#     ungroup()%>%mutate(time = tm)
# 
#   frontline_data_control_num_all_previous_time = rbind(frontline_data_control_num_all_previous_time,frnt_data_control_num_all_previous_time)
# 
# 
# 
# }


library(future.apply)
library(dplyr)
library(future)
# --------------------------------------------------
# Set number of parallel workers
# --------------------------------------------------



geometry_lookup <- grid %>%
  select(cell_id, geometry)

grid_cntrl_mnth_no_geom <- st_drop_geometry(grid_cntrl_mnth)

n_workers <- max(1, min(3,parallel::detectCores() - 1))
plan(multicore, workers = n_workers)


results <- future_lapply(
  1:nrow(date_combinations),
  function(d) {
    
    tm <- date_combinations$year_mnth_date[d]
    
    message("Processing: ", tm)
    
    frnt_data_control_num_all_previous_time <-
      grid_cntrl_mnth_no_geom %>%
      filter(year_mnth_date <= tm) %>%
      group_by(cell_id) %>%
      filter(!(is.na(control_num) & any(!is.na(control_num)))) %>%
      slice_max(
        year_mnth_date,
        n = 1,
        with_ties = FALSE
      ) %>%
      ungroup() %>%
      mutate(time = tm)
    
    frnt_data_control_num_all_previous_time
  }
)



frontline_data_control_num_all_previous_time <-
  bind_rows(results)


frontline_data_control_num_all_previous_time <-
  frontline_data_control_num_all_previous_time %>%
  left_join(
    geometry_lookup,
    by = "cell_id"
  ) %>%
  st_as_sf()


plan(sequential)


name_of_grid_file_name = gsub(".shp","",name_of_grid)
data_to_be_saved_to = paste0("./data/frontline_data_all_mnths_quality_",quality_strict,"_",name_of_grid_file_name,".RData")

save(frontline_data_control_num_all_previous_time,file = data_to_be_saved_to)
message(paste0("./data/frontline_data_all_mnths_quality_",quality_strict,"_",name_of_grid_file_name,".RData is saved!"))
