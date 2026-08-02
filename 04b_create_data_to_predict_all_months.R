#04b predict all months



library(sf)
library(dplyr)
library(spdep)



source("./04b_a_helper_functions.R")


if(add_streets){
  data_to_read = paste0("./data/frontline_data_all_previous_mnths_control_num_street.RData")
} else {
  data_to_read = paste0("./data/frontline_data_all_previous_mnths_control_num.RData")
  
}

load(data_to_read)

frontline_data = frontline_data_controle_num_all_previous_time
frontline_data = frontline_data[order(frontline_data$cell_id),]

rm(frontline_data_controle_num_all_previous_time)

frontline_data$control_binom = frontline_data$control
frontline_data$control_binom = ifelse(frontline_data$control_binom ==0.5,0,frontline_data$control_binom)

mean(frontline_data$control_binom,na.rm =T)

all_dates = unique(frontline_data$year_mnth)
all_dates =  paste0(all_dates, "01")
all_dates = as.Date(all_dates,format = "%Y%m%d")
# 11174 cellids

N = length(unique(frontline_data$cell_id))

for(date in all_dates){
  print(as.Date(date))
  prepare_data_for_prediction(frontline_data,date,N)
}


# needs to be done only once because grid stays the same for the whole time span
prepare_adj_matrix_for_prediction(frontline_data,N,snap = 10,second_degree_neighbours = T,bol_distance = T)
 