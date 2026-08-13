#04b predict all months

library(sf)
library(dplyr)
library(spdep)



source("./04b_a_helper_functions.R")


name_of_grid_file_name = gsub(".shp","",name_of_grid)
data_path_to_be_read = paste0("./data/frontline_data_all_mnths_",name_of_grid_file_name,".RData")



load(data_path_to_be_read)

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
  prepare_data_for_prediction(frontline_data,date,N,grid_file_name = name_of_grid_file_name)
}




# needs to be done only once because grid stays the same for the whole time span
prepare_adj_matrix_for_prediction(frontline_data,N,
                                  date = NULL,
                                  snap = 10,
                                  second_degree_neighbours = F,
                                  bol_distance = T,
                                  border_distance = F,
                                  FUN = dist_in_km,
                                  name_of_grid)


 