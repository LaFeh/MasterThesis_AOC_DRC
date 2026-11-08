#04b predict all months

library(sf)
library(dplyr)
library(spdep)



source("./04b_a_helper_functions.R")


name_of_grid_file_name = gsub(".shp","",name_of_grid)

data_path_to_be_read = paste0("./data/frontline_data_all_mnths_",name_of_final_data,".RData")



load(data_path_to_be_read)

frontline_data = frontline_data_control_num_all_previous_time
frontline_data = frontline_data[order(frontline_data$cell_id),]

rm(frontline_data_controle_num_all_previous_time)

frontline_data$control_binom = frontline_data$control
frontline_data$control_binom = ifelse(frontline_data$control_binom ==0.5,0,frontline_data$control_binom)

mean(frontline_data$control_binom,na.rm =T)

all_dates = unique(frontline_data$year_mnth)
all_dates =  paste0(all_dates, "01")
all_dates = as.Date(all_dates,format = "%Y%m%d")
all_dates = all_dates[which(all_dates>as.Date("20231231",format = "%Y%m%d"))]
# 11174 cellids


N = length(unique(frontline_data$cell_id))
print(nrow(frontline_data)/length(all_dates))

for(date in all_dates){
  print(as.Date(date))
  prepare_data_for_prediction(frontline_data,
                              date,
                              N,
                              quality_strict,
                              grid_file_name = name_of_final_data)
}



# date = "20241101"
# time = zoo::as.yearmon(as.Date(date,format ="%Y%m%d"))
# frontline_data = frontline_data[which(frontline_data$time == time),]
gc()
# needs to be done only once because grid stays the same for the whole time span

if(adjacency_params$adj_neighbour_degree=="first"){
  bol_second_degree_neighbours = F
} else{
  bol_second_degree_neighbours = T
}

prepare_adj_matrix_for_prediction(frontline_data,N,
                                  date = NULL,
                                  snap = 10,
                                  bol_second_degree_neighbours = bol_second_degree_neighbours,
                                  bol_distance = adjacency_params$adj_distance,
                                  border_distance =  adjacency_params$adj_border,
                                  FUN = dist_in_km,
                                  name_of_grid,
                                  name_of_adjacency,
                                  time_travel_dataset = adjacency_params$adj_traveltime)


 