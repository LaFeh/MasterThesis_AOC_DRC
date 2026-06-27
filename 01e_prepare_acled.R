####### ACLED DATA ############################
setwd("D:/DRC/gaussian_process_AOC")

# get ACLED data and calculate for each grid AOC for each month

library(sf)
library(dplyr)
library(lubridate)
library(tidyverse)

# Read shapefile
gdf <- st_read("D:/DRC/01_prepare_acled_data/data/acled_event_data.gpkg")

# Prepare data
gdf <- gdf %>%
  mutate(
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude),
    event_date = as.Date(event_date),
    year_mnth = format(event_date, "%Y%m"),
    year = format(event_date, "%Y")
  ) %>%
  filter(actor1 == "M23: March 23 Movement" | actor2 == "M23: March 23 Movement")%>%rename(geometry = geom)


# -----------------------------
# TERRITORY EVENTS
# -----------------------------

gdf_territory <- gdf %>%
  filter(sub_event %in% c(
    "Government regains territory",
    "Non-state actor overtakes territory",
    "Non-violent transfer of territory"
  ))

non_violent_transfers = gdf_territory[which(gdf_territory$sub_event=="Non-violent transfer of territory"),]
non_violent_transfers = non_violent_transfers[order(non_violent_transfers$event_date),]

for(row_idx in 1:nrow(non_violent_transfers)){
  instance = non_violent_transfers[row_idx,]
  max_date = instance$event_date

  past_instance = gdf_territory%>%filter((event_date<max_date) & (geometry ==instance$geometry))%>%
    slice_max(order_by = event_date, with_ties =FALSE)
  
  if (nrow(past_instance)==0){
    
    non_violent_transfers[row_idx,]$sub_event ="Non-state actor overtakes territory"
    
  }else if (past_instance$sub_event =="Government regains territory"){# territory war before under gov controle
    
    non_violent_transfers[row_idx,]$sub_event ="Non-state actor overtakes territory"
 
  } else if (past_instance$sub_event =="Non-state actor overtakes territory"){
    
    non_violent_transfers[row_idx,]$sub_event ="Government regains territory"
    
  } else {
    
    past_instance = non_violent_transfers%>%filter((event_date<max_date) & (geometry ==instance$geometry))%>%
      slice_max(order_by = event_date, with_ties =FALSE)
      
    if (past_instance$sub_event =="Government regains territory"){# territory war before under gov controle
      
      non_violent_transfers[row_idx,]$sub_event ="Non-state actor overtakes territory"
      
    } else if (past_instance$sub_event =="Non-state actor overtakes territory"){
      
      non_violent_transfers[row_idx,]$sub_event ="Government regains territory"}
    
    else{
      warning(paste0(row_idx,": we need a double loop, two times after each other peaceful transition"))
      
    }


  }
  
}

gdf_territory <-  gdf_territory%>%filter(sub_event != "Non-violent transfer of territory")
gdf_territory <- rbind(gdf_territory,non_violent_transfers)

acled_territory_mnth <- gdf_territory %>%
  group_by(year_mnth, latitude, longitude) %>%
  slice_max(event_date, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    controle = case_when(
      sub_event == "Government regains territory" ~ "government",
      sub_event == "Non-state actor overtakes territory" ~ "non-state actor",
      sub_event == "Non-violent transfer of territory" ~ "unknown"
    )
  )


# -----------------------------
# ALL OTHER EVENTS
# -----------------------------

acled_conflict <- gdf %>%
  filter(!sub_event %in% c(
    "Government regains territory",
    "Non-state actor overtakes territory",
    "Non-violent transfer of territory"
  ))%>%mutate(controle = NA)


acled_conflict_wide_sum = as.data.frame(acled_conflict)%>%
  mutate(events = 1)%>%
  tidyr::pivot_wider(id_cols =c("year_mnth","longitude","latitude"),
                     names_from =c("event_type"),values_from= c("events","fatalities"),values_fn = sum,values_fill = 0)


names(acled_conflict_wide_sum)[names(acled_conflict_wide_sum) == "events_Violence against civilians"] <- "events_violence_civilian"
names(acled_conflict_wide_sum)[names(acled_conflict_wide_sum) == "events_Strategic developments"] <- "events_strategic_developments"
names(acled_conflict_wide_sum)[names(acled_conflict_wide_sum) == "events_Explosions/Remote violence"] <- "events_remote_violence"

names(acled_conflict_wide_sum)[names(acled_conflict_wide_sum) == "fatalities_Violence against civilians"] <- "fatalities_violence_civilian"
names(acled_conflict_wide_sum)[names(acled_conflict_wide_sum) == "fatalities_Strategic developments"] <- "fatalities_strategic_developments"
names(acled_conflict_wide_sum)[names(acled_conflict_wide_sum) == "fatalities_Explosions/Remote violence"] <- "fatalities_remote_violence"



acled_conflict_wide_sum$fatalities_total = apply(
  acled_conflict_wide_sum[,grep("fatalities",colnames(acled_conflict_wide_sum))],1,sum)
acled_conflict_wide_sum$events_total = apply(
  acled_conflict_wide_sum[,grep("events_",colnames(acled_conflict_wide_sum))],1,sum)


acled_conflict_summarise_avg = as.data.frame(acled_conflict)%>%group_by(year_mnth,longitude,latitude)%>%
  summarise(geo_prcsn = mean(geo_prcsn))

acled_conflict_mnth = acled_conflict_wide_sum%>%left_join(acled_conflict_summarise_avg, by =c("year_mnth","longitude","latitude") )


#------------------------------------------------
# MERGE FATALITIES THAT HAPPENED AFTER CONTROLE
# ------------------------------------------------
controle_per_conflict = left_join(acled_conflict,as.data.frame(acled_territory_mnth),by =c("latitude","longitude","year_mnth"),suffix = c("","_territory"))
conflict_after_controle= controle_per_conflict %>%
  filter(is.na(event_date_territory) | event_date_territory<event_date)

fatalities_after_controle = as.data.frame(conflict_after_controle)%>%
  mutate(events = 1)%>%
  tidyr::pivot_wider(id_cols =c("year_mnth","longitude","latitude"),
                     names_from =c("event_type"),values_from= c("events","fatalities"),values_fn = sum,values_fill = 0)


names(fatalities_after_controle)[names(fatalities_after_controle) == "events_Violence against civilians"] <- "events_violence_civilian"
names(fatalities_after_controle)[names(fatalities_after_controle) == "events_Strategic developments"] <- "events_strategic_developments"
names(fatalities_after_controle)[names(fatalities_after_controle) == "events_Explosions/Remote violence"] <- "events_remote_violence"

names(fatalities_after_controle)[names(fatalities_after_controle) == "fatalities_Violence against civilians"] <- "fatalities_violence_civilian"
names(fatalities_after_controle)[names(fatalities_after_controle) == "fatalities_Strategic developments"] <- "fatalities_strategic_developments"
names(fatalities_after_controle)[names(fatalities_after_controle) == "fatalities_Explosions/Remote violence"] <- "fatalities_remote_violence"

fatalities_after_controle$fatalities_total = apply(fatalities_after_controle[,grep("fatalities",colnames(fatalities_after_controle))],1,sum)


conflict_after_controle  = conflict_after_controle %>%
  left_join(fatalities_after_controle, by = c("year_mnth","longitude","latitude"))

fatalities_and_territory_after_controle  = acled_territory_mnth %>%
  left_join(fatalities_after_controle, by = c("year_mnth","longitude","latitude"),
            suffix = c("","_conflict"))
days_in_month(fatalities_and_territory_after_controle$event_date)


fatalities_and_territory_after_controle$days_till_end_of_month = as.numeric(days_in_month(fatalities_and_territory_after_controle$event_date))-as.numeric(format(fatalities_and_territory_after_controle$event_date,"%d"))
fatalities_and_territory_after_controle$fatalities_per_day = fatalities_and_territory_after_controle$fatalities_total/fatalities_and_territory_after_controle$days_till_end_of_month

government_controle = 0
rebell_controle = 1
government_controle_disputed = 0.25
rebell_controle_disputed = 0.75
disputed = 0.5


fatalities_and_territory_after_controle$controle_num = NA



fatalities_and_territory_after_controle[which(fatalities_and_territory_after_controle$controle == "government"),]$controle_num = government_controle
fatalities_and_territory_after_controle[which(fatalities_and_territory_after_controle$controle == "government" & 
                                                fatalities_and_territory_after_controle$fatalities_per_day  >=1 ),]$controle_num = government_controle_disputed

fatalities_and_territory_after_controle[which(fatalities_and_territory_after_controle$controle == "non-state actor"),]$controle_num = rebell_controle
fatalities_and_territory_after_controle[which(fatalities_and_territory_after_controle$controle == "non-state actor" & 
                                                fatalities_and_territory_after_controle$fatalities_per_day  >=1 ),]$controle_num = rebell_controle_disputed

fatalities_and_territory_after_controle[which(fatalities_and_territory_after_controle$controle == "unknown"),]$controle_num = disputed



acled_territory_mnth = st_as_sf(fatalities_and_territory_after_controle,crs =st_crs(gdf))
acled_conflict_mnth = st_as_sf(acled_conflict_mnth,coords = c("longitude","latitude"),crs ="EPSG:4326")
acled_conflict_mnth = st_transform(acled_conflict_mnth,st_crs(gdf))





save(acled_territory_mnth, file="./data/acled_territory_mnth.RData")

save(acled_conflict_mnth, file="./data/acled_conflict_mnth.RData")
