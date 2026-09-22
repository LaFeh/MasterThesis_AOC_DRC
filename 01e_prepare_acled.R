####### ACLED DATA ############################


# get ACLED data and calculate for each grid AOC for each month

library(sf)
library(dplyr)
library(lubridate)
library(tidyverse)

# Read shapefile
gdf <- st_read("./data/acled_event_data.gpkg")

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

gdf$id = 1:nrow(gdf)

############################ remove unplausible data rows, given provinces does not correspond with coordinates:
gdf$poor_quality = FALSE

regions = st_read("./data/cod_admin_boundaries.shp",layer="cod_admin3")
regions = st_transform(regions,st_crs(gdf))
regions_for_join = regions%>%select(adm1_name,adm2_name,adm2_pcode,geometry)


gdf_with_regions = st_join(gdf,regions_for_join,st_within)

gdf_with_regions$admin2_location_check = ifelse(gdf_with_regions$admin2==gdf_with_regions$adm2_name,TRUE,FALSE)
admin2_false = gdf_with_regions[!gdf_with_regions$admin2_location_check,c("id","notes","admin1","adm1_name","admin2","adm2_name")]
admin2_false$distance = NA

for( x in 1:nrow(admin2_false)){
  region_bound = st_boundary(st_union(regions_for_join[regions_for_join$adm2_name == admin2_false[x,]$adm2_name,]))
  admin2_false[x,"distance"] = st_distance(region_bound,admin2_false[x,]$geometry)
}

admin2_false$poor_quality = admin2_false$distance > grid_cell
admin2_false = admin2_false[admin2_false$poor_quality,]

gdf[which(gdf$id %in% admin2_false$id),]$poor_quality = TRUE

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



acled_conflict$event_type_clean = tolower(gsub(" |/","_",acled_conflict$event_type))
acled_conflict_wide_sum = as.data.frame(acled_conflict)%>%
  mutate(events = 1)%>%
  tidyr::pivot_wider(id_cols =c("year_mnth","longitude","latitude"),
                     names_from =c("event_type_clean"),values_from= c("events","fatalities"),values_fn = sum,values_fill = 0)



acled_conflict_wide_sum$fatalities_total = apply(
  acled_conflict_wide_sum[,grep("fatalities",colnames(acled_conflict_wide_sum))],1,sum)
acled_conflict_wide_sum$events_total = apply(
  acled_conflict_wide_sum[,grep("events_",colnames(acled_conflict_wide_sum))],1,sum)


acled_conflict_summarise_avg = as.data.frame(acled_conflict)%>%group_by(year_mnth,longitude,latitude)%>%
  summarise(geo_prcsn = mean(geo_prcsn))

acled_conflict_mnth = acled_conflict_wide_sum%>%left_join(acled_conflict_summarise_avg, by =c("year_mnth","longitude","latitude") )

#------------------------------------------------
# MERGE FATALITIES THAT HAPPENED AFTER CONTROL with in 10 days
# ------------------------------------------------

conflict_per_control = left_join(acled_territory_mnth,as.data.frame(acled_conflict),by =c("latitude","longitude"),suffix = c("","_conflict"),relationship = "many-to-many")
# conflict 10 days after control. 
conflict_after_control= conflict_per_control %>%
  mutate(difference_in_days = event_date_conflict-event_date)%>%
  filter(difference_in_days<10 & difference_in_days>0 ) # bigger than zero, not considering battles of the day of territory control takeover

conflict_after_control$event_type_clean = paste0(tolower(gsub(" |/","_",conflict_after_control$event_type_conflict)),"_after_control")
conflict_after_control$sub_event_type_clean = paste0(tolower(gsub(" |/","_",conflict_after_control$sub_event_conflict)),"_after_control")
events_after_control = as.data.frame(conflict_after_control)%>%
  mutate(events = 1)%>% # one occurence of event = one event
  group_by(id,event_date,longitude,latitude,
           event_type_clean,
           #sub_event_type_clean
  )%>%
  summarise(events = sum(events),
            fatalities = sum(fatalities))%>%
  tidyr::pivot_wider(id_cols =c("id","event_date","longitude","latitude"),
                     names_from =c("event_type_clean",
                                   #"sub_event_type_clean"
                     ),values_from= c("events","fatalities"),values_fn = sum,values_fill = 0)


prefixes <- c(
  "fatalities",
  "events"#,
  # "fatalities_battles",
  # "fatalities_violence_against_civilians",
  # "fatalities_strategic_developments",
  # "fatalities_explosions_remote_violence",
  # "events_battles",
  # "events_violence_against_civilians",
  # "events_strategic_developments",
  # "events_explosions_remote_violence",
  # "events_protests",
  # "events_riots"
)

for (prefix in prefixes) {
  cols <- grep(
    paste0("^", prefix, "_"),
    names(events_after_control),
    value = TRUE
  )
  
  cols <- cols[!grepl("_total_after_control$", cols)]
  
  events_after_control[[paste0(prefix, "_total_after_control")]] <-
    rowSums(events_after_control[cols], na.rm = TRUE)
}

#### conflict 10 days before control
conflict_before_control= conflict_per_control %>%
  mutate(difference_in_days = event_date_conflict-event_date)%>%
  filter(difference_in_days>10 & difference_in_days>0 ) # bigger than zero, not considering battles of the day of territory control takeover


conflict_before_control$event_type_clean = paste0(tolower(gsub(" |/","_",conflict_before_control$event_type_conflict)),"_before_control")
conflict_before_control$sub_event_type_clean = paste0(tolower(gsub(" |/","_",conflict_before_control$sub_event_conflict)),"_before_control")
events_before_control = as.data.frame(conflict_before_control)%>%
  mutate(events = 1)%>% # one occurence of event = one event
  group_by(id,event_date,longitude,latitude,
           event_type_clean,
           #sub_event_type_clean
  )%>%
  summarise(events = sum(events),
            fatalities = sum(fatalities))%>%
  tidyr::pivot_wider(id_cols =c("id","event_date","longitude","latitude"),
                     names_from =c("event_type_clean",
                                   #"sub_event_type_clean"
                     ),values_from= c("events","fatalities"),values_fn = sum,values_fill = 0)



prefixes <- c(
  "fatalities",
  "events"#,
  # "fatalities_battles",
  # "fatalities_violence_against_civilians",
  # "fatalities_strategic_developments",
  # "fatalities_explosions_remote_violence",
  # "events_battles",
  # "events_violence_against_civilians",
  # "events_strategic_developments",
  # "events_explosions_remote_violence",
  # "events_protests",
  # "events_riots"
)

for (prefix in prefixes) {
  cols <- grep(
    paste0("^", prefix, "_"),
    names(events_before_control),
    value = TRUE
  )
  
  cols <- cols[!grepl("_total_before_control$", cols)]
  
  events_before_control[[paste0(prefix, "_total_before_control")]] <-
    rowSums(events_before_control[cols], na.rm = TRUE)
}



###### join everything back together
acled_territory_mnth = left_join(acled_territory_mnth,events_before_control, by=c("id","event_date","longitude","latitude"))
acled_territory_mnth = left_join(acled_territory_mnth,events_after_control, by=c("id","event_date","longitude","latitude"))


# create numeric control indicator
government_control = 0
rebell_control = 1

acled_territory_mnth$control_num = NA

acled_territory_mnth[which(acled_territory_mnth$controle == "government"),]$control_num = government_control
acled_territory_mnth[which(acled_territory_mnth$controle == "non-state actor"),]$control_num = rebell_control


acled_territory_mnth = st_as_sf(acled_territory_mnth,crs =st_crs(gdf))
acled_conflict_mnth = st_as_sf(acled_conflict_mnth,coords = c("longitude","latitude"),crs ="EPSG:4326")
acled_conflict_mnth = st_transform(acled_conflict_mnth,st_crs(gdf))


save(acled_territory_mnth, file="./data/acled_territory_mnth.RData")
save(acled_conflict_mnth, file="./data/acled_conflict_mnth.RData")