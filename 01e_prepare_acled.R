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

admin2_false$poor_quality = admin2_false$distance > cell_size
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



# Solve problem with non state actors
gdf_territory$actor1_gov = grepl("Democratic Republic of Congo",gdf_territory$actor1)
gdf_territory$actor2_gov = grepl("Democratic Republic of Congo",gdf_territory$actor2)

gdf_territory$sub_event[(gdf_territory$sub_event=="Non-state actor overtakes territory" & (gdf_territory$actor1_gov | gdf_territory$actor2_gov))] = "M23 overtakes territory"

inconclusive_territory= gdf_territory%>%filter(((sub_event=="Non-state actor overtakes territory" & !actor1_gov & !actor2_gov) | 
                                                 (sub_event =="Non-violent transfer of territory")  ))
conclusive_territory= gdf_territory%>%filter(!((sub_event=="Non-state actor overtakes territory" & !actor1_gov & !actor2_gov) | 
                                                  (sub_event =="Non-violent transfer of territory")  ))


#inconclusive_territory$id2 = 1:nrow(inconclusive_territory)

inconclusive_territory = inconclusive_territory[order(inconclusive_territory$event_date),]

openxlsx::write.xlsx(st_drop_geometry(inconclusive_territory[c("id","notes")]), "./data/inconclusive_territories.xlsx")
inconclusive_territories_solved = openxlsx::read.xlsx("./data/inconclusive_territories_solved.xlsx")

testing = F
if (testing){
  
  source("./01e_a_rule_based_acled_attribution.R")
  all_events = sum( table(inconclusive_territory$sub_event))
  n_non_state = round(table(inconclusive_territory$sub_event)["Non-state actor overtakes territory"]/all_events*100,0)
  n_non_violent = round(table(inconclusive_territory$sub_event)["Non-violent transfer of territory"]/all_events*100,0)
  non_state_actor = gdf_territory%>%filter(((sub_event=="Non-state actor overtakes territory" & !actor1_gov & !actor2_gov)))
  non_violent_transfer = gdf_territory%>%filter(sub_event =="Non-violent transfer of territory")
  set.seed(123)
  ids = c(sample(non_state_actor$id,n_non_state),sample(non_violent_transfer$id,n_non_violent))
  
  openxlsx::write.xlsx(st_drop_geometry(inconclusive_territory[inconclusive_territory$id %in% ids,c("id","notes")]), "./data/inconclusive_territories_manually.xlsx")
 
  inconclusive_territories_manually_solved = openxlsx::read.xlsx("./data/inconclusive_territories_manually_solved.xlsx")
  
  inconclusive_territories_solved = openxlsx::read.xlsx("./data/inconclusive_territories_solved.xlsx")
  inconclusive_territories_solved = inconclusive_territories_solved[,-which(colnames(inconclusive_territories_solved)=="id")[1]]
  inconclusive_territories_solved$Actor.that.gained.territory[grepl("M23",inconclusive_territories_solved$Actor.that.gained.territory)]  = "M23"
  
  
  inconclusive_territories_solved$Actor.that.gained.territory[which(inconclusive_territories_solved$Actor.that.gained.territory=="Unclear" &
                                                                      grepl("M23",inconclusive_territories_solved$Actor.that.lost.territory))] <- "not M23"
  inconclusive_territories_solved$Actor.that.gained.territory[((!grepl("M23",inconclusive_territories_solved$Actor.that.gained.territory )) & 
                                                                inconclusive_territories_solved$Actor.that.gained.territory !="Unclear")] <- "not M23"
  
  test = left_join(inconclusive_territories_manually_solved,inconclusive_territories_solved , by = "id")
  test$truth = test$manual_attribution ==test$Actor.that.gained.territory
  sum(test$manual_attribution ==test$Actor.that.gained.territory)
  
  rule_based_attribution = rule_based_acled_attribution(inconclusive_territory)
  rule_based_attribution$rule_based_actor_control = NA
  rule_based_attribution$rule_based_actor_control[grepl("not M23",rule_based_attribution$sub_event)] = "not M23"
  rule_based_attribution$rule_based_actor_control[!grepl("not M23",rule_based_attribution$sub_event)] = "M23"
  test = left_join(test,rule_based_attribution , by = "id")
  
  sum(test$rule_based_actor_control == test$manual_attribution)/nrow(test)
  
  
}

inconclusive_territories_solved$Actor.that.gained.territory[grepl("M23",inconclusive_territories_solved$Actor.that.gained.territory)]  = "M23 overtakes territory"


inconclusive_territories_solved$Actor.that.gained.territory[which(inconclusive_territories_solved$Actor.that.gained.territory=="Unclear" &
                                  grepl("M23",inconclusive_territories_solved$Actor.that.lost.territory))] <- "not M23 overtakes territory"

inconclusive_territories_solved = inconclusive_territories_solved[inconclusive_territories_solved$Actor.that.gained.territory!="Unclear",]
inconclusive_territories_solved$Actor.that.gained.territory[!grepl("M23",inconclusive_territories_solved$Actor.that.gained.territory)] <- "not M23 overtakes territory"

inconclusive_territories_solved$sub_event = inconclusive_territories_solved$Actor.that.gained.territory

inconclusive_territories_solved = inconclusive_territories_solved[,c("id","sub_event")]
inconclusive_territory$sub_event = NULL
inconclusive_territories_solved = inner_join(inconclusive_territory,inconclusive_territories_solved,by = c("id"))

gdf_territory = rbind(inconclusive_territories_solved,conclusive_territory)

#### for testing 100 - ids ######



acled_territory_mnth <- gdf_territory %>%
  group_by(year_mnth, latitude, longitude) %>%
  slice_max(event_date, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    control = case_when(
      sub_event == "Government regains territory" ~ "not M23",
      sub_event == "not M23 overtakes territory" ~ "not M23",
      sub_event == "M23 overtakes territory" ~ "M23",
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
  ))%>%mutate(control = NA)



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
not_M23 = 0
M23 = 1

acled_territory_mnth$control_num = NA

acled_territory_mnth[which(acled_territory_mnth$control == "not M23"),]$control_num = not_M23
acled_territory_mnth[which(acled_territory_mnth$control == "M23"),]$control_num = M23


acled_territory_mnth = st_as_sf(acled_territory_mnth,crs =st_crs(gdf))
acled_conflict_mnth = st_as_sf(acled_conflict_mnth,coords = c("longitude","latitude"),crs ="EPSG:4326")
acled_conflict_mnth = st_transform(acled_conflict_mnth,st_crs(gdf))


save(acled_territory_mnth, file="./data/acled_territory_mnth.RData")
save(acled_conflict_mnth, file="./data/acled_conflict_mnth.RData")

