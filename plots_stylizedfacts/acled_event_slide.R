#xx summary statistics acled data


setwd("~/MasterThesis_AOC_DRC")

library("MetBrewer")

# Generate 5 colors from the Hiroshige palette
colors <- met.brewer(name = "Hiroshige", n = 10)

# Display the colors
print(colors)
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
  )
min(gdf$event_date)
nrow(gdf)
table(gdf$event_type)

gdf_battle_strategic_events <- gdf %>%
  filter(event_type %in%c("Battles",
                          "Explosions/Remote violence") | sub_event =="Non-violent transfer of territory")

table(gdf_battle_strategic_events$event_type)
table(gdf_battle_strategic_events$sub_event)

nrow(gdf_battle_strategic_events)/nrow(gdf)
nrow(gdf_battle_strategic_events)

m23_events = gdf%>%
  filter(actor1 == "M23: March 23 Movement" | actor2 == "M23: March 23 Movement")%>%
  rename(geometry = geom)

min(m23_events$event_date)

m23_battle_strategic_events = gdf_battle_strategic_events%>%
  filter(actor1 == "M23: March 23 Movement" | actor2 == "M23: March 23 Movement")%>%
  rename(geometry = geom)



nrow(m23_battle_strategic_events)
table(m23_battle_strategic_events$event_type)

cnt_per_month_m23 <- m23_battle_strategic_events %>%
  mutate(month = floor_date(event_date, "month")) %>%
  group_by(month) %>%
  summarise(
    count = sum(event_type %in% c("Battles",
                "Explosions/Remote violence") & 
                  !sub_event %in% c("Government regains territory",
                                   "Non-state actor overtakes territory",
                                   "Non-violent transfer of territory")
                ,na.rm =T),
    control_count = sum(sub_event %in% c("Government regains territory",
                                          "Non-state actor overtakes territory",
                                          "Non-violent transfer of territory"), na.rm = TRUE),
    .groups = "drop"
  )

sum(cnt_per_month_m23$count)
sum(cnt_per_month_m23$control_count)


gdf_territory <- m23_battle_strategic_events <- gdf %>%
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
  
  past_instance = gdf_territory%>%filter((event_date<max_date) & (geom ==instance$geom))%>%
    slice_max(order_by = event_date, with_ties =FALSE)
  
  if (nrow(past_instance)==0){
    
    non_violent_transfers[row_idx,]$sub_event ="Non-state actor overtakes territory"
    
  }else if (past_instance$sub_event =="Government regains territory"){# territory war before under gov controle
    
    non_violent_transfers[row_idx,]$sub_event ="Non-state actor overtakes territory"
    
  } else if (past_instance$sub_event =="Non-state actor overtakes territory"){
    
    non_violent_transfers[row_idx,]$sub_event ="Government regains territory"
    
  } else {
    
    past_instance = non_violent_transfers%>%filter((event_date<max_date) & (geom ==instance$geom))%>%
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

cnt_per_month_m23_territory <- gdf_territory %>%
  mutate(month = floor_date(event_date, "month")) %>%
  group_by(month,sub_event) %>%
  summarise(
    count = n(),
    .groups = "drop"
  )

cnt_per_month_m23

last_date <- max(cnt_per_month_m23$month, na.rm = TRUE)

year_breaks <- seq(
  from = as.Date(paste0(format(min(cnt_per_month_m23$month, na.rm = TRUE), "%Y"), "-01-01")),
  to = as.Date(paste0(format(last_date, "%Y"), "-01-01")),
  by = "1 year"
)

ggplot() +
  geom_col(data = as.data.frame(st_drop_geometry(cnt_per_month_m23)), 
           aes(x = month,y = count, fill = "Battle Events")) +
  geom_col(data = as.data.frame(st_drop_geometry(cnt_per_month_m23_territory)),
           aes(x = month,y = control_count, fill = sub_event),position = "stack") +
  scale_fill_manual(
    name = NULL,
    values = c(
      "Government regains territory" = colors[2],
      "Non-state actor overtakes territory" = colors[3],
      "Battle Events" = colors[9]
    )
  ) +
  labs(
    x = "Date",
    y = "Number of Events"
  ) + 
  scale_x_date(
    breaks = year_breaks,
    labels = format(year_breaks, "%Y %b")
  )+
  theme_minimal()+
  geom_hline(yintercept = 0)+
  theme(
    axis.text.x = element_text(
      colour = "black",
      size = 12
    ),
    axis.text.y = element_text(
      colour = "black",
      size = 12,
    ),
    plot.background  = element_rect(fill = "transparent", colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.box.background = element_rect(fill = "transparent", colour = NA),
    legend.text = element_text(colour = "black"),
    legend.position = "bottom",
    legend.direction = "horizontal"
  )+
  labs(x = NULL, y = NULL,title = NULL)







temp = m23_battle_strategic_events%>%filter(sub_event =="Non-state actor overtakes territory")

temp$actor1_gov = grepl("Democratic Republic of Congo",temp$actor1)
temp$actor2_gov = grepl("Democratic Republic of Congo",temp$actor2)
k = temp[which(!(temp$actor1_gov | temp$actor2_gov)),]

k =st_drop_geometry(k[,c("event_date","actor1", "actor2", "notes")])
k$actors = apply(k[,c("actor1", "actor2")],1, function(x) paste(x,collapse =", "))
k[,c("actor1", "actor2")] = NULL
write_excel_csv(k,file = "")










##




unique(c(k$actor1,k$actor2))
