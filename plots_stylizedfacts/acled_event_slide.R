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
                          "Explosions/Remote violence") & sub_event !="Non-violent transfer of territory")

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
  mutate(month = floor_date(as.Date(event_date), "month")) %>%
  group_by(month) %>%
  summarise(
    count = sum(event_type %in% c("Battles",
                "Explosions/Remote violence")
                ,na.rm =T)
  )

sum(cnt_per_month_m23$count)
sum(cnt_per_month_m23$control_count)


gdf_territory <-  m23_events %>%
  filter(sub_event %in% c(
    "Government regains territory",
    "Non-state actor overtakes territory",
    "Non-violent transfer of territory"
  ))


cnt_per_month_m23_territory <- gdf_territory %>%
  mutate(month = floor_date(event_date, "month")) %>%
  group_by(month,sub_event) %>%
  summarise(
    count = n()
  )


last_date <- max(cnt_per_month_m23$month, na.rm = TRUE)

year_breaks <- seq(
  from = as.Date(paste0(format(min(cnt_per_month_m23$month, na.rm = TRUE), "%Y"), "-01-01")),
  to = as.Date(paste0(format(last_date, "%Y"), "-01-01")),
  by = "1 year"
)

cnt_per_month_m23$sub_event = "Battle Events"
plot_data = rbind(st_drop_geometry(cnt_per_month_m23),st_drop_geometry(cnt_per_month_m23_territory))

ggplot() +
  geom_col(data = plot_data, 
           aes(x = month,y = count, fill = sub_event),position = "stack")+
  scale_fill_manual(
    name = NULL,
    values = c(
      "Non-violent transfer of territory" = colors[3],
      "Non-state actor overtakes territory" = colors[1],
      "Government regains territory" = colors[2],
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
    legend.text = element_text(colour = "black",size = 12),
    legend.position = "bottom",
    legend.direction = "horizontal"
  )+
  labs(x = NULL, y = NULL,title = NULL)














##

