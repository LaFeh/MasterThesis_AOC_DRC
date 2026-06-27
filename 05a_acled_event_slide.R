#xx summary statistics acled data


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
  )
min(gdf$event_date)
nrow(gdf)
table(gdf$event_type)
gdf_battle_strategic_events <- gdf %>%
  filter(event_type %in%c("Battles",
                          "Strategic developments",
                          "Explosions/Remote violence"))

table(gdf_battle_strategic_events$event_type)

nrow(gdf_battle_strategic_events)/nrow(gdf)
nrow(gdf_battle_strategic_events)

# m23_events = gdf%>%
#   filter(actor1 == "M23: March 23 Movement" | actor2 == "M23: March 23 Movement")%>%
#   rename(geometry = geom)

m23_battle_strategic_events = gdf_battle_strategic_events%>%
  filter(actor1 == "M23: March 23 Movement" | actor2 == "M23: March 23 Movement")%>%
  rename(geometry = geom)
nrow(m23_battle_strategic_events)
table(m23_battle_strategic_events$event_type)

cnt_per_month_m23 <- m23_battle_strategic_events %>%
  mutate(month = floor_date(event_date, "month")) %>%
  group_by(month) %>%
  summarise(
    count = n(),
    control_count = sum(sub_event %in% c("Government regains territory",
                                          "Non-state actor overtakes territory",
                                          "Non-violent transfer of territory"), na.rm = TRUE),
    .groups = "drop"
  )

sum(cnt_per_month_m23$count)
sum(cnt_per_month_m23$control_count)

ggplot(cnt_per_month_m23, aes(x = month)) +
  geom_col(aes(y = count, fill = "Strategic Battle Events")) +
  geom_col(aes(y = control_count, fill = "Change in Control")) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "Strategic Battle Events" = "#FF6600",
      "Change in Control" = "#1F77B4"
    )
  ) +
  labs(
    x = "Date",
    y = "Number of Events",
    title = "Events per Day"
  ) + expand_limits(y = 0)+
  theme_minimal() +
  geom_hline(yintercept = 0,color="#FF6600")+
  theme(
    axis.text.x = element_text(
      colour = "white",
      face = "bold",
      size = 16
    ),
    axis.text.y = element_text(
      colour = "white",
      face = "bold",
      size = 16
    ),
    plot.background  = element_rect(fill = "transparent", colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.box.background = element_rect(fill = "transparent", colour = NA),
    legend.text = element_text(colour = "white"),
    legend.position = "bottom",
    legend.direction = "horizontal"
  )+labs(x = NULL, y = NULL,title = NULL)

ggsave(
  "./plots/MTC/distribution_events_count_and_control_with_legend_non_tp.png",
  width = 10,
  height = 5
)
cnt_per_month_m23 = st_drop_geometry(cnt_per_month_m23)
write.csv(cnt_per_month_m23,"acled_events.csv")

sum(cnt_per_month_m23$control_count)

sum(cnt_per_month_m23$count)

##########################################################


ggplot(cnt_per_month_m23, aes(x = month)) +
  geom_col(aes(y = count, fill = "All events")) +
  geom_col(aes(y = controle_count, fill = "Change in control")) +
  scale_fill_manual(
    values = c(
      "All events" = "#FF6600",
      "Change in control" = "#1F77B4"
    )
  ) +
  labs(
    x = "Date",
    y = "Number of Events",
    title = "Events per Day"
  ) + expand_limits(y = 0)+
  theme_minimal() +
  geom_hline(yintercept = 0,color="#FF6600")+
  theme(
    axis.text.x = element_text(
      colour = "white",
      face = "bold",
      size = 16
    ),
    axis.text.y = element_text(
      colour = "white",
      face = "bold",
      size = 16
    ),
    plot.background  = element_rect(fill = "transparent", colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.box.background = element_rect(fill = "transparent", colour = NA),
    legend.position = "none"
  )+labs(x = NULL, y = NULL,title = NULL)

ggsave(
  "./plots/MTC/distribution_events_count_and_control_without_legend.png",
  bg = "transparent",
  width = 10,
  height = 5
)


# M23 violent events:
unique(gdf$event_type)

m23_events_violence = gdf%>%
  filter(actor1 == "M23: March 23 Movement" | actor2 == "M23: March 23 Movement")%>%
  filter(event_type %in% c("Violence against civilians","Battles", "Explosions/Remote violence"))%>%
  filter(sub_event !="Non-violent transfer of territory")%>%
  rename(geometry = geom)

nrow(m23_events_violence)/nrow(m23_events)


cnt_per_month_m23_violence <- m23_events_violence %>%
  mutate(month = floor_date(event_date, "month")) %>%
  count(month, name = "count") %>%
  complete(
    month = seq(min(month), max(month), by = "month"),
    fill = list(count = 0)
  )

ggplot(cnt_per_month_m23_violence, aes(x = month, y = count,group = 1)) +
  geom_col(fill = "#FF6600",color = "black") +
  labs(
    x = "Date",
    y = "Number of Events",
    title = "Events per Day"
  ) + expand_limits(y = 0)+
  theme_minimal() +
  geom_hline(yintercept = 0,color="#FF6600")+
  theme(
    axis.text.x = element_text(
      colour = "white",
      face = "bold",
      size = 16
    ),
    axis.text.y = element_text(
      colour = "white",
      face = "bold",
      size = 16
    ),
    plot.background  = element_rect(fill = "transparent", colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.box.background = element_rect(fill = "transparent", colour = NA)
  )+labs(x = NULL, y = NULL,title = NULL)

ggsave(
  "./plots/MTC/distribution_violent_events_count.png",
  bg = "transparent",
  width = 10,
  height = 5
)




# controle and violent events next to each other

m23_events = gdf%>%
  filter(actor1 == "M23: March 23 Movement" | actor2 == "M23: March 23 Movement")%>%
  rename(geometry = geom)

cnt_per_month_m23_violence_controle <- m23_events %>%
  mutate(month = floor_date(event_date, "month")) %>%
  group_by(month) %>%
  summarise(
    count_violence = sum(
      event_type %in% c("Violence against civilians",
                        "Battles",
                        "Explosions/Remote violence") &
        sub_event != "Non-violent transfer of territory",
      na.rm = TRUE
    ),
    controle_count = sum(
      sub_event %in% c("Government regains territory",
                       "Non-state actor overtakes territory",
                       "Non-violent transfer of territory"),
      na.rm = TRUE
    ),
    .groups = "drop"
  )


plot_data <- cnt_per_month_m23_violence_controle %>%
  pivot_longer(
    cols = c(count_violence, controle_count),
    names_to = "type",
    values_to = "n"
  ) %>%
  mutate(
    type = recode(
      type,
      count_violence = "All events",
      controle_count = "Change in control"
    )
  )


ggplot(plot_data, aes(x = month, y = n, fill = type)) +
  geom_col(position = "dodge") +
  scale_fill_manual(
    name = NULL,
    values = c(
      "All events" = "#FF6600",
      "Change in control" = "#1F77B4"
    )
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = NULL
  ) +
  expand_limits(y = 0) +
  theme_minimal() +
  geom_hline(yintercept = 0, color = "#FF6600") +
  theme(
    axis.text.x = element_text(
      colour = "white",
      face = "bold",
      size = 16
    ),
    axis.text.y = element_text(
      colour = "white",
      face = "bold",
      size = 16
    ),
    plot.background  = element_rect(fill = "transparent", colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.box.background = element_rect(fill = "transparent", colour = NA),
    legend.text = element_text(colour = "white"),
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

ggsave(
  "./plots/MTC/distribution_violent_events_count_and_control_with_legend.png",
  bg = "transparent",
  width = 10,
  height = 5
)
