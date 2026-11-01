library(dplyr)
library(ggplot2)
load("./data/acled_territory_mnth.RData")
acled_territory = st_drop_geometry(acled_territory_mnth)%>%
  select(year_mnth,control_num)%>%
  mutate(control_num_plot = if_else(control_num ==0,-1,1))%>%
  arrange(year_mnth)%>%
  group_by(year_mnth)%>%
  summarise(control_num_plot = sum(control_num_plot))%>%
  mutate(cum_control_num_plot = cumsum(control_num_plot))


sum(acled_territory_mnth$control_num==1)
sum(acled_territory_mnth$control_num==0)

library(dplyr)
library(tidyr)
library(lubridate)

acled_territory_quarterly <- acled_territory %>%
  mutate(date = as.Date(paste0(year_mnth, "01"), format = "%Y%m%d")) %>%
  arrange(date) %>%
  complete(
    date = seq(min(date), max(date), by = "month")
  ) %>%
  mutate(
    year_mnth = format(date, "%Y%m"),
    across(-c(date, year_mnth), ~ zoo::na.locf(.x, na.rm = FALSE))
  ) %>%
  filter(substr(year_mnth, 5, 6) %in% c("03", "06", "09", "12")) %>%
  select(-date)

ggplot(data = acled_territory_quarterly) +
  geom_vline(xintercept = 0,linetype = "dotted",
             color = "grey50") +
  geom_line(aes(
    x = cum_control_num_plot,
    y = year_mnth,
    group = 1
  ),
  orientation = "y",
  linetype = "dashed",
  color = "grey50"
  ) +
  scale_color_gradient(
    low = "lightblue",
    high = "darkblue",
    name = "Cumulative control"
  ) +
  geom_point(aes(
    x = cum_control_num_plot,
    y = year_mnth,
    color = cum_control_num_plot
  ), size = 5) +
  geom_text(
    aes(
    x = cum_control_num_plot,
    y = year_mnth,
    label =year_mnth
    ),
  hjust = -0.5,
  size = 3
  ) +

  scale_x_continuous(expand = expansion(mult = c(0.05, 0.15)))+
  ylab(label = "")+
  xlab(label = "Number of locations occupied by M23")+
  theme_minimal()+
  theme(axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="none")
