#01x data quality checks acled


library(dplyr)
library(data.table)
library(R.utils)
library(sf)

setwd("D:/DRC/gaussian_process_AOC")

regions = st_read("./data/cod_admin_boundaries.shp",layer="cod_admin3")

gdf <- st_read("D:/DRC/01_prepare_acled_data/data/acled_event_data.gpkg")

regions = st_transform(regions,st_crs(gdf))

################################

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

gdf$admin1_othername = ifelse(gdf$admin1=="Nord-Kivu","Nord Kivu",gdf$admin1)
gdf$admin1_note_check_a = apply(gdf,1, function(x){return(grepl(x$admin1,x$notes, ignore.case = TRUE))})
gdf$admin1_note_check_b = apply(gdf,1, function(x){return(grepl(x$admin1_othername,x$notes, ignore.case = TRUE))})
gdf$admin1_note_check = ifelse((gdf$admin1_note_check_a+gdf$admin1_note_check_b)>=1,TRUE,FALSE)
sum(!gdf$admin1_note_check)

gdf$admin2_note_check = apply(gdf,1, function(x){return(grepl(x$admin2,x$notes, ignore.case = TRUE))})
sum(!gdf$admin2_note_check)

gdf$admin3_note_check = apply(gdf,1, function(x){return(grepl(x$admin3,x$notes, ignore.case = TRUE))})
sum(!gdf$admin3_note_check)


regions_for_join = regions%>%select(adm1_name,adm2_name,adm2_pcode,geometry)

gdf = st_join(gdf,regions_for_join,st_within)
gdf$admin2_location_check = ifelse(gdf$admin2==gdf$adm2_name,TRUE,FALSE)
gdf$admin1_location_check = ifelse(gdf$admin1==gdf$adm1_name,TRUE,FALSE)
sum(!gdf$admin2_location_check)


# bukombo
bukombo = gdf%>%filter(!admin1_location_check & admin1_note_check)


# ndumna
nduma =gdf%>%filter(admin1_location_check & !admin1_note_check)
'
Bewermana https://www.aljazeera.com/news/2025/1/21/m23-rebels-seize-key-eastern-drc-town-of-minova seems true afterall

'


#beides nicht
both_not_true =gdf%>%filter(!admin1_location_check & !admin1_note_check)


# plot
regions_for_plot = regions_for_join %>%filter(adm1_name %in% c("Ituri","Sud-Kivu","Nord-Kivu"))
plot(regions_for_plot[,c("adm1_name")]$geometry)
#nduma




plot(nduma$geometry,add =T,col = "red")
## bukombo
plot(bukombo$geometry,add =T,col = "green")
plot(both_not_true$geometry,add =T,col = "purple")


##### double cities variance preprocessing
duplicated_towns = readRDS("./data/duplicated_towns.rds")

#nduma[nduma$location %in% unlist(duplicated_towns$all_names),]


duplicated_towns = st_as_sf(duplicated_towns,coords =c("longitude","latitude"), crs = 4326)
duplicated_towns = st_transform(duplicated_towns,st_crs(regions_for_plot))


plot(regions_for_plot$geometry)
plot(duplicated_towns$geometry,col ="red",add =T)


cols <- as.factor(duplicated_towns$name)
plot(regions_for_plot[which(regions_for_plot$adm1_name =="Ituri"),]$geometry)
plot(duplicated_towns["name"], add = TRUE, col = cols,type ="p")
#14 ituri
#13 Nord Kivu
#12 sud kivu


# plots for presentation 13.05


library(sf)
library(leaflet)
library(dplyr)
library(htmlwidgets)
library(leaflet.providers)

# Example popup content


regions_leaflet = st_transform(regions_for_plot, 4326)
nduma_leaflet   = st_transform(nduma, 4326)%>%mutate(
  popup_text = paste0(
    "<b>event_date:</b> ", event_date, "<br>",
    "<b>location:</b> ", location, "<br>",      
    "<b>adm1_name_coordinates:</b> ", adm1_name, "<br>",
    "<b>adm2_name_coordinates:</b> ", adm2_name, "<br>",
    "<b>admin1_acled</b> ", admin1, "<br>",
    "<b>admin2_acled</b> ", admin2, "<br>",
    "<b>note:</b> ", notes, "<br>"
  )
)
bukombo_leaflet   = st_transform(bukombo, 4326)%>%mutate(
  popup_text = paste0(
    "<b>event_date:</b> ", event_date, "<br>",
    "<b>location:</b> ", location, "<br>",
    
    "<b>adm1_name_coordinates:</b> ", adm1_name, "<br>",
    "<b>adm2_name_coordinates:</b> ", adm2_name, "<br>",
    "<b>admin1_acled</b> ", admin1, "<br>",
    "<b>admin2_acled</b> ", admin2, "<br>",
    "<b>note:</b> ", notes, "<br>"
  )
)
gdf_leaflet   = st_transform(gdf, 4326)
gdf_leaflet = gdf_leaflet%>%filter(sub_event %in% c("Government regains territory",
                                       "Non-state actor overtakes territory"))%>%mutate(
                                         popup_text = paste0(
                                           "<b>event_date:</b> ", event_date, "<br>",
                                           "<b>location:</b> ", location, "<br>",      
                                           "<b>adm1_name_coordinates:</b> ", adm1_name, "<br>",
                                           "<b>adm2_name_coordinates:</b> ", adm2_name, "<br>",
                                           "<b>admin1_acled</b> ", admin1, "<br>",
                                           "<b>admin2_acled</b> ", admin2, "<br>",
                                           "<b>note:</b> ", notes, "<br>"
                                         )
                                       )


bb <- as.list(st_bbox(regions_leaflet))
## create the visual outliers:

gdf_leaflet_outlier = gdf_leaflet %>%filter(location %in% c("Ndumba","Bukombo","Malemo","Muganzo"))
gdf_leaflet_no_outlier = gdf_leaflet %>%filter(!location %in% c("Ndumba","Bukombo","Malemo","Muganzo"))


outlier_intro <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = regions_leaflet,
              color = "black",
              weight = 1,
              fillOpacity = 0.2,) %>%

  addCircleMarkers(data = gdf_leaflet_no_outlier,
                   color ="blue",
                   radius = 2,
                   popup = ~popup_text) %>%
  addCircleMarkers(data = gdf_leaflet_outlier,
                   color ="red",
                   radius = 2,
                   popup = ~popup_text) %>%

  fitBounds(bb$xmin, bb$ymin, bb$xmax, bb$ymax)

outlier_intro

##
gdf_leaflet_no_outlier%>%filter(geometry != nduma$geometry | geometry != bukombo$geometry )

outlier_ndumba_bukombo <- leaflet() %>%
  addTiles() %>%
  addPolygons(data = regions_leaflet,
              color = "black",
              weight = 1,
              fillOpacity = 0.2,) %>%
  addCircleMarkers(data = gdf_leaflet_no_outlier,
                   color ="blue",
                   radius = 2,
                   opacity = 1,
                   popup = ~popup_text) %>%
  addCircleMarkers(data = nduma_leaflet,group = "Events",
                   color ="darkgreen",radius = 2,
                   popup = ~popup_text) %>%
  addCircleMarkers(data = bukombo_leaflet,group = "Events",
                   color ="red",radius = 2,
                   popup = ~popup_text) %>%
  fitBounds(bb$xmin, bb$ymin, bb$xmax, bb$ymax)

outlier_ndumba_bukombo

# Save as HTML
saveWidget(m, "interactive_map.html", selfcontained = TRUE)