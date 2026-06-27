#01a leaflet plots
library(leaflet)
library(sf)

center = st_transform(st_centroid(st_combine(grid)),4326)
coords <- st_coordinates(center)





# grid built only acled 0 
grid = grid%>%filter(adm1_name == "Nord-Kivu")
center = st_transform(st_centroid(st_combine(grid)),4326)
coords <- st_coordinates(center)

load("./data/frontline_data_all_previous_mnths_controle_num.RData")
tm = zoo::as.yearmon(as.Date("2025-02-01"),format = "%Y%M")
data = frontline_data_controle_num_all_previous_time%>%
  filter(time == tm)
data$control_binom = data$control
data[which(data$control_binom ==0.5),]$control_binom = 0
data = data[which(!is.na(data$control_binom)),]
data = data[which(data$control_binom==1),]
data = st_transform(data,4326)
grid = st_transform(grid,4326)

st_centroid(data)
contained = st_contains(grid,st_centroid(data),sparse =F)
cnt = rowSums(contained,na.rm =T)

yellow = grid[which(cnt!=0),]


leaflet(options = leafletOptions(zoomControl = FALSE))%>%
  addTiles() %>%  # OpenStreetMap
  addPolygons(
    data = st_transform(relevant_regions,4326),
    fillColor = "green",
    fillOpacity = 0,
    color = "black",
    weight = 1
  )%>%addPolygons(
    data = st_transform(yellow,4326),
    fillColor = "yellow",
    fillOpacity = 1,
    color = "black",
    weight = 1)  %>%
  setView(lng = coords[1], lat = coords[2],zoom = 8)

# grid built acled 1
grid = grid%>%filter(adm1_name == "Nord-Kivu")
center = st_transform(st_centroid(st_combine(grid)),4326)
coords <- st_coordinates(center)

load("./data/frontline_data_all_previous_mnths_controle_num.RData")
tm = zoo::as.yearmon(as.Date("2025-02-01"),format = "%Y%M")
data = frontline_data_controle_num_all_previous_time%>%
  filter(time == tm)
data$control_binom = data$control
data[which(data$control_binom ==0.5),]$control_binom = 0
data = data[which(!is.na(data$control_binom)),]
data = data[which(data$control_binom==1),]
data = st_transform(data,4326)
grid = st_transform(grid,4326)

st_centroid(data)
contained = st_contains(grid,st_centroid(data),sparse =F)
cnt = rowSums(contained,na.rm =T)

yellow = grid[which(cnt!=0),]


leaflet(options = leafletOptions(zoomControl = FALSE))%>%
  addTiles() %>%  # OpenStreetMap
  addPolygons(data = st_transform(grid,4326),
              fillColor = "purple",
              fillOpacity = 0.4,
              color = "black",
              weight = 1)%>%
  addPolygons(
    data = st_transform(relevant_regions,4326),
    fillColor = "green",
    fillOpacity = 0,
    color = "black",
    weight = 1
    
  )%>%addPolygons(
  data = st_transform(yellow,4326),
  fillColor = "yellow",
  fillOpacity = 1,
  color = "black",
  weight = 1)  %>%
  setView(lng = coords[1], lat = coords[2],zoom = 8)


# grid built wth national parcs acled
leaflet(options = leafletOptions(zoomControl = FALSE))%>%
  addTiles() %>%  # OpenStreetMap
  addPolygons(data = st_transform(grid_without_parks,4326),
              fillColor = "purple",
              fillOpacity = 0.4,
              color = "black",
              weight = 1)%>%
  addPolygons(
    data = st_transform(national_parks_shape,4326),
    fillColor = "green",
    fillOpacity = 0.4,
    color = "black",
    weight = 1
    
  )%>%
  addPolygons(
    data = st_transform(relevant_regions,4326),
    fillColor = "green",
    fillOpacity = 0,
    color = "black",
    weight = 1
    
  )%>%addPolygons(
    data = st_transform(data,4326),
    fillColor = "yellow",
    fillOpacity = 1,
    color = "black",
    weight = 1)%>%
  setView(lng = coords[1], lat = coords[2],zoom = 8)

# grid built 2
library(leaflet)
leaflet(options = leafletOptions(zoomControl = FALSE))%>%
  addTiles() %>%  # OpenStreetMap
  addPolygons(data = st_transform(grid,4326),
              fillColor = "purple",
              fillOpacity = 0.4,
              color = "black",
              weight = 1)%>%
  addPolygons(
    data = st_transform(relevant_regions,4326),
    fillColor = "green",
    fillOpacity = 0,
    color = "black",
    weight = 1
    
  )%>%
  setView(lng = coords[1], lat = coords[2],zoom = 8)


# grid built 1
leaflet(options = leafletOptions(zoomControl = FALSE))%>%
  addTiles() %>%  # OpenStreetMap
  addPolygons(data = st_transform(grid,4326),
              fillColor = "purple",
              fillOpacity = 0.4,
              color = "black",
              weight = 1)%>%
  addPolygons(
    data = st_transform(relevant_regions,4326),
    fillColor = "green",
    fillOpacity = 0,
    color = "black",
    weight = 1
    
  )%>%
  setView(lng = coords[1], lat = coords[2],zoom = 8)


# grid built 4
leaflet(options = leafletOptions(zoomControl = FALSE))%>%
  addTiles() %>%  # OpenStreetMap
  addPolygons(data = st_transform(grid_without_parks,4326),
              fillColor = "purple",
              fillOpacity = 0.4,
              color = "black",
              weight = 1)%>%
  addPolygons(
    data = st_transform(national_parks_shape,4326),
    fillColor = "green",
    fillOpacity = 0.4,
    color = "black",
    weight = 1
    
  )%>%
  addPolygons(
    data = st_transform(water_all,4326),
    fillColor = "blue",
    fillOpacity = 0.4,
    color = "black",
    weight = 1
    
  )%>%
  addPolygons(
    data = st_transform(relevant_regions,4326),
    fillColor = "green",
    fillOpacity = 0,
    color = "black",
    weight = 1
    
  )%>%addPolygons(
    data = st_transform(data,4326),
    fillColor = "yellow",
    fillOpacity = 1,
    color = "black",
    weight = 1) %>%
  setView(lng = coords[1], lat = coords[2],zoom = 8)

# grid built cell difference

set.seed(123)


grid_plot = grid_final

n = unique(grid_plot$cell_id)

id_colors <- setNames(
  rgb(
    runif(n),
    runif(n),
    runif(n)
  ),
  unique(grid_plot$cell_id)
)
grid_plot$fill_col <- id_colors[as.character(grid_plot$cell_id)]

leaflet() %>%
  addPolygons(
    data = st_transform(grid_plot, 4326),
    fillColor = ~fill_col,
    fillOpacity = 0.6,
    color= "black",
    weight= 1
  )





# grid test water
leaflet(options = leafletOptions(zoomControl = FALSE))%>%
  addTiles() %>%  # OpenStreetMap
    addPolygons(data = st_transform(water_all,4326),
    fillColor = "blue",
    fillOpacity = 0.4,
    color = "black",
    weight = 1)#,
    #label = ~as.character( cell_id))
