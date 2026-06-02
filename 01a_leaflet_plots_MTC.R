#01a leaflet plots
library(leaflet)
library(sf)

center = st_transform(st_centroid(st_combine(grid)),4326)
coords <- st_coordinates(center)

# grid built 3
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
    
  )%>%
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
    
  )%>%
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
