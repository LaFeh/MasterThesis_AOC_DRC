setwd("D:/DRC/gaussian_process_AOC")
library(sf)
library(dplyr)
library(terra)


# get OSM DATA
# 1. Install and load necessary libraries
# install.packages(c("osmdata", "sf", "tidyverse"))
library(osmdata)
library(sf)
library(tidyverse)

# 2. Get the official administrative boundary of North Kivu
# This uses OpenStreetMap's Nominatim to dynamically fetch the geometry
nk_boundary_osm <- getbb("North Kivu, Democratic Republic of the Congo", format_out = "sf_polygon")

# Extract the main polygon/multipolygon geometry of the province
nk_boundary <- nk_boundary_osm$sf_polygon
if (is.null(nk_boundary)) {
  # Fallback: manual bounding box for North Kivu if the polygon API times out
  # Longitude: 27.22 to 30.00 | Latitude: -2.07 to 0.96
  nk_boundary <- st_as_sfc(st_bbox(c(xmin=27.22, ymin=-2.07, xmax=30.00, ymax=0.96), crs = 4326))
}

# 3. Create the Overpass API query using the bounding box of North Kivu
nk_bbox <- st_bbox(nk_boundary)

# 4. Fetch the base landuse/infrastructure data (e.g., settlements, roads, grass)
message("Fetching base area data...")
base_query <- opq(bbox = nk_bbox, timeout = 300) %>%
  add_osm_feature(key = "landuse", value = c("residential", "commercial", "industrial", "farmyard", "grass")) %>%
  osmdata_sf()
save(base_query, file = "./data/base_query_farm.RData")

# Combine base polygons and multipolygons into a single spatial layer
base_polygons <- bind_rows(base_query$osm_polygons, base_query$osm_multipolygons)
rm(base_query)
gc()
# 5. Fetch forest and wood polygons to be excluded
message("Fetching forest and wood data...")
forest_query <- opq(bbox = nk_bbox, timeout = 300) %>%
  add_osm_feature(key = "landuse", value = "forest") %>%
  osmdata_sf()
save(forest_query, file = "./data/forest_query.RData")
wood_query <- opq(bbox = nk_bbox, timeout = 1000) %>%
  add_osm_feature(key = "natural", value = "wood") %>%
  osmdata_sf()
save(wood_query, file = "./data/wood_query.RData")

# Combine all forest/wood elements
forest_polygons <- bind_rows(
  forest_query$osm_polygons, forest_query$osm_multipolygons,
  wood_query$osm_polygons, wood_query$osm_multipolygons
)

# 6. Crop all data tightly to North Kivu's actual provincial border 
base_cropped <- st_intersection(st_make_valid(base_polygons), nk_boundary)

# 7. Perform the spatial subtraction to exclude forests
message("Excluding forests...")
if (!is.null(forest_polygons) && nrow(forest_polygons) > 0) {
  # Union all forest patches into a single spatial geometry mask
  forest_mask <- st_union(st_make_valid(forest_polygons))
  
  # Keep only the base areas that do NOT overlap with the forest mask
  area_without_forest <- st_difference(st_make_valid(base_cropped), forest_mask)
} else {
  area_without_forest <- base_cropped
}

# 8. Visualize the result
plot(st_geometry(nk_boundary), border = "red", main = "North Kivu: Non-Forest Areas")
plot(st_geometry(area_without_forest), col = "lightgreen", add = TRUE)

# 9. Optional: Save your cleaned dataset
# st_write(area_without_forest, "north_kivu_no_forest.geojson", delete_dsn = TRUE)

















#######



#grid = read_sf("./data/grid_surface.shp")
load("./data/frontline_data_all_previous_mnths_controle_num.RData")
id_one_month = which(frontline_data_controle_num_all_previous_time$time == unique(frontline_data_controle_num_all_previous_time$time)[1])
grid = frontline_data_controle_num_all_previous_time[id_one_month,]#
library(units)
grid$shape_area = drop_units(st_area(grid))

river = st_read("./data/HydroRIVERS_v10_af.gdb/HydroRIVERS_v10_af.gdb")
# for now only consider rivers of order bigger than 5, cause the others might be easily walkthrough



## add my rivers

# gpkg_path <- "./data/congo-democratic-republic-260530-free.gpkg/congo-democratic-republic.gpkg"
# 
# water2 <- st_read(gpkg_path, layer = "gis_osm_waterways_free") |>
#   st_transform(st_crs(grid)) |>
#   st_crop(st_bbox(grid)) |>
#   filter(fclass !="drain" & fclass != "stream")
# 
# water3 <- st_read(gpkg_path, layer = "gis_osm_water_a_free") |>
#   st_transform(st_crs(grid)) |>  st_crop(st_bbox(grid))
# 
# 
# 
# # Start with df1
# water_result <- water3
# 
# # Remove any overlap from df2
# df2_no_overlap <- st_difference(water2, st_union(water3))
# 
# # Remove empty geometries
# df2_no_overlap <- df2_no_overlap[!st_is_empty(df2_no_overlap), ]
# df2_no_overlap = st_buffer(df2_no_overlap, units::set_units(1, "m"))
# # Combine
# water_all <- rbind(water_result[,c("osm_id","code","fclass","name")], df2_no_overlap[,c("osm_id","code","fclass","name")])
# rm(water2,water3)
# 
# 
# 
# water_grid <- st_intersection(water_all, grid)
# 
# water_area <- water_grid %>%
#   mutate(water_area = drop_units(st_area(geom))) %>%
#   st_drop_geometry() %>%
#   group_by(cell_id) %>%
#   summarise(
#     water_area = sum(water_area)
#   )
# 
# 
# grid <- grid %>%
#   left_join(water_area, by = "cell_id")
# 
# 
# grid$water = grid$water_area/grid$shape_area
# grid$lg_water = log(grid$water)
# plot(grid[,"lg_water"])


grid = st_transform(grid,st_crs(river))
#river = st_transform(river,st_crs(grid))
river = st_crop(river,st_bbox(grid))

plot(river[,"ORD_FLOW"])
river = river[which(river$ORD_FLOW>6),]

##
library(leaflet)
library(sf)

# Color palette
pal <- colorFactor(
  palette = c("blue", "red"),
  domain = levels(river$MAIN_RIV)
)

leaflet(st_transform(river, 4326)) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolylines(
    color = ~pal(MAIN_RIV),
    weight = 3,
    opacity = 1,
    smoothFactor = 0.5,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "yellow",
      bringToFront = TRUE
    )
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal,
    values = ~MAIN_RIV,
    title = "MAIN_RIV"
  )








plot(river)
river_grid <- st_intersection(river, grid)



river_lengths <- river_grid %>%
  mutate(river_length = st_length(Shape)) %>%
  st_drop_geometry() %>%
  group_by(cell_id) %>%
  summarise(
    river_length = sum(river_length)
  )

grid <- grid %>%
  left_join(river_lengths, by = "cell_id")

grid$lg_river_length = log(grid$river_length)
grid$lg_river_length = ifelse(is.na(grid$lg_river_length),0,grid$lg_river_length)
grid$water = grid$river_length/grid$shape_area
grid$water = drop_units(grid$water)
grid$lg_water = log(grid$water)
grid$lg_water = ifelse(is.infinite(grid$lg_water),NA,grid$lg_water)

grid$river_length = ifelse(is.na(grid$river_length),0,grid$river_length)

grid$shape_area = drop_units(st_area(grid))

summary(grid$water)
grid$building_per_cell = grid$building_area/grid$shape_area


##
library(leaflet)
library(sf)

# Color palette
pal <- colorNumeric(
  palette = "viridis",
  domain = grid$lg_water,
  na.color = "transparent"
)

leaflet(st_transform(grid,4326)) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    fillColor = ~pal(lg_water),
    fillOpacity = 0.3,
    color = "black",
    weight = 0.5,
    smoothFactor = 0.5,
    label = ~paste0("Mix time: ", round(mix_time_mean, 2),
                    "\n building:", round(building_area),
                    "\n area_river: ",round(lg_water,2)),
    highlightOptions = highlightOptions(
      weight = 2,
      color = "white",
      fillOpacity = 1,
      bringToFront = TRUE
    )
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal,
    values = ~lg_water,
    title = "water",
    opacity = 0.8
  )




cor(grid$water,grid$mix_time_mean)
small_mix_time =(grid$mix_time_mean < summary(grid$mix_time_mean)[3])
high_water_area = (grid$water>= summary(grid$water)[3])

grid$unusual = FALSE
grid[which(high_water_area & small_mix_time),]$unusual = TRUE

plot(grid[,"unusual"])

plot(
  grid$water,
  grid$mix_time_mean,
  col = ifelse(grid$unusual, "red", "blue"),
  pch = 19,
  xlab = "Water",
  ylab = "Mix time"
)


# Calculate Mahalanobmix_time_mean# Calculate Mahalanobis distance
vars <- grid[which(grid$surface!="water"), c("mix_time_mean", "water","landcover_mean","building_per_cell")]
vars = vars[which(vars$water>0),]
plot(vars$water,vars$mix_time_mean,xlim = c(0,0.02),ylim = c(0,5000))

plotdf = vars[,c("mix_time_mean", "water","landcover_mean","building_per_cell")]
plotdf$mix_time_mean = log(plotdf$mix_time_mean )
plotdf$water = log(plotdf$water )
plotdf$landcover_mean = log(plotdf$landcover_mean )
plotdf$building_per_cell = log(plotdf$building_per_cell )
plot(plotdf)
cor(vars$water,vars$mix_time_mean)
vars <- st_drop_geometry(vars)
vars = vars[which(vars$water>0),]


df_clean <- na.omit(vars)

md <- mahalanobis(
  df_clean,
  colMeans(df_clean),
  cov(df_clean)
)

# Chi-square cutoff (95%)
cutoff <- qchisq(0.9, df = ncol(vars))

# Remove outliers
df_clean <- df_clean[md <= cutoff, ]

model = lm(mix_time_mean~water+landcover_mean+building_per_cell,data = df_clean)
mean(residuals(model)^2)
# Get predictions with 95% prediction intervals
pred <- predict(model, newdata = df_clean, interval = "prediction", level = 0.95)

# Combine real values, predictions, and intervals
results <- data.frame(
  Real = df_clean$mix_time_mean,
  Predicted = pred[, "fit"],
  Lower_95 = pred[, "lwr"],
  Upper_95 = pred[, "upr"]
)

mean((df_clean$mix_time_mean -pred)^2)

library(ggplot2)
ggplot(df_clean, aes(x = water, y = mix_time_mean)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(
    title = "Linear Regression with 95% Confidence Interval",
    x = "X",
    y = "Y"
  )

library(randomForest)

# 80% training, 20% testing
train_index <- sample(
  1:nrow(df_clean),
  size = 0.8 * nrow(df_clean)
)

train_data <- df_clean[train_index, ]
test_data  <- df_clean[-train_index, ]

# Build Random Forest model
rf_model <- randomForest(
  mix_time_mean~water+landcover_mean+building_per_cell,
  data = train_data,
  ntree = 250,       # number of trees
  mtry = 2,          # number of variables tested at each split
  importance = TRUE
)

test_data$prediction <- predict(rf_model, newdata = test_data)

mean((test_data$mix_time_mean- test_data$prediction)^2)

plot(
  test_data$mix_time_mean,
  test_data$prediction,
  pch = 19,
  xlab = "Observed",
  ylab = "Predicted",
  main = "Random Forest: Observed vs Predicted"
)

abline(0, 1, lwd = 2)


grid$prediction <- predict(rf_model, newdata = grid)

summary_square_error = summary((test_data$mix_time_mean- test_data$prediction)^2)
grid$error = (grid$prediction-grid$mix_time_mean)^2
plot(grid[which(grid$error<summary_square_error[5]),"mix_time_mean"])


plot(
  rf_model$mse,
  type = "l",
  xlab = "Number of trees",
  ylab = "OOB Mean Squared Error",
  main = "Random Forest OOB Error"
)

