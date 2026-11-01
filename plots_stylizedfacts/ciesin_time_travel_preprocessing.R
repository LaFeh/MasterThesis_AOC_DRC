# ciesin_time_travel
library("tidyterra")
library("ggplot2")
 



tif_path_no_stream <-'./data/travel_time_friction_surface_removed_streams.tif' 
tif_path_stream <-'./data/GRID3_COD_mix_travel_time_friction_surface_v1/GRID3_COD_mix_travel_time_friction_surface_v1.tif' 

mix_time_no_stream=rast(tif_path_no_stream)
mix_time_stream=rast(tif_path_stream)

cod_boundaries <- read_sf("~/MasterThesis_AOC_DRC/data/cod_admin_boundaries.shp",layer = "cod_admin1") %>%
  filter(adm1_name %in% c("Ituri", "Nord-Kivu", "Sud-Kivu")) %>%
 # filter(adm1_name %in% c("Nord-Kivu")) %>%
  st_transform(st_crs(mix_time_stream)) %>%
  group_by(adm1_name) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")


# crop raster to grid bounding box
mix_time_crop_no_stream <- crop(
  mix_time_no_stream,
  vect(cod_boundaries),
  overwrite = F
)

mix_time_crop_stream <- crop(
  mix_time_stream,
  vect(cod_boundaries),
  overwrite = F
)


mix_time_crop_stream = log(mix_time_crop_stream)
mix_time_crop_no_stream = log(mix_time_crop_no_stream)


mix_time_crop_stream_3857 <- terra::project(
  mix_time_crop_stream,
  "EPSG:3857"
)

mix_time_crop_no_stream_3857 <- terra::project(
  mix_time_crop_no_stream,
  "EPSG:3857"
)



library(maptiles)

ext <- terra::ext(mix_time_crop_stream_3857)

buffer_x <- (ext[2] - ext[1]) * 0.1
buffer_y <- (ext[4] - ext[3]) * 0.1#0.005

ext_small <- terra::ext(
  ext[1] - buffer_x,
  ext[2] + buffer_x,
  ext[3] - buffer_y,
  ext[4] + buffer_y
)

extent_poly <- terra::as.polygons(
  ext_small,
  crs = terra::crs(mix_time_crop_stream_3857)
)


basemap <- get_tiles(
  x = extent_poly,
  provider = "Esri.WorldGrayCanvas"
)

basemap_crop <- terra::crop(
  basemap,
  extent_poly
)

p_stream <- ggplot() +
  tidyterra::geom_spatraster_rgb(data = basemap_crop) +
  tidyterra::geom_spatraster(
    data = mix_time_crop_stream_3857,
    alpha = 0.9,
    na.rm =T
  ) +
  geom_sf(
    data = cod_boundaries%>%st_transform("EPSG:3857"),
    fill = NA,
    color = "white",
    linewidth = 0.5
  ) +
  scale_fill_viridis_c(
    option = "magma",
    name = "log Travel Time\nin s/m",
    na.value = NA
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none",
    strip.background = element_blank(),
  )

p_no_stream <- ggplot() +
  tidyterra::geom_spatraster_rgb(data = basemap_crop) +
  tidyterra::geom_spatraster(
    data = mix_time_crop_no_stream_3857,
    alpha = 0.9,
    na.rm =T
  ) +
  geom_sf(
    data = cod_boundaries%>%st_transform("EPSG:3857"),
    fill = NA,
    color = "white",
    linewidth = 0.5
  ) +
  scale_fill_viridis_c(
    option = "magma",
    name = "log Travel Time\nin s/m",
    na.value = NA
  ) +
  labs(
    caption = "Tiles © Esri - Esri, DeLorme, NAVTEQ; © OpenStreetMap contributors"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "right",
    strip.text = element_text(
      size = 14,
      face = "bold",
      margin = margin(b = 2)
    ),
    strip.background = element_blank(),
    plot.caption = element_text(
      hjust = 0.5,
      size = 9
    )
  )

library(patchwork)
p_stream +p_no_stream
