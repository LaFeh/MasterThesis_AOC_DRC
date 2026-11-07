# graph results and grid 3


# ciesin_time_travel
library("tidyterra")
library("ggplot2")




tif_path_no_stream <-'./data/travel_time_friction_surface_removed_streams.tif' 

mix_time_no_stream=rast(tif_path_no_stream)

cod_boundaries <- read_sf("~/MasterThesis_AOC_DRC/data/cod_admin_boundaries.shp",layer = "cod_admin1") %>%
  filter(adm1_name %in% c("Ituri", "Nord-Kivu", "Sud-Kivu")) %>%
  # filter(adm1_name %in% c("Nord-Kivu")) %>%
  st_transform(st_crs(mix_time_no_stream)) %>%
  group_by(adm1_name) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")


# crop raster to grid bounding box
mix_time_crop_no_stream <- crop(
  mix_time_no_stream,
  vect(cod_boundaries),
  overwrite = F
)



mix_time_crop_no_stream_3857 <- terra::project(
  mix_time_crop_no_stream,
  "EPSG:3857"
)


library(maptiles)

ext <- terra::ext(mix_time_crop_no_stream_3857)

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
  crs = terra::crs(mix_time_crop_no_stream_3857)
)


basemap <- get_tiles(
  x = extent_poly,
  provider = "Esri.WorldGrayCanvas"
)

basemap_crop <- terra::crop(
  basemap,
  extent_poly
)


grid = read_sf(paste0("./data/",name_of_grid))
grid = st_transform(grid,"EPSG:3857")

summary <- exactextractr::exact_extract(mix_time_crop_no_stream_3857, grid, 'mean')

grid$mix_time_mean <- summary
grid$lg_mix_time_mean = log(grid$mix_time_mean)


p_no_stream <- ggplot() +
  tidyterra::geom_spatraster_rgb(data = basemap_crop) +
  geom_sf(
    data = grid,
    aes(fill = lg_mix_time_mean),
    lwd = 0,
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





# modep_no_stream <- ggplot() +
#   tidyterra::geom_spatraster_rgb(data = basemap_crop) +
#   tidyterra::geom_spatraster(
#     data = log(mix_time_crop_no_stream_3857),
#     alpha = 0.9,
#     na.rm =T
#   ) +
#   geom_sf(
#     data = cod_boundaries%>%st_transform("EPSG:3857"),
#     fill = NA,
#     color = "white",
#     linewidth = 0.5
#   ) +
#   scale_fill_viridis_c(
#     option = "magma",
#     name = "log Travel Time\nin s/m",
#     na.value = NA
#   ) +
#   labs(
#     caption = "Tiles © Esri - Esri, DeLorme, NAVTEQ; © OpenStreetMap contributors"
#   ) +
#   theme_minimal() +
#   theme(
#     panel.grid = element_blank(),
#     axis.title = element_blank(),
#     axis.text = element_blank(),
#     axis.ticks = element_blank(),
#     legend.position = "right",
#     strip.text = element_text(
#       size = 14,
#       face = "bold",
#       margin = margin(b = 2)
#     ),
#     strip.background = element_blank(),
#     plot.caption = element_text(
#       hjust = 0.5,
#       size = 9
#     )
#   )


source("./00b_helper_create_grid_name.R")
source("./05_model/05a_a_helper_run_model.R")
source("./05_model/05a_b_helper_parameter_grid.R")
source("./04b_a_helper_functions.R")

# ============================================================
# Prepare data
# ============================================================
covariates_list = create_covariates_list()
gridname <- create_grid_name(
  add_streets = T,
  add_nationalparks = T,
  add_waterways = T,
  street_size = "",
  cell_size = 3000
)

estimate_rho = T
quality_strict = T

if(quality_strict ==""){
  model_dirs <- paste0(
    "~/MasterThesis_AOC_DRC/05_model/model_",
    names(parameter_grid),
    "_",
    gsub(".shp", "", gridname)#,"_estimate_rho"
  )
  
}else{
  model_dirs <- paste0(
    "~/MasterThesis_AOC_DRC/05_model/model_quality_",quality_strict,"_",
    names(parameter_grid),
    "_",
    gsub(".shp", "", gridname)#,"_estimate_rho"
  )
  
  
}


if(estimate_rho){
  
  model_dirs = model_dirs[grepl("estimaterho",model_dirs)]
  parameter_grid = parameter_grid[grepl("estimaterho",parameter_grid)]
  
}else{
  model_dirs <- paste0(
    "~/MasterThesis_AOC_DRC/05_model/model_",
    names(parameter_grid),
    "_",
    gsub(".shp", "", gridname)
  )
  
  model_dirs = model_dirs[grepl("fixedrho",model_dirs)]
  parameter_grid = parameter_grid[grepl("fixedrho",parameter_grid)]
}





#model_dirs = gsub("estimaterho_","",model_dirs)

date <- "202412"

if(quality_strict==""){
  load(paste0("./data/data_for_prediction/",gsub(".shp","",gridname),"/",date,"_events.RData"))
}else{
  load(paste0("./data/data_for_prediction/",gsub(".shp","",gridname),"_quality_", quality_strict,"/",date,"_events.RData"))
}


# ============================================================
# Find existing model directories
# ============================================================

files <- paste0(
  "~/MasterThesis_AOC_DRC/05_model/",
  list.files("~/MasterThesis_AOC_DRC/05_model")
)


model_dirs <- model_dirs[
  which(model_dirs %in% files)
]


p_no_stream <- ggplot() +
  geom_sf(
    data = grid[which(grid$name=="Nord-Kivu"),],
    aes(fill = lg_mix_time_mean),
    lwd = 0,
    na.rm =T
  ) +
  geom_sf(
    data = cod_boundaries%>%filter(adm1_name =="Nord-Kivu")%>%st_transform("EPSG:3857"),
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

for(date in c("202404","202405","202406")){#"202411","202412","202501","202502","202503")){
  
  model_idx = 5
  
  model_name = names(parameter_grid)[model_idx]
  print(model_name)
  model_dir = model_dirs[model_idx]
  if(model_name =="streets_first_degree_no_dist_noIntercept_estimaterho"){
    next()
  }
  
  
  load(paste0(model_dir, "/",date,"_report.RData"))
  load(paste0(model_dir, "/",date,"_report_vals.RData"))
  
  if(estimate_rho){
    tau = exp(rep$value["tau"])
    rho = plogis(rep$value["rho"])
    # fixed = summary(rep,"fixed")
    # tau = exp(fixed[,"Estimate"]["log_tau"])
    # rho = plogis(fixed[,"Estimate"]["logit_rho"])
    # 
  }else{
    tau = exp(rep$value["tau"])
    rho = plogis(rep$value["rho"])
    
  }
  
  
  
  
  covariates_title = parameter_grid[[model_name]]$model$covariates_title
  X = model.matrix(as.formula(covariates_list[[covariates_title]]),data)
  
  data$phi_w = rep$par.random
  data$phi_w_plogis = plogis(rep$par.random)
  betas = rep$par.fixed[names(rep$par.fixed)=="beta"]
  data_subset = data[which(data$name =="Nord-Kivu"),]
  if(length(betas)>1){
    betas_minus_intercept = betas[2:length(betas)]
    X_minus_intercept = X[, -1, drop = FALSE]
    data$risk = plogis(X_minus_intercept %*% as.vector(betas_minus_intercept) + data$phi_w)
    data$p_w = plogis(betas+data$phi_w)
  } else{
    data$risk = plogis(data$phi_w)
    data$p_w = data$risk
  }
  
  
  
  data$phi_mean   <- as.numeric(plogis(report_vals$phi))
  data_subset = data[which(data$name =="Nord-Kivu"),]
  
  
  
  ### comparison area 
  risky_cells = data[which(data$risk>mean(data$risk)),]
  # # 
  # library(sf)
  # library(dplyr)
  # tolerance <- 1500 # CRS units
  # # 
  # buffered_cells <- st_buffer(risky_cells, tolerance)
  # parts <- st_union(buffered_cells) |> st_cast("POLYGON") |> st_as_sf()
  # parts$cluster_id <- seq_len(nrow(parts))
  # 
  # # tag each ORIGINAL cell with the cluster polygon it falls in
  # risky_cells$cluster_id <- st_join(
  #   st_centroid(risky_cells),
  #   parts,
  #   join = st_intersects
  # )$cluster_id
  # 
  # # now union the ORIGINAL (unbuffered) geometries per cluster
  # clusters <- risky_cells |>
  #   group_by(cluster_id) |>
  #   summarise(geometry = st_union(geometry), n_cells = n(), .groups = "drop")
  # 
  # clusters$area = units::drop_units(st_area(clusters))
  # aoc = clusters[which(clusters$area ==max(clusters$area)),]
  aoc = st_union(risky_cells)
  aoc_wo_holes = nngeo::st_remove_holes(aoc)
  
  territory = acled_territory_mnth%>%filter(year_mnth<=date)%>%
    group_by(geometry)%>%slice_max(order_by = year_mnth, n = 1)%>%st_transform(st_crs(grid))
  
  plt = p_no_stream+geom_sf(data=aoc,alpha = 0.5,fill ="white")
    
  plt2 = plt + geom_sf(data=territory,aes(colour = factor(control_num)),size = 1)+  scale_colour_manual(
      values = c("0" = "red", "1" = "blue"),
      name = "Control"
    )
  
  ggsave(plot = plt2,filename = paste0(date,"_aoc_travel_time_territory.png"))

  ggsave(plot = plt,filename = paste0(date,"_aoc_travel_time.png"))
  
  
  
  
  
  
  
  
  
}

