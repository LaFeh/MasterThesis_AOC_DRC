# evaluate based on ipis map:


calculate_overlapping_area_percentage<- function(x,y){
  
  overlap = st_intersection(st_simplify(x),st_simplify(y))
  area_overlap = st_area(overlap)
  
  x_area = st_area(x)
  y_area = st_area(y)
   
  percentage_overlap = area_overlap*2/(x_area+y_area)*100
  
  return(percentage_overlap)
}


library(Matrix)
library(grid)
library(png)
library(dplyr)
library(ggplot2)
library(patchwork)

source("./00b_helper_create_grid_name.R")
source("./05_model/05a_a_helper_run_model.R")
source("./05_model/05a_b_helper_parameter_grid.R")
source("./04b_a_helper_functions.R")

# ============================================================
# Prepare data
# ============================================================
covariates_list = create_covariates_list()

cell_size = 3000 
gridname <- create_grid_name(
  add_streets = T,
  add_nationalparks = T,
  add_waterways = T,
  cell_size = cell_size
)

estimate_rho = T
quality_strict = T
all_months = c(paste0("0",1:9),10:12)
all_years = c(2024,2025)
all_dates= c(paste0(all_years[1], all_months),paste0(all_years[2], all_months))

#date <- "202412"

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


all_months = c(paste0("0",1:9),10:12)
all_years = c(2024,2025)
all_dates= c(paste0(all_years[1], all_months),paste0(all_years[2], all_months))


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


library(future)
library(future.apply)

data_date_df = data.frame(date=all_dates)
data_date_df$data_geom = NA
data_date_df$unioned_data_geom = NA

datex= all_dates[1]
if(quality_strict==""){
  load(paste0("./data/data_for_prediction/",gsub(".shp","",gridname),"/",datex,"_events.RData"))
}else{
  load(paste0("./data/data_for_prediction/",gsub(".shp","",gridname),"_quality_", quality_strict,"/",datex,"_events.RData"))
}


unioned_data <- st_union(data)

total_boundary <- st_boundary(
  unioned_data
)


jobs <- expand.grid(
  model_idx = seq_along(parameter_grid),
  date = all_dates,
  stringsAsFactors = FALSE
)

# Number of workers
# Use parallelly::availableCores() - 1 if you want to leave one core free
n_workers <- 6 #max(1, parallelly::availableCores() - 1)

plan(multisession, workers = n_workers)

model_names <- names(parameter_grid)


results <- future_lapply(
  seq_len(nrow(jobs)),
  function(i) {
    
    model_idx <- jobs$model_idx[i]
    date <- jobs$date[i]
    
    
    model_name <- model_names[model_idx]

    model_dir <- model_dirs[model_idx]
    
    if (model_name == "streets_first_degree_no_dist_noIntercept_estimaterho") {
      next
    }
    

    message(
      "Starting: worker=", Sys.getpid(),
      " | model=", model_name,
      " | date=", date
    )
    
    # --------------------------------------------------
    # Load model results
    # --------------------------------------------------
    if(file.exists(paste0(model_dir, "/", date, "_report.RData"))){
      load(
        paste0(model_dir, "/", date, "_report.RData")
      )
    }else{
      return(NULL)
    }
    
    load(
      paste0(model_dir, "/", date, "_report_vals.RData"),
      envir = environment()
    )
    
    if(quality_strict==""){
      load(paste0("./data/data_for_prediction/",gsub(".shp","",gridname),"/",date,"_events.RData"))
    }else{
      load(paste0("./data/data_for_prediction/",gsub(".shp","",gridname),"_quality_", quality_strict,"/",date,"_events.RData"))
    }
    
    
    # --------------------------------------------------
    # Extract parameters
    # --------------------------------------------------
    
    if (estimate_rho) {
      tau <- exp(rep$value["tau"])
      rho <- plogis(rep$value["rho"])
    } else {
      tau <- exp(rep$value["tau"])
      rho <- plogis(rep$value["rho"])
    }
    
    # --------------------------------------------------
    # Construct design matrix
    # --------------------------------------------------
    
    covariates_title <- parameter_grid[[model_name]]$model$covariates_title
    
    X <- model.matrix(
      as.formula(covariates_list[[covariates_title]]),
      data
    )
    
    data$phi_w <- rep$par.random
    data$phi_w_plogis <- plogis(rep$par.random)
    
    betas <- rep$par.fixed[names(rep$par.fixed) == "beta"]
    
    data_subset <- data[data$name == "Nord-Kivu", ]
    
    if (length(betas) > 1) {
      
      betas_minus_intercept <- betas[2:length(betas)]
      X_minus_intercept <- X[, -1, drop = FALSE]
      
      data$risk <- plogis(
        X_minus_intercept %*% as.vector(betas_minus_intercept) +
          data$phi_w
      )
      
      data$p_w <- plogis(betas + data$phi_w)
      
    } else {
      
      data$risk <- plogis(data$phi_w)
      data$p_w <- data$risk
      
    }
    
    data$phi_mean <- as.numeric(plogis(report_vals$phi))
    
    data_subset <- data[data$name == "Nord-Kivu", ]
    
    # --------------------------------------------------
    # Find risky cells
    # --------------------------------------------------
    
    risky_cells <- data[
      data$risk > mean(data$risk, na.rm = TRUE),
    ]
    
    # --------------------------------------------------
    # Prepare comparison area
    # --------------------------------------------------
    
    tolerance <- cell_size
    
    buffered_cells <- st_buffer(
      risky_cells,
      tolerance
    )
    
    parts <- st_union(buffered_cells) |>
      st_cast("POLYGON") |>
      st_as_sf()
    
    parts$cluster_id <- seq_len(nrow(parts))
    
    # Tag each original cell with the cluster polygon
    risky_cells$cluster_id <- st_join(
      st_centroid(risky_cells),
      parts,
      join = st_intersects
    )$cluster_id
    
    # Union original geometries by cluster
    clusters <- risky_cells |>
      group_by(cluster_id) |>
      summarise(
        geometry = st_union(geometry),
        n_cells = n(),
        .groups = "drop"
      )
    
    clusters$area <- units::drop_units(
      st_area(clusters)
    )
    
    aoc <- clusters[
      clusters$area == max(clusters$area),
    ]
    
    aoc_wo_holes <- nngeo::st_remove_holes(aoc)
    
    # --------------------------------------------------
    # Prepare boundary
    # --------------------------------------------------
    
    # unioned_data <- st_union(data)
    # 
    # total_boundary <- st_boundary(
    #   unioned_data
    # )
    
    bounding_box_aoc <- st_bbox(
      aoc_wo_holes
    )
    
    cropped_boundary <- st_crop(
      total_boundary,
      bounding_box_aoc
    )
    
    buffered_cropped_boundary <- st_buffer(
      cropped_boundary,
      1
    )
    
    aoc_and_boundary <- st_union(
      buffered_cropped_boundary,
      aoc
    )
    
    aoc_without_holes <- nngeo::st_remove_holes(
      aoc_and_boundary
    )
    
    # --------------------------------------------------
    # Remove lakes
    # --------------------------------------------------
    
    unioned_lakes <- st_union(
      data[
        data$surface == "water" &
          data$fclass == "water",
      ]
    )
    
    unioned_lakes_boundary <- st_union(
      unioned_lakes,
      buffered_cropped_boundary
    )
    
    aoc_without_holes <- st_difference(
      aoc_without_holes,
      unioned_lakes_boundary
    )
    
    # --------------------------------------------------
    # IPIS map
    # --------------------------------------------------
    
    ipis_map <- get_ipis_map(date)
    
    if("contested" %in% colnames(ipis_map)){
      
      ipis_map = st_make_valid(ipis_map[is.na(ipis_map$contested),])
    }else{
      ipis_map = st_make_valid(ipis_map)
    }
    
    ipis_map <- st_transform(
      st_union(ipis_map),
      st_crs(data)
    )
    
    
    tryCatch({
      overlap_area_percentage <-
        calculate_overlapping_area_percentage(
          st_simplify(ipis_map),
          st_simplify(aoc_without_holes)
        )
      saveRDS(overlap_area_percentage, file = file.path(model_dir,paste0(date,"_overlap_area_percentage.rds")))
      
      
      # --------------------------------------------------
      # IPIS + AOC evaluation plot
      # --------------------------------------------------
      
      ipis_aoc_evaluation <- ggplot2::ggplot() +
        ggplot2::geom_sf(
          data = st_as_sf(data_subset),
          fill = NA,
          color = "grey",
          linewidth = 0.01,
          alpha = 0.5
        ) +
        ggplot2::geom_sf(
          data = st_as_sf(aoc_without_holes),
          fill = "blue",
          alpha = 0.5
        ) +
        ggplot2::geom_sf(
          data = st_as_sf(ipis_map),
          fill = "red",
          alpha = 0.5
        ) +
        ggplot2::theme_minimal()
      
      name <- paste0(
        date,
        "_ipis_aoc_evaluation.png"
      )
      
      ggplot2::ggsave(
        file.path(
          model_dir,
          "plots",
          name
        ),
        ipis_aoc_evaluation
      )
      
      # --------------------------------------------------
      # IPIS + AOC plot
      # --------------------------------------------------
      
      ipis_aoc <- ggplot2::ggplot() +
        ggplot2::geom_sf(
          data = st_as_sf(data_subset),
          fill = NA,
          color = "grey",
          linewidth = 0.01,
          alpha = 0.5
        ) +
        ggplot2::geom_sf(
          data = st_as_sf(aoc),
          fill = "blue",
          alpha = 0.5
        ) +
        ggplot2::geom_sf(
          data = st_as_sf(ipis_map),
          fill = "red",
          alpha = 0.5
        ) +
        ggplot2::theme_minimal()
      
      name <- paste0(
        date,
        "_ipis_aoc.png"
      )
      
      ggplot2::ggsave(
        filename = file.path(
          model_dir,
          "plots",
          name
        ),
        ipis_aoc
      )
      
    }, error = function(e) {
      message("Could not calculate area ", model_dir,"\n",date, ": ", e$message)
      next
    })
  },
  future.seed = TRUE
)


# Return to sequential execution afterwards
plan(sequential)




