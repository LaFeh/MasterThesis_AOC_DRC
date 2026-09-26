#o6_evaluate_pretty

# 05a run model

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

max_width = 8

# ============================================================
# Prepare data
# ============================================================
covariates_list = create_covariates_list()
gridname <- create_grid_name(
  add_streets = T,
  add_nationalparks = T,
  add_waterways = T,
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

date <- "202411"

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


# ============================================================
# Helper function for plotting PNGs
# ============================================================
#
# The important part:
# We preserve the original aspect ratio of the PNG.
#
# max_width  = maximum allowed width
# max_height = maximum allowed height
#
# The image will be fitted inside this box without distortion.
# ============================================================



# ============================================================
# Run through models and create comparison PDF
# ============================================================

cairo_pdf(
  paste0(
    date,
    "_model_comparison_quality_",quality_strict, "_",
    gsub(".shp", "", gridname),"_ggplot2_",ifelse(estimate_rho,"estimate_rho","fixed_rho"),
    ".pdf"
  ),
  width = 9,
  height = 11
)


for (model_idx in 1:length(names(parameter_grid))) {
  
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

  p1 <- ggplot2::ggplot(data_subset) +
    geom_sf(aes(fill =  phi_mean),
            color = "grey",
            linewidth = 0.01,
            alpha =0.5) +
    scale_fill_viridis_c() +
    theme_minimal() 
  
  
  compare_ipis = T
  if (compare_ipis){
    
    if(date == "202405"){
      ipis_map = read_sf("./data/IPIS_maps/2024/2024_05_may_M23_aoi_ipis.gpkg")
    } else if (date =="202411"){
      ipis_map = read_sf("./data/IPIS_maps/2024/2024_11_nov_M23_aoi_ipis.gpkg")
    } else {
      ipis_map <- NULL
    }
    
    if (!is.null(ipis_map)) {
      
      
      ipis_map = st_transform(st_union(ipis_map),st_crs(data))
      aoc = st_union(data[which(data$risk>mean(data$risk)),])
      
      p2 <- ggplot2::ggplot() +
        ggplot2::geom_sf(
          data = st_as_sf(data_subset),
          fill = NA,
          color = "grey",
          linewidth = 0.01,
          alpha =0.5
        ) +
        geom_sf(data = st_as_sf(aoc), fill = "blue", alpha = 0.5) +
        geom_sf(data = st_as_sf(ipis_map), fill = "red", alpha = 0.5) +
        theme_minimal() 
      
    }
    
    print(p1 + p2 + patchwork::plot_layout(ncol = 2))
  
  
    grid.text(
      paste0(basename(model_dir),
             "\n rho: ",round(rho,6), 
             "\n tau: ",round(tau,6)),
      x = 0.5,
      y = 0.97,
      gp = gpar(
        fontsize = 8
      )
    )
    
    grid.text(
      as.formula(covariates_list[[covariates_title]]),
      x = 0.5,
      y = 0.93,
      gp = gpar(
        fontsize = 8
      )
    )
    grid.text(
      paste(
        names(betas),
        plogis(round(betas, 4)),
        collapse = "\n"
      ),
      x = 0.5,
      y = 0.87,
      gp = gpar(
        fontsize = 8
      )
    )
  
  
  # ----------------------------------------------------------
  # New page for each model
  # ----------------------------------------------------------
  
  grid.newpage()
  
  }
  
}


# ============================================================
# Close PDF
# ============================================================

dev.off()




## comparison ipis area:


for (model_idx in 1:length(names(parameter_grid))) {
  
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
  
  p1 <- ggplot2::ggplot(data_subset) +
    geom_sf(aes(fill =  phi_mean),
            color = "grey",
            linewidth = 0.01,
            alpha =0.5) +
    scale_fill_viridis_c() +
    theme_minimal() 
  
  
  ### comparison area 
  risky_cells = data[which(data$risk>mean(data$risk)),]
  
  library(sf)
  library(dplyr)
  library(igraph)
  
  tolerance <- 1500 # CRS units
  
  buffered_cells <- st_buffer(risky_cells, tolerance)
  parts <- st_union(buffered_cells) |> st_cast("POLYGON") |> st_as_sf()
  parts$cluster_id <- seq_len(nrow(parts))
  
  # tag each ORIGINAL cell with the cluster polygon it falls in
  risky_cells$cluster_id <- st_join(
    st_centroid(risky_cells),
    parts,
    join = st_intersects
  )$cluster_id
  
  # now union the ORIGINAL (unbuffered) geometries per cluster
  clusters <- risky_cells |>
    group_by(cluster_id) |>
    summarise(geometry = st_union(geometry), n_cells = n(), .groups = "drop")
  
  clusters$area = units::drop_units(st_area(clusters))
  aoc = clusters[which(clusters$area ==max(clusters$area)),]
  
  aoc_wo_holes = nngeo::st_remove_holes(aoc)
  
  
  
  parts = st_cast(st_union(risky_cells),"POLYGON")
  parts = st_as_sf(parts)
  parts$area = units::drop_units(st_area(parts$x))
  parts$id = 1:nrow(parts)
  aoc_idx = which(parts$area==max(parts$area))
  aoc = parts[aoc_idx,]
  #plot(aoc)
  #plot(nngeo::st_remove_holes(aoc))
  #plot(st_boundary(st_union(grid)))
  
  
  

  buffered_data = st_buffer(data, 1500)
  

  buffered_union = st_union(buffered_data)
  union <- st_buffer(buffered_union,-1500)
  boundary = st_boundary(union)

  boundary_zone <- st_buffer(boundary,10)
  boundary_cells <- data[
    st_intersects(data, st_as_sf(boundary_zone), sparse = FALSE)[, 1] | st_touches(data, st_as_sf(boundary), sparse = FALSE)[, 1],
  ]
  
  boundary_at_aoc = st_intersects(boundary_cells, aoc,sparse = F)
  boundary_mat = st_intersects(st_buffer(boundary_cells,1500), sparse = F)
  boundary_at_aoc_2 = boundary_mat%*%boundary_mat#%*%boundary_mat%*%boundary_mat%*%boundary_mat
  
  k = apply(boundary_at_aoc_2, 2,function(x) {which(x==1)})
  touching_2_aoc_boundary = unique(unlist(k[boundary_at_aoc]))

  plot(boundary_cells[unique(c(which(boundary_at_aoc),touching_2_aoc_boundary)),]$geometry,col = "red")
  plot(boundary_cells[unique(c(which(boundary_at_aoc))),]$geometry,col = "blue")
  
  touches_boundary <- st_touches(boundary_cells)
  
  edges <- do.call(
    rbind,
    lapply(seq_along(touches_boundary), function(i) {
      if (!length(touches_boundary[[i]])) return(NULL)
      cbind(i, touches_boundary[[i]])
    })
  )
  
  g <- graph_from_edgelist(edges, directed = FALSE)
  
  components(g)$membership
  
  
  
  
  plot(boundary_cells[boundary_at_aoc,]$geometry)
  
  combined =st_union(c(boundary_cells$geometry,aoc$x))
  wo_holes = nngeo::st_remove_holes(combined)
  plot(wo_holes,col = "red")
  
  plot(boundary_cells$geometry,add =T,col = "red")
  plot(aoc$x)
  
  compare_ipis = T
  if (compare_ipis){
    
    if(date == "202405"){
      ipis_map = read_sf("./data/IPIS_maps/2024/2024_05_may_M23_aoi_ipis.gpkg")
    } else if (date =="202411"){
      ipis_map = read_sf("./data/IPIS_maps/2024/2024_11_nov_M23_aoi_ipis.gpkg")
    } else {
      ipis_map <- NULL
    }
    
    if (!is.null(ipis_map)) {
      
      
      ipis_map = st_transform(st_union(ipis_map),st_crs(data))
      aoc = st_union(data[which(data$risk>mean(data$risk)),])
      
      
      
      p2 <- ggplot2::ggplot() +
        ggplot2::geom_sf(
          data = st_as_sf(data_subset),
          fill = NA,
          color = "grey",
          linewidth = 0.01,
          alpha =0.5
        ) +
        geom_sf(data = st_as_sf(aoc), fill = "blue", alpha = 0.5) +
        geom_sf(data = st_as_sf(ipis_map), fill = "red", alpha = 0.5) +
        theme_minimal() 
      
    }
    
    print(p1 + p2 + patchwork::plot_layout(ncol = 2))
    
    
    grid.text(
      paste0(basename(model_dir),
             "\n rho: ",round(rho,6), 
             "\n tau: ",round(tau,6)),
      x = 0.5,
      y = 0.97,
      gp = gpar(
        fontsize = 8
      )
    )
    
    grid.text(
      as.formula(covariates_list[[covariates_title]]),
      x = 0.5,
      y = 0.93,
      gp = gpar(
        fontsize = 8
      )
    )
    grid.text(
      paste(
        names(betas),
        plogis(round(betas, 4)),
        collapse = "\n"
      ),
      x = 0.5,
      y = 0.87,
      gp = gpar(
        fontsize = 8
      )
    )
    
    
    # ----------------------------------------------------------
    # New page for each model
    # ----------------------------------------------------------
    
    grid.newpage()
    
  }
  
}

