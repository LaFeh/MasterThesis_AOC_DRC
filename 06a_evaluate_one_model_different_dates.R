# 05a run model

library(Matrix)
library(grid)
library(png)
library(dplyr)
library(openxlsx)

source("./00b_helper_create_grid_name.R")
source("./05_model/05a_a_helper_run_model.R")
source("./05_model/05a_b_helper_parameter_grid.R")
source("./04b_a_helper_functions.R")

max_width = 4
model_name = "streets_first_degree_no_dist_noIntercept_grid_surface_3000_water_park_street_estimate_rho"

yrs =2022:2025
mnths = c(paste0("0",1:9),10:12)

date_combinations = dplyr::cross_join(tidyr::as_tibble(yrs),tidyr::as_tibble(mnths))
dates = paste0(date_combinations$value.x,date_combinations$value.y)
# ============================================================
# Prepare data
# ============================================================

gridname <- create_grid_name(
  add_streets = T,
  add_nationalparks = T,
  add_waterways = T,
  cell_size = 3000
)

model_dir <- paste0(
  "~/MasterThesis_AOC_DRC/05_model/model_",
  model_name)







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


draw_png <- function(file, x, y, max_width_in) {
  
  img <- readPNG(file)
  
  img_width  <- dim(img)[2]
  img_height <- dim(img)[1]
  
  # Original aspect ratio
  aspect_ratio <- img_width / img_height
  
  # Use the specified width
  width_in <- max_width_in
  height_in <- width_in / aspect_ratio
  
  grid.raster(
    img,
    x = unit(x, "npc"),
    y = unit(y, "npc"),
    width = unit(width_in, "inches"),
    height = unit(height_in, "inches")
  )
}


# ============================================================
# CV summary object
# ============================================================

grid_cv_summary <- list()


# ============================================================
# Run through models and create comparison PDF
# ============================================================

pdf(
  paste0(model_dir,"/",
    "model_all_dates.pdf"
  ),
  width = 8.5,
  height = 11
)


for (date in dates) {
  
  # ----------------------------------------------------------
  # New page for each model
  # ----------------------------------------------------------
  
  grid.newpage()
  
  # ----------------------------------------------------------
  # Find plots
  # ----------------------------------------------------------
  
  plot_files <- list.files(
    paste0(model_dir, "/plots"),
    pattern = paste0(
      date,
      ".*\\.(png|jpg|jpeg)$"
    ),
    full.names = TRUE
  )
  
  
  # Make sure plots are in a consistent order
  plot_files <- sort(plot_files)
  
  
  # Check how many plots were found
  if (length(plot_files) != 2) {
    
    warning(
      paste(
        basename(model_dir),
        "has",
        length(plot_files),
        "plots instead of 2"
      )
    )
  }
  
  
  if (length(plot_files) >= 2) {
    draw_png(
      plot_files[2],
      x = 0.73,
      y = 0.68,
      max_width_in = max_width
    )
  }
  
  
  if (length(plot_files) >= 1) {
    draw_png(
      plot_files[1],
      x = 0.27,
      y = 0.68,
      max_width_in = max_width
    )
  }
  
  if (length(plot_files) >= 3) {
    draw_png(
      plot_files[3],
      x = 0.27,
      y = 0.30,
      max_width_in = max_width
    )
  }
  
  # ----------------------------------------------------------
  # settings.txt
  # ----------------------------------------------------------
  
  text_file <- file.path(
    model_dir,
    "settings.txt"
  )
  
  
  if (file.exists(text_file)) {
    
    txt <- paste(
      readLines(text_file),
      collapse = "\n"
    )
    
    grid.text(
      txt,
      x = 0.05,
      y = 0.28,
      just = c("left", "top"),
      gp = gpar(
        fontsize = 8
      )
    )
  }
  
}


# ============================================================
# Close PDF
# ============================================================

dev.off()


# ============================================================
# Read CV summaries and overlapping area
# ============================================================

rho_list = c()

for (date in dates) {
  
  load(paste0(model_dir,"/",date,"_report.RData"))
  fixed = summary(rep, "fixed")    
  rho = plogis(fixed[,"Estimate"]["logit_rho"])
  rho_list = c(rho_list,rho)
}

save(rho_list, paste0(model_dir,"/rho_list.RData"))
mean_rho = mean(rho_list)
save(mean_rho, paste0(model_dir,"/mean_rho.RData"))
