#06 evaluate civilians

# evaluate based on ipis map:

library(dplyr)
library(ggplot2)


source("./00b_helper_create_grid_name.R")
source("./05_model/05a_a_helper_run_model.R")
source("./05_model/05a_b_helper_parameter_grid.R")
source("./04b_a_helper_functions.R")

cov_list = create_covariates_list()



################


# Set directory paths

model_dir_name_civilian = "model_quality_TRUE_streets_first_degree_no_dist_lag_lead_rv_battles_civilians_estimaterho_grid_surface_3000_water_park_street"
model_dir_name_without = "model_quality_TRUE_streets_first_degree_no_dist_lag_lead_rv_battles_estimaterho_grid_surface_3000_water_park_street"

dir_civilians = file.path("~/MasterThesis_AOC_DRC/05_model/",model_dir_name_civilian,"plots")
dir_without = file.path("~/MasterThesis_AOC_DRC/05_model/",model_dir_name_without,"plots")
library(png)


# Directory for combined images
output_dir <- "~/MasterThesis_AOC_DRC/comparison_plots/civilian/"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Find matching PNG files
files_civ <- list.files(
  dir_civilians,
  pattern = "^\\d{6}_ipis_aoc\\.png$",
  full.names = TRUE
)

files_without <- list.files(
  dir_without,
  pattern = "^\\d{6}_ipis_aoc\\.png$",
  full.names = TRUE
)

# Extract dates
dates_civ <- sub("_ipis_aoc\\.png$", "", basename(files_civ))
dates_without <- sub("_ipis_aoc\\.png$", "", basename(files_without))

# Only dates available in both directories
common_dates <- intersect(dates_civ, dates_without)

# White gap between images, in pixels
gap <- 20
title_height <- 60  # Space reserved for titles, in pixels

for (date in common_dates) {
  
  
  img_civ <- png::readPNG(files_civ[match(date, dates_civ)])
  img_without <- png::readPNG(files_without[match(date, dates_without)])
  
  h1 <- dim(img_civ)[1]
  w1 <- dim(img_civ)[2]
  h2 <- dim(img_without)[1]
  w2 <- dim(img_without)[2]
  
  gap <- 20
  title_height <- 60
  
  canvas_w <- w1 + gap + w2
  canvas_h <- max(h1, h2) + title_height
  
  png(
    filename = file.path(output_dir, paste0(date, "_comparison.png")),
    width = canvas_w,
    height = canvas_h,
    res = 96
  )
  
  grid.newpage()
  
  # Overall layout: title row + image row, with separate left/right columns
  pushViewport(viewport(
    layout = grid.layout(
      nrow = 2, ncol = 2,
      heights = unit(c(title_height, max(h1, h2)), "null"),
      widths = unit(c(w1, w2 + gap), "null")
    )
  ))
  
  # Titles
  pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1))
  grid.text(paste(date, "with civilian"), gp = gpar(fontsize = 16, fontface = "bold"))
  popViewport()
  
  pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 2))
  grid.text(paste(date, "without civilian"), gp = gpar(fontsize = 16, fontface = "bold"))
  popViewport()
  
  # Images
  pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 1))
  grid.raster(as.raster(img_civ), interpolate = FALSE)
  popViewport()
  
  pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 2))
  grid.raster(as.raster(img_without), interpolate = FALSE)
  popViewport()
  
  popViewport()
  
  dev.off()
}

message("Combined ", length(common_dates), " dates.")
