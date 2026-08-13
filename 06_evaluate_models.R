# 05a run model

library(Matrix)
source("./00b_helper_create_grid_name.R")
source("./05_model/05a_a_helper_run_model.R")
source("./05_model/05a_b_helper_parameter_grid.R")
source("./04b_a_helper_functions.R")

## prepare data

library(grid)
library(png)

model_dirs <- paste0(
  "~/MasterThesis_AOC_DRC/05_model/model_",
  names(parameter_grid)
)

pdf("model_comparison.pdf", width = 8.5, height = 11)

for (model_dir in model_dirs) {
  
  # -------------------------
  # New page for each model
  # -------------------------
  
  grid.newpage()
  
  # -------------------------
  # Model title
  # -------------------------
  
  grid.text(
    basename(model_dir),
    x = 0.5,
    y = 0.97,
    gp = gpar(
      fontsize = 18,
      fontface = "bold"
    )
  )
  
  # -------------------------
  # Find both plots
  # -------------------------
  
  plot_files <- list.files(
    paste0(model_dir, "/plots"),
    pattern = "\\.(png|jpg|jpeg)$",
    full.names = TRUE
  )
  
  # Make sure plots are in a consistent order
  plot_files <- sort(plot_files)
  
  # Check that there are exactly 2 plots
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
  
  # -------------------------
  # Plot 1
  # -------------------------
  
  if (length(plot_files) >= 1) {
    
    img1 <- readPNG(plot_files[1])
    
    grid.raster(
      img1,
      x = 0.27,
      y = 0.68,
      width = 0.45,
      height = 0.38
    )
  }
  
  # -------------------------
  # Plot 2
  # -------------------------
  
  if (length(plot_files) >= 2) {
    
    img2 <- readPNG(plot_files[2])
    
    grid.raster(
      img2,
      x = 0.73,
      y = 0.68,
      width = 0.45,
      height = 0.38
    )
  }
  
  # -------------------------
  # settings.txt
  # -------------------------
  
  text_file <- file.path(
    model_dir,
    "settings.txt"
  )
  
  txt <- paste(
    readLines(text_file),
    collapse = "\n"
  )
  
  grid.text(
    txt,
    x = 0.05,
    y = 0.43,
    just = c("left", "top"),
    gp = gpar(fontsize = 8)
  )
}

dev.off()


