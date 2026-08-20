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

max_width = 8

# ============================================================
# Prepare data
# ============================================================

gridname <- create_grid_name(
  add_streets = T,
  add_nationalparks = T,
  add_waterways = T,
  cell_size = 3000
)

model_dirs <- paste0(
  "~/MasterThesis_AOC_DRC/05_model/model_",
  names(parameter_grid),
  "_",
  gsub(".shp", "", gridname)
)


gridname2 <- create_grid_name(
  add_streets = F,
  add_nationalparks = T,
  add_waterways = T,
  cell_size = 3000
)

model_dirs2 <- paste0(
  "~/MasterThesis_AOC_DRC/05_model/model_",
  names(parameter_grid),
  "_",
  gsub(".shp", "", gridname2)
)


# Combine both sets of model directories
model_dirs <- c(model_dirs, model_dirs2)


date <- "202411"


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
  paste0(
    date,
    "_model_comparison_",
    gridname,
    ".pdf"
  ),
  width = 8.5,
  height = 11
)


for (model_dir in model_dirs) {
  
  # ----------------------------------------------------------
  # New page for each model
  # ----------------------------------------------------------
  
  grid.newpage()
  
  
  # ----------------------------------------------------------
  # Model title
  # ----------------------------------------------------------
  
  grid.text(
    basename(model_dir),
    x = 0.5,
    y = 0.97,
    gp = gpar(
      fontsize = 18,
      fontface = "bold"
    )
  )
  
  
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
  
  
  # ----------------------------------------------------------
  # LORO-CV summary
  # ----------------------------------------------------------
  
  RUN_LORO_CV <- F
  
  
  if (RUN_LORO_CV) {
    
    cv_file <- paste0(
      model_dir,
      "/",
      as.character("202405"),
      "_loro_cv.RData"
    )
    
    
    if (file.exists(cv_file)) {
      
      load(cv_file)
      
      
      grid_cv_summary[[model_dir]] <- data.frame(
        model_name = model_dir,
        n_folds = cv_summary$n_folds,
        n_converged = cv_summary$n_converged,
        elpd = cv_summary$elpd,
        mean_log_score = cv_summary$mean_log_score,
        brier = cv_summary$brier,
        auc = ifelse(
          is.null(cv_summary$auc),
          NA,
          cv_summary$auc
        )
      )
      
      
      grid.text(
        paste(
          capture.output(
            print(
              grid_cv_summary[[model_dir]]
            )
          ),
          collapse = "\n"
        ),
        x = 0.5,
        y = 0.2,
        just = "center",
        gp = gpar(
          fontfamily = "mono",
          fontsize = 5
        )
      )
      
      
    } else {
      
      warning(
        paste(
          "No LORO-CV file found for",
          model_dir
        )
      )
    }
  }
}


# ============================================================
# Close PDF
# ============================================================

dev.off()


# ============================================================
# Read CV summaries and overlapping area
# ============================================================

grid_cv_summary <- list()


for (model_dir in model_dirs) {
  
  # ----------------------------------------------------------
  # LORO-CV
  # ----------------------------------------------------------
  
  cv_file <- paste0(
    model_dir,
    "/",
    as.character("202405"),
    "_loro_cv.RData"
  )
  
  
  if (file.exists(cv_file)) {
    
    print(model_dir)
    
    load(cv_file)
    
    
    grid_cv_summary[[model_dir]] <- data.frame(
      model_name = model_dir,
      n_folds = cv_summary$n_folds,
      n_converged = cv_summary$n_converged,
      elpd = cv_summary$elpd,
      mean_log_score = cv_summary$mean_log_score,
      brier = cv_summary$brier,
      auc = ifelse(
        is.null(cv_summary$auc),
        NA,
        cv_summary$auc
      )
    )
  }
  
  
  # ----------------------------------------------------------
  # Overlapping area
  # ----------------------------------------------------------
  
  area_file <- paste0(
    model_dir,
    "/overlapping_area_",
    as.character("202405"),
    ".rds"
  )
  
  
  if (file.exists(area_file)) {
    
    print(model_dir)
    
    area <- readRDS(area_file)
    
    
    # Only add overlapping_area if this model
    # already has a CV summary
    if (!is.null(grid_cv_summary[[model_dir]])) {
      
      grid_cv_summary[[model_dir]]$overlapping_area <-
        as.numeric(area)
    }
  }
}


# ============================================================
# Combine summaries
# ============================================================

grid_cv_summary_df <- dplyr::bind_rows(
  grid_cv_summary
)


# ============================================================
# Save Excel summary
# ============================================================

openxlsx::write.xlsx(
  grid_cv_summary_df,
  paste0(
    "./",
    date,
    "_model_summary.xlsx"
  )
)


