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
  
  
  # pull the CV summary that run_model_wrapper() / run_model() just saved
  # (if requested) and stash it for the final cross-model table
  RUN_LORO_CV = T
  if (RUN_LORO_CV) {
    cv_file <- paste0(model_dir,"/",as.character("202405"),"_loro_cv.RData")
    if (file.exists(cv_file)) {
      load(cv_file)   # loads loro_df, cv_summary
      grid_cv_summary[[model_name]] <- data.frame(
        model_name      = model_name,
        n_folds         = cv_summary$n_folds,
        n_converged     = cv_summary$n_converged,
        elpd            = cv_summary$elpd,
        mean_log_score  = cv_summary$mean_log_score,
        brier           = cv_summary$brier,
        auc             = ifelse(is.null(cv_summary$auc), NA, cv_summary$auc)
      )
      
      grid.text(
        paste(capture.output(print(grid_cv_summary[[model_name]])), collapse = "\n"),
        x = 0.5,
        y = 0.2,
        just = "center",
        gp = gpar(fontfamily = "mono", fontsize = 5)
      )
      

    } else {
      warning(paste("No LORO-CV file found for", model_name))
    }
  }
}

dev.off()


for (model_dir in model_dirs) {
  
  cv_file <- paste0(model_dir,"/",as.character("202405"),"_loro_cv.RData")
  if (file.exists(cv_file)) {
    print(model_dir)
    load(cv_file)   # loads loro_df, cv_summary
    grid_cv_summary[[model_dir]] <- data.frame(
      model_name      = model_dir,
      n_folds         = cv_summary$n_folds,
      n_converged     = cv_summary$n_converged,
      elpd            = cv_summary$elpd,
      mean_log_score  = cv_summary$mean_log_score,
      brier           = cv_summary$brier,
      auc             = ifelse(is.null(cv_summary$auc), NA, cv_summary$auc)
    )

    
  
  }
  
  grid_cv_summary_df <- dplyr::bind_rows(grid_cv_summary)
  
  openxlsx::write.xlsx(grid_cv_summary_df,"./model_summary.xlsx")
 
}

