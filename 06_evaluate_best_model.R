#06 evaluate best model



library(grid)
library(dplyr)

source("./00b_helper_create_grid_name.R")
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




df_overlap = expand.grid(model_dirs,all_dates)
colnames(df_overlap) = c("dir","date")
df_overlap$overlap_area = NA

model_dirs_names = setNames(model_dirs,seq_along(model_dirs))

df_overlap$dir_num = names(model_dirs_names[df_overlap[,"dir"]])
for(model_dir in model_dirs){
  
  all_files = list.files(file.path(model_dir))
  
  overlap_files = all_files[grepl("overlap_area_percentage",all_files)]
  
  for ( file in overlap_files){

    area = readRDS(file.path(model_dir,file))
    date = substring(file,1,6)
  
    
    if(length(area)==0){
      next
    }else{
      
      df_overlap[which(df_overlap$date == date & df_overlap$dir ==model_dir),"overlap_area"] = area
      
    }
  }
  
}


ggplot2::ggplot(df_overlap) +
  ggplot2::geom_line(
    ggplot2::aes(
      x = date,
      y = overlap_area,
      group = dir_num,
      color = dir_num
    )
  )

