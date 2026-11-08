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


model_dirs = file.path("~/MasterThesis_AOC_DRC/05_model",paste0("model_",names(parameter_grid)))


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
df_overlap_uncontested = df_overlap

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
  
  overlap_files_uncontested = all_files[grepl("overlap_area_percentage_unconstested",all_files)]
  
  for ( file in overlap_files_uncontested){
    
    area = readRDS(file.path(model_dir,file))
    date = substring(file,1,6)
    
    
    if(length(area)==0){
      next
    }else{
      
      df_overlap_uncontested[which(df_overlap_uncontested$date == date & df_overlap_uncontested$dir ==model_dir),"overlap_area"] = area
      
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
df_overlap%>%group_by(dir,dir_num)%>%summarise(mn_overlap_area = mean(overlap_area,na.rm =T))%>%arrange(mn_overlap_area)

ggplot2::ggplot(df_overlap%>%filter(dir_num %in%c(4,6))) +
  ggplot2::geom_line(
    ggplot2::aes(
      x = date,
      y = overlap_area,
      group = dir_num,
      color = dir_num
    )
  )


ggplot2::ggplot(df_overlap_uncontested) +
  ggplot2::geom_line(
    ggplot2::aes(
      x = date,
      y = overlap_area,
      group = dir_num,
      color = dir_num
    )
  )

j = df_overlap_uncontested%>%group_by(dir,dir_num)%>%summarise(mn_overlap_area = mean(overlap_area,na.rm =T))%>%arrange(mn_overlap_area)

#df_overlap%>%filter(dir_num==4)%>%select(dir)
