
# lalnd cover usage
#https://storage.googleapis.com/earthenginepartners-hansen/GLCLU2000-2020/v2/download.html
setwd("D:/DRC/gaussian_process_AOC")

library(sf)
library(dplyr)
library(terra)
library(exactextractr)

tif_path <-'./data/landcover_2020.tif'
landcover=rast(tif_path)


grid = read_sf("./data/grid_surface.shp")
grid = st_transform(grid,st_crs(landcover))

# crop raster to grid bounding box
landcover_crop <- crop(
  landcover,
  vect(grid)
)


summary <- exact_extract(landcover_crop, grid, 'mean')

grid$landcover_mean <- summary
grid$lg_landcover_mean <- log(summary)
grid = st_drop_geometry(grid)

result = grid[,c("cell_id","landcover_mean","lg_landcover_mean")]

data.table::fwrite(result,"./data/landcover.csv")

