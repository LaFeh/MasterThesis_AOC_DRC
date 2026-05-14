# settlement data
setwd("D:/DRC/gaussian_process_AOC")

library(sf)
library(dplyr)
rwa = read_sf("./data/rwa_adm_2006_nisr_wgs1984_20181002_shp",layer = "rwa_adm0_2006_NISR_WGS1984_20181002")



grid = read_sf("./data/grid_surface.shp")

rwa = st_transform(rwa, st_crs(grid))

#plot(st_boundary(grid))
drc = st_union(grid)
# 
# plot(c(drc,rwa$geometry))
# 
# 
# any(st_touches(grid,rwa,sparse = F))
# any(st_crosses(grid,rwa,sparse = F))
grid$intersection = st_intersects(grid,rwa,sparse = F)
border_grid = grid[which(grid$intersection == T),]
dist_mat = st_distance(grid,border_grid,by_element = FALSE)

grid$min_dist_to_rwa = apply(dist_mat,1,function(x) min(x))

dist_rwa = grid[,c("cell_id","min_dist_to_rwa")]
dist_rwa = st_drop_geometry(dist_rwa)

data.table::fwrite(dist_rwa,"./data/distance_rwanda.csv")
