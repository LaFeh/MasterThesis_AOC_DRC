# distance to rwanda 


library(sf)
library(dplyr)


grid = read_sf(paste0("./data/",name_of_grid))

rwa = read_sf("./data/rwa_adm_2006_nisr_wgs1984_20181002_shp",layer = "rwa_adm0_2006_NISR_WGS1984_20181002")
rwa = st_transform(rwa, st_crs(grid))

#drc = st_union(grid)

# grid$intersection = st_intersects(grid,rwa,sparse = F)
# border_grid = grid[which(grid$intersection == T),]
# dist_mat = st_distance(grid,border_grid,by_element = FALSE)
# calculate distance over centroids, otherwise it takes too long
cat("calculate distance....")
boundary = st_boundary(st_union(rwa))
cent <- st_centroid(grid)

dist <- st_distance(
  cent,
  boundary,
  by_element = FALSE
)

#dist_mat = st_distance(grid,boundary,by_element = FALSE)
cat("distance is calculated....")

grid$min_dist_to_rwa = dist

dist_rwa = grid[,c("cell_id","min_dist_to_rwa")]
dist_rwa = st_drop_geometry(dist_rwa)

data.table::fwrite(dist_rwa,"./data/distance_rwanda.csv")
