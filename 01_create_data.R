setwd("D:/DRC/gaussian_process_AOC")


library(terra)
library(sf)


### --

trimmed_lakes = read_sf("./data/Congo_relevant_lakes.shp")
relevant_regions_all = read_sf("./data/Congo_relevant_provinces.shp")
national_parks =read_sf("./data/WDPA_WDOECM_May2026_Public_COD_nationalparks/WDPA_WDOECM_May2026_Public_COD.gdb",layer ="WDPA_WDOECM_poly_May2026_COD")


####### interstection create whole picture:

relevant_regions = unique(relevant_regions_all[which(relevant_regions_all$name %in% c("Ituri","Sud-Kivu","Nord-Kivu")),])



leaflet() %>%
  addTiles() %>%  # OpenStreetMap
  addPolygons(
    data = st_transform(relevant_regions_all,4326),
    fillColor = "black",
    fillOpacity = 0.4,
    color = "black",
    weight = 1
  )

# relevant regions & lakes
mat_intersection = st_intersects(relevant_regions,trimmed_lakes,sparse = FALSE)

for (x in 1:nrow(mat_intersection)){
  lakes = st_combine(trimmed_lakes[which(mat_intersection[x,]),])
  relevant_regions[x,]$geometry =st_sfc(st_difference(relevant_regions[x,]$geometry,lakes))
}

rm(mat_intersection)


relevant_regions$surface = "land"

relevant_regions$iso =NULL

trimmed_lakes$surface = "water"
trimmed_lakes$iso = NULL

drc_with_lakes = rbind(trimmed_lakes,relevant_regions)
#plot(drc_with_lakes[,c("surface")])

############### create grid


library(terra)
library(sf)
library(dplyr)

# 1. Project to an Equal Area CRS (e.g., Africa Albers - EPSG:102022)
# This ensures that 5000m is actually 5km on the ground.
drc_vect <- vect(drc_with_lakes$geometry)
drc_proj <- drc_vect #project(drc_vect, "ESRI:102022")
rm(drc_vect)

drc_m <- project(drc_proj, "EPSG:102022")
rm(drc_proj)
# 2. Define the resolution (5km)
cell_size <- 5000 

# 3. Create the raster template and mask it in one go
# rasterize() is cleaner than mask() when starting from scratch
grid_template <- rast(ext(drc_m), resolution = cell_size, crs = crs(drc_m))
drc_raster <- rasterize(drc_m, grid_template, field = 1)

# 1. Convert the raster cells to polygons (SpatVector)
# na.rm = TRUE ensures we only get rows for cells inside the DRC
drc_cells_v <- as.polygons(drc_raster, aggregate = FALSE,touches = TRUE)


# 2. Convert the SpatVector to an sf object
drc_sf <- st_as_sf(drc_cells_v)

drc_sf$cell_id = 1:nrow(drc_sf)

#drc_with_lakes = st_transform(drc_with_lakes,st_crs(drc_sf))
#grid_wth_names = st_join(drc_sf,drc_with_lakes[,c("name","osm_id")], join = st_intersects,left =TRUE)
# intersects creates duplicates: remove them, province borders do not need to be exact
#grid_wth_names = grid_wth_names[!duplicated(grid_wth_names[,c("geometry")]),]

#rm(drc_sf)
#rm(drc_with_lakes)

admin = read_sf("./data/cod_admin_boundaries.shp",layer = "cod_admin2")[,c("adm2_name","adm2_pcode","adm1_name","adm1_pcode")]
admin = st_transform(admin, crs = st_crs(drc_sf))

grid_wth_names = st_join(drc_sf,admin, by =st_intersection, largest = T, left =T)

plot(grid_wth_names$geometry)

write_sf(grid_wth_names,"./data/grid_with_names.geojson")
##### add waterways


water = read_sf("./data/hotosm_cod_waterways_polygons_geojson/hotosm_cod_waterways_polygons_geojson.geojson")
#grid = read_sf("./data/grid_surface.shp")
# thsi is much faster than the other way
water = st_transform(water,st_crs(grid_wth_names))
# crop raster to grid bounding box
water_crop <- st_crop(
  water,
  vect(grid_wth_names),
  #filename = "./data/water_crop.tif",
  overwrite = TRUE
)

water_crop = water_crop[,c("name","natural","water","osm_type","osm_id")]

grid_contain_water = st_contains(grid_wth_names,water_crop,sparse =F)
water_is_contained = apply(grid_contain_water,2,function(x) {any(x)})
water_crop =water_crop[!water_is_contained,]

water_crop$surface = water_crop$natural
water_crop$natural = NULL
water_crop$osm_type = NULL
trimmed_lakes$water = "lake"

trimmed_lakes = st_transform(trimmed_lakes,st_crs(water_crop))
equal_lakes = st_equals(trimmed_lakes,water_crop,sparse =T)
k = 1
while(k <= length(equal_lakes)){
  if (!identical(equal_lakes[[k]],integer(0))){
    trimmed_lakes = trimmed_lakes[-k,]
  }
  k = k +1
}


water = st_union(rbind(water_crop,trimmed_lakes))


grid_without_water <- st_difference(grid_wth_names,water)
#rm(grid_wth_names)
#grid_cut <- st_difference(grid, river_poly)

grid_split <- grid_without_water |>
  st_cast("MULTIPOLYGON") |>
  st_cast("POLYGON")
gc()

grid_wth_names$surface = "land"

water_shape <- st_intersection(grid_wth_names, water)


colnames_not_in_grid =colnames(water_shape)[!colnames(water_shape)%in% colnames(grid_split)]
for(c in colnames_not_in_grid){
  grid_split[,c] =NA
}
colnames_not_in_water =colnames(grid_split)[!colnames(grid_split)%in% colnames(water_shape)]
for(c in colnames_not_in_water){
  water_shape[,c] =NA
}

water_shape$surface = "water"
grid_split$surface = "land"

grid = rbind(grid_split,water_shape)
grid$cell_id = 1:nrow(grid)
grid$name = grid$adm1_name 
grid$adm1_name  = NULL
write_sf(grid, "./data/grid_surface.shp")


#
#
# setwd("D:/DRC/gaussian_process_AOC")
# 
# 
# library(terra)
# library(sf)
# 
# 
# ### --
# 
# trimmed_lakes = read_sf("./data/Congo_relevant_lakes.shp")
# relevant_regions = read_sf("./data/Congo_relevant_provinces.shp")
# 
# 
# ####### interstection create whole picture:
# 
# relevant_regions = unique(relevant_regions[which(relevant_regions$name %in% c("Ituri","Sud-Kivu","Nord-Kivu")),])
# 
# mat_intersection = st_intersects(relevant_regions,trimmed_lakes,sparse = FALSE)
# 
# for (x in 1:nrow(mat_intersection)){
#   lakes = st_combine(trimmed_lakes[which(mat_intersection[x,]),])
#   relevant_regions[x,]$geometry =st_sfc(st_difference(relevant_regions[x,]$geometry,lakes))
# }
# 
# rm(mat_intersection)
# 
# relevant_regions$surface = "land"
# 
# trimmed_lakes$surface = "water"
# trimmed_lakes$iso = NA
# 
# drc_with_lakes = rbind(trimmed_lakes,relevant_regions)
# #plot(drc_with_lakes[,c("surface")])
# rm(trimmed_lakes)
# ############### create grid
# 
# 
# library(terra)
# library(sf)
# library(dplyr)
# 
# drc_vect <- vect(drc_with_lakes)
# # 1. Project to an Equal Area CRS (e.g., Africa Albers - EPSG:102022)
# # This ensures that 5000m is actually 5km on the ground.
# drc_vect <- vect(drc_with_lakes$geometry)
# drc_proj <- drc_vect #project(drc_vect, "ESRI:102022")
# rm(drc_vect)
# 
# drc_m <- project(drc_proj, "EPSG:102022")
# rm(drc_proj)
# # 2. Define the resolution (5km)
# cell_size <- 5000 
# 
# # 3. Create the raster template and mask it in one go
# # rasterize() is cleaner than mask() when starting from scratch
# grid_template <- rast(ext(drc_m), resolution = cell_size, crs = crs(drc_m))
# drc_raster <- rasterize(drc_m, grid_template, field = 1)
# rm(grid_template)
# rm(drc_m)
# 
# # 1. Convert the raster cells to polygons (SpatVector)
# # na.rm = TRUE ensures we only get rows for cells inside the DRC
# drc_cells_v <- as.polygons(drc_raster, aggregate = FALSE,touches = TRUE)
# 
# rm(drc_raster)
# # 2. Convert the SpatVector to an sf object
# drc_sf <- st_as_sf(drc_cells_v)
# 
# drc_sf$cell_id = 1:nrow(drc_sf)
# rm(drc_cells_v)
# 
# drc_with_lakes = st_transform(drc_with_lakes,st_crs(drc_sf))
# grid_wth_names = st_join(drc_sf,drc_with_lakes[,c("name","osm_id")], join = st_intersects,left =TRUE)
# # intersects creates duplicates: remove them, province borders do not need to be exact
# grid_wth_names = grid_wth_names[!duplicated(grid_wth_names[,c("geometry")]),]
# 
# rm(drc_sf)
# rm(drc_with_lakes)
# 
# admin = read_sf("./data/cod_admin_boundaries.shp",layer = "cod_admin2")[,c("adm2_name","adm2_pcode","adm1_name","adm1_pcode")]
# admin = st_transform(admin, crs = st_crs(grid_wth_names))
# 
# grid_wth_names = st_join(grid_wth_names,admin, by =st_intersection, largest = T, left =T)
# 
# gc()
# rm(admin)
# ##### add waterways
# 
# 
# water = read_sf("./data/hotosm_cod_waterways_polygons_geojson/hotosm_cod_waterways_polygons_geojson.geojson")
# #grid = read_sf("./data/grid_surface.shp")
# # thsi is much faster than the other way
# water = st_transform(water,st_crs(grid_wth_names))
# # crop raster to grid bounding box
# water_crop <- st_crop(
#   water,
#   vect(grid_wth_names),
#   #filename = "./data/water_crop.tif",
#   overwrite = TRUE
# )
# rm(water)
# 
# water_crop = water_crop[,c("name","natural","water","osm_type","osm_id")]
# 
# 
# 
# grid_contain_water = st_contains(grid_wth_names,water_crop,sparse =F)
# water_is_contained = apply(grid_contain_water,2,function(x) {any(x)})
# water_crop =water_crop[!water_is_contained,]
# rm(grid_contain_water)
# 
# grid_without_water <- st_difference(grid_wth_names,st_union(water_crop))
# #rm(grid_wth_names)
# #grid_cut <- st_difference(grid, river_poly)
# 
# grid_split <- grid_without_water |>
#   st_cast("MULTIPOLYGON") |>
#   st_cast("POLYGON")
# rm(grid_without_water)
# gc()
# 
# water_shape <- st_intersection(grid_wth_names, st_union(water_crop))
# #water_shape <- st_intersection(grid_split, st_union(water_crop))
# water_shape$osm_id = water_shape$osm_id.1
# water_shape$osm_id.1 =NULL
# water_shape$name.1 =NULL
# 
# rm(water_crop)
# rm(grid_wth_names)
# colnames_not_in_grid =colnames(water_shape)[!colnames(water_shape)%in% colnames(grid_split)]
# for(c in colnames_not_in_grid){
#   grid_split[,c] =NA
# }
# colnames_not_in_water =colnames(grid_split)[!colnames(grid_split)%in% colnames(water_shape)]
# for(c in colnames_not_in_water){
#   water_shape[,c] =NA
# }
# 
# grid = rbind(grid_split,water_shape)
# grid$cell_id = 1:nrow(grid)
# # 
# # 
# # which(st_geometry_type(grid_split)=="MULTILINESTRING")
# # 
# # grid = grid[which(st_geometry_type(grid)!="MULTILINESTRING"),]
# # 
# # plot(grid[which(st_geometry_type(grid)=="MULTILINESTRING"),])
# # plot(grid_split[c(9852),]$geometry)
# # 
# # grid$cell_id = 1:nrow(grid)
# # 
# # unique(st_geometry_type(grid))
# # grid[which(grid$osm_type=="MULTILINESTRING"),]
# #write_sf(water_shape, "./data/test.shp")
# write_sf(grid, "./data/grid_surface.shp")
# # 
# # read_sf("./data/grid_surface.shp")
# 
