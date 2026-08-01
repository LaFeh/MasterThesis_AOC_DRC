setwd(here::here())

library(terra)
library(sf)
library(dplyr)
library(units)
library(smoothr)

source("./01_xb_remove_from_grid.R")

add_streets = F
if(add_streets){
  grid_name_street = "street"
}else {
  grid_name_street = ""
}

cell_size     <- 5000

name_of_grid = paste0("grid_surface_",cell_size,"_",grid_name_street,".shp")

# ============================================================
# 1. LOAD AND PROJECT STUDY AREA
# ============================================================

relevant_regions <- read_sf("./data/congo_relevant_provinces/")
relevant_regions <- relevant_regions |>
  filter(name %in% c("Ituri", "Sud-Kivu", "Nord-Kivu")) |>
  distinct()

relevant_regions = st_transform(relevant_regions,sf::st_crs("ESRI:102022"))
drc_m <- relevant_regions |>
  vect()# |>
  #project("EPSG:102022")


# ============================================================
# 2. CREATE 5KM GRID
# ============================================================

cell_size     <- 5000
grid_template <- rast(ext(drc_m), resolution = cell_size, crs = crs(drc_m))
drc_raster    <- rasterize(drc_m, grid_template, field = 1)
drc_cells_v   <- as.polygons(drc_raster, aggregate = FALSE, touches = TRUE)

grid <- st_as_sf(drc_cells_v)
grid$cell_id <- seq_len(nrow(grid))

# ============================================================
# 3. ADD ADMIN NAMES
# ============================================================

admin <- read_sf("./data/cod_admin_boundaries.shp", layer = "cod_admin2") |>
  dplyr::select(adm2_name, adm2_pcode, adm1_name, adm1_pcode) |>
  st_transform(st_crs(grid))

grid <- st_join(grid, admin, join = st_intersects, largest = TRUE, left = TRUE)

# ============================================================
# 4. ADD NATIONAL PARKS
# ============================================================

national_parks <- read_sf(
  "./data/WDPA_WDOECM_May2026_Public_COD_nationalparks/WDPA_WDOECM_May2026_Public_COD.gdb",
  layer = "WDPA_WDOECM_poly_May2026_COD"
) |>
  dplyr::select(DESIG_ENG) |>
  filter(DESIG_ENG %in% c("National Park","Wetland of International Importance (Ramsar Site)","Nature reserve")) |>
  st_transform(st_crs(grid)) |>
  st_crop(st_bbox(grid)) |>
  st_make_valid() |>
  st_buffer(0)

grid <- grid |>
  st_make_valid() |>
  st_buffer(0)

# Intersection: grid cells that overlap parks
national_parks_shape <- st_intersection(grid, national_parks) |>
  st_buffer(0)

# Keep only meaningful pieces
national_parks_shape <- national_parks_shape[
  as.numeric(st_area(national_parks_shape)) > 1, 
]

# Remove park areas from grid cells
grid_without_parks <- st_difference(grid, st_union(national_parks)) |>
  st_buffer(0)

# Remove tiny slivers
grid_without_parks <- grid_without_parks[
  as.numeric(st_area(grid_without_parks)) > 1,
]

# Label surfaces and combine
grid_without_parks$surface  <- "land"
national_parks_shape$surface <- national_parks_shape$DESIG_ENG
national_parks_shape$DESIG_ENG <- NULL

# Align columns before rbind
align_cols <- function(a, b) {
  for (col in setdiff(names(b), names(a))) a[[col]] <- NA
  for (col in setdiff(names(a), names(b))) b[[col]] <- NA
  list(a[, names(a)], b[, names(a)])
}
res                   <- align_cols(grid_without_parks, national_parks_shape)
grid_without_parks    <- res[[1]]
national_parks_shape  <- res[[2]]



grid <- rbind(grid_without_parks, national_parks_shape)
grid <- grid[!st_is_empty(grid), ]

rm(grid_without_parks, national_parks_shape, national_parks)



# ============================================================
# 5. LOAD AND PREPARE WATER LAYERS
# ============================================================

gpkg_path <- "./data/congo-democratic-republic-260530-free.gpkg/congo-democratic-republic.gpkg"

water2 <- st_read(gpkg_path, layer = "gis_osm_waterways_free") |>
  st_transform(st_crs(grid)) |>
  st_crop(st_bbox(grid)) |> # only pick bigger rivers better, modelling throught the whole area
  filter(fclass !="drain" & fclass != "stream")

water3 <- st_read(gpkg_path, layer = "gis_osm_water_a_free") |>
  st_transform(st_crs(grid)) |>  st_crop(st_bbox(grid))|>
  st_cast("MULTIPOLYGON") |>
  st_cast("POLYGON")

# a lot of very small areas here, kick them out dependent on resolution of grid
# remove anything that is smaller than half the grid size grid cell is cell_size^2
half_grid_size = cell_size^2/2
water3$area = drop_units(st_area(water3))
water3 = water3[which(water3$area>half_grid_size),]

#water2$id_w2 =1:nrow(water2)
#water3$id_w3 =1:nrow(water3)

# Start with df1
water_result <- water3

# Remove any overlap from df2
df2_no_overlap <- st_difference(water2, st_union(water3))

# Remove empty geometries
df2_no_overlap <- df2_no_overlap[!st_is_empty(df2_no_overlap), ]
df2_no_overlap = st_buffer(df2_no_overlap, units::set_units(1, "m"))
# Combine
water_all <- rbind(water_result[,c("osm_id","code","fclass","name")], df2_no_overlap[,c("osm_id","code","fclass","name")])
#plot(water_all[,"fclass"])

# Remove waterways already contained within water polygons
# contained_in_water3 <- unlist(st_contains(st_union(water3), water2))
# if (length(contained_in_water3) > 0) water2 <- water2[-contained_in_water3, ]
# contained_in_water2 <- unlist(st_contains(st_union(water2),water3))
# if (length(contained_in_water2) > 0) water3 <- water3[-contained_in_water2, ]

# Buffer waterways by their width (minimum 1m)
# water2$width_buf <- ifelse(is.na(water2$width) | water2$width == 0, 1, water2$width)
# water2 <- st_buffer(water2, units::set_units(water2$width_buf, "m"))
# water2$width     <- NULL
# water2$width_buf <- NULL

# Remove water features fully contained within a single grid cell (negligible for masking)
# w2_contained <- unlist(st_contains(grid, water2, sparse = TRUE))
# w3_contained <- unlist(st_contains(grid, water3, sparse = TRUE))
# if (length(w2_contained) > 0) water2 <- water2[-w2_contained, ]
# if (length(w3_contained) > 0) water3 <- water3[-w3_contained, ]

# ============================================================
# 6. ERASE WATERWAYS (water2) FROM GRID — via terra for speed
# ============================================================


grid_without_water = remove_from_grid(grid,water_all)

# intersects2_idx  <- unique(unlist(st_intersects(water_all, grid)))
# grid2_touches    <- grid[intersects2_idx, ]
# grid2_no_touch   <- grid[-intersects2_idx, ]
# 
# # Convert to terra, make valid, erase
# grid2_touches_terra <- makeValid(vect(grid2_touches))
# water_terra        <- makeValid(vect(water_all))
# water_terra        <- disagg(water_terra)
# water_terra        <- aggregate(water_terra, dissolve = TRUE)
# water_terra        <- makeValid(water_terra)
# 
# grid_without_water_terra <- erase(grid2_touches_terra, water_terra)
# grid_without_water_terra <- disagg(grid_without_water_terra)
# # Convert back and recombine
# grid_without_water <- st_as_sf(grid_without_water_terra)
# grid_without_water <- rbind(grid2_no_touch, grid_without_water)
# grid_without_water <- grid_without_water[!st_is_empty(grid_without_water), ]
# 
# rm(grid2_touches, grid2_no_touch, grid2_touches_terra,
#    water_terra, grid_without_water_terra)


# Only process cells that actually touch water2
# intersects2_idx  <- unique(unlist(st_intersects(water2, grid)))
# grid2_touches    <- grid[intersects2_idx, ]
# grid2_no_touch   <- grid[-intersects2_idx, ]
# 
# # Convert to terra, make valid, erase
# grid2_touches_terra <- makeValid(vect(grid2_touches))
# water2_terra        <- makeValid(vect(water2))
# water2_terra        <- disagg(water2_terra)
# water2_terra        <- aggregate(water2_terra, dissolve = TRUE)
# water2_terra        <- makeValid(water2_terra)
# 
# grid_without_water2_terra <- erase(grid2_touches_terra, water2_terra)
# grid_without_water2_terra <- disagg(grid_without_water2_terra)
# # Convert back and recombine
# grid_without_water2 <- st_as_sf(grid_without_water2_terra)
# grid_without_water2 <- rbind(grid2_no_touch, grid_without_water2)
# grid_without_water2 <- grid_without_water2[!st_is_empty(grid_without_water2), ]
# 
# rm(grid2_touches, grid2_no_touch, grid2_touches_terra,
#    water2_terra, grid_without_water2_terra)

# ============================================================
# 7. ERASE WATER POLYGONS (water3) FROM GRID
# ============================================================

# Only process cells that touch water3
# intersects3_idx  <- unique(unlist(st_intersects(water3, grid_without_water2)))
# grid3_touches    <- grid_without_water2[intersects3_idx, ]
# grid3_no_touch   <- grid_without_water2[-intersects3_idx, ]
# 
# grid3_touches_terra <- makeValid(vect(grid3_touches))
# water3_terra        <- makeValid(vect(water3))
# water3_terra        <- disagg(water3_terra)
# water3_terra        <- aggregate(water3_terra, dissolve = TRUE)
# water3_terra        <- makeValid(water3_terra)
# 
# grid_without_water3_terra <- erase(grid3_touches_terra, water3_terra)
# grid_without_water3_terra <- disagg(grid_without_water3_terra)
# 
# grid_without_water <- st_as_sf(grid_without_water3_terra)
# grid_without_water <- rbind(grid3_no_touch, grid_without_water)
# grid_without_water <- grid_without_water[!st_is_empty(grid_without_water), ]
# 
# rm(grid3_touches, grid3_no_touch, grid3_touches_terra,
#    water3_terra, grid_without_water3_terra, grid_without_water2)

# ============================================================
# 8. BUILD WATER SHAPE LAYER
# ============================================================

# water_all <- rbind(
#   water2 |> dplyr::select(geom) |> mutate(surface = "water"),
#   water3 |> dplyr::select(geom) |> mutate(surface = "water")
# ) |>
#   st_make_valid()

water_all$surface = "water"

# Clip water to grid boundary
#water_all_try     <- st_intersection(water_all, grid)

grid_boundary <- st_union(grid) |> st_make_valid()
water_all     <- st_intersection(water_all, grid_boundary) |>
  st_make_valid()
water_all     <- water_all[!st_is_empty(water_all), ]


#rm(water2, water3)
water_all = water_all%>%rename(geometry = geom)
# ============================================================
# 9. COMBINE LAND + WATER INTO FINAL GRID
# ============================================================

grid_without_water$surface <- "land"


# Align columns
align_cols2 <- function(a, b) {
  for (col in setdiff(names(b), names(a))) a[[col]] <- NA
  for (col in setdiff(names(a), names(b))) b[[col]] <- NA
  list(a[, names(a)], b[, names(a)])
}
res2                 <- align_cols2(grid_without_water, water_all)
grid_without_water   <- res2[[1]]
water_all            <- res2[[2]]
water_self_contained = st_contains(water_all,remove_self =T)
list_water_contains = unlist(lapply(water_self_contained, function(x){if(rlang::is_empty(x)){FALSE}else{TRUE}}))
if (any(list_water_contains)){
  water_all = water_all[-unlist(water_self_contained[list_water_contains]),]
}
water_all$surface = "water"

grid_final           <- rbind(grid_without_water, water_all)
grid_final           <- grid_final[!st_is_empty(grid_final), ]
grid_final$name      <- grid_final$adm1_name
grid_final$adm1_name <- NULL

grid_final_clean <- grid_final %>%
  st_cast("MULTIPOLYGON") %>%
  st_cast("POLYGON")


if (add_streets){
  
 
  if(!file.exists("./data/streets_split.gpkg")){
    source("./01_xa_prepare_street_data.R")
    create_street_splitted()
  }
  
  streets = read_sf("./data/streets_split.gpkg")
  streets_transformed = st_transform(streets,st_crs(grid_final_clean))
  streets_transformed_valid1=st_make_valid(streets_transformed)
  line_string_idx = which(st_geometry_type(streets_transformed_valid1)=="LINESTRING")

  
  sf::st_crs(streets_transformed)$units
  buffered_valids = streets_transformed[line_string_idx,]%>%
    st_buffer(0.001) %>%   # tiny buffer, adjust to your CRS units
    st_make_valid()
  
  streets_transformed = streets_transformed[-line_string_idx,]
  streets_transformed = rbind(streets_transformed,buffered_valids)
  streets_transformed = streets_transformed %>%
    st_cast("MULTIPOLYGON")%>%
    st_cast("POLYGON")%>%st_make_valid()

  grid_without_streets = remove_from_grid(grid_final_clean,streets_transformed)
  
  streets_transformed$osm_id = streets_transformed$id
  streets_transformed$fclass = streets_transformed$highway
  streets_transformed$name = streets_transformed$adm1_name
  
  streets_transformed$surface ="road"
  streets_transformed$code =NA
  streets_transformed$layer =4
  streets_transformed$cell_id = streets_transformed$segment_id
  st_geometry(streets_transformed) <- "geometry"
  
  strt = streets_transformed[,c("osm_id","adm1_pcode","name","adm2_pcode","adm2_name",
             "fclass","code","layer","cell_id","surface")]

  grid_final_clean = rbind(grid_without_streets,strt)
  
  grid_final_clean = grid_final_clean %>%st_cast(.,"MULTIPOLYGON")%>%st_cast(.,"POLYGON")
  
  
  }


#area = st_area(grid_final_clean)
#table(st_area(grid_final_clean)<set_units(900,"m^2"))
#summary(st_area(grid_without_streets))

grid_final = smoothr::drop_crumbs(grid_final_clean,threshold = units::set_units((15*15),"m^2"))

grid_final_cast = st_difference(grid_final)
grid_final_cast$cell_id   <- seq_len(nrow(grid_final_cast))
#grid_final = st_set_precision(grid_final,1)

# grid_final_snapped <- grid_final %>%
#   st_make_valid() %>%
#   lwgeom::st_snap_to_grid(size = 1) %>%   # adjust size to your CRS units (e.g. 1cm if in meters)
#   st_make_valid()                             # re-validate after snapping, snapping can create new invalidities

#plot(grid_final[,"surface"])
# ============================================================
# 10. SAVE
# ============================================================
# overlap_final = st_overlaps(grid_final,grid_final)
# distance_final = st_distance(grid_final[bol_overlap,],grid_final[bol_overlap,])
# 
# bol_overlap = unlist(lapply(overlap_final, function(x){if(length(x)>0){TRUE}else{FALSE}}))
# which(!bol_overlap)
# overlap_final[bol_overlap][1]
# plot(grid_final[c(which(bol_overlap)[2],overlap_final[bol_overlap][2][[1]]),"cell_id"])
sf::st_crs(grid_final_cast)$units
data_adj = spdep::poly2nb(grid_final,queen =T,snap = 10)
# data_adj_s10 = spdep::poly2nb(grid_final,queen =T,snap = 10)

# bol_nghbr = unlist(lapply(data_adj, function(x){if(x[[1]]!=0){TRUE}else{FALSE}}))
# which(!bol_nghbr)
# plot(grid_final[which(!bol_nghbr)[1],"surface"])
# bbox =st_as_sfc(st_bbox(grid_final[which(!bol_nghbr)[2],]))
# bbox = st_buffer(bbox,5000)
# gf = st_intersects(grid_final,bbox)
# bol_gf = unlist(lapply(gf, function(x){if(length(x)!=0){TRUE}else{FALSE}}))


graph = F
if(graph){
  
  library(leaflet)
  library(sf)
  library(dplyr)
  
  # make sure everything is in WGS84 for leaflet
  grid_final_ll <- st_transform(grid_final, 4326)
  
  # indices of problem features
  problem_idx <- which(!bol_nghbr)
  
  # base map
  m <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron)
  
  # loop through each problem feature, add its context (bol_gf) + itself highlighted
  for (i in seq_along(problem_idx)) {
    
    idx <- problem_idx[i]
    
    # bbox + buffer around this specific problem feature
    bbox <- st_as_sfc(st_bbox(grid_final[idx, ])) %>%
      st_buffer(5000)
    
    # find neighboring context polygons
    gf <- st_intersects(grid_final, bbox)
    bol_gf <- lengths(gf) != 0
    
    # transform this feature's context + itself to WGS84
    context_layer <- grid_final_ll[bol_gf, ]
    highlight_layer <- grid_final_ll[idx, ]
    
    group_name <- paste0("Problem #", i, " (row ", idx, ")")
    
    m <- m %>%
      addPolygons(data = context_layer,
                  fillColor = "lightblue",
                  color = "black",
                  weight = 1,
                  fillOpacity = 0.4,
                  group = group_name) %>%
      addPolygons(data = highlight_layer,
                  fillColor = "red",
                  color = "red",
                  weight = 2,
                  fillOpacity = 0.8,
                  group = group_name,
                  popup = paste("Row:", idx))
  }
  
  # layer control to toggle each problem feature's context on/off
  m <- m %>%
    addLayersControl(
      overlayGroups = paste0("Problem #", seq_along(problem_idx), " (row ", problem_idx, ")"),
      options = layersControlOptions(collapsed = FALSE)
    )
  
  m
}
# 
# # 
# plot(grid_final[which(!bol_nghbr)[1],]$geometry, col ="red", add =T)
# plot(grid_final[which(bol_gf),]$geometry)
# st_intersects(grid_final[which(!bol_nghbr)[1],],grid_final[bol_gf,])
# st_contains(grid_final[which(bol_gf),],grid_final[which(!bol_nghbr)[1],])
# # #st_area(grid_final[which(!bol_nghbr),])
# st_distance(grid_final[which(bol_gf),],grid_final[which(!bol_nghbr)[1],])
# # k = st_distance(grid_final[which(!bol_nghbr),],grid_final)
# # dist_k_1 = k[1,]
# dist_k_1 = data.frame(dist = dist_k_1, id = 1:length(dist_k_1))
# dist_k_1 = dist_k_1[order(dist_k_1$dist),]
# 
# st_contains(grid_final[dist_k_1$id[c(1:3)],])
# 
# plot(grid_final[dist_k_1$id[1],"cell_id"],add =T)
# plot(grid_final[dist_k_1$id[c(1:3)],"cell_id"])
# plot(grid_final[dist_k_1$id[c(1,3)],"cell_id"],add = T)
# plot(grid_final[which(!bol_nghbr)[1],]$geometry,col ="red",add =T)

# st_equals(grid_final_clean[c(1721,28231),])
# 
# plot(grid_final[which(grid_final$name=="Nord-Kivu"),]$geometry)
# plot(grid_final[which(!bol_nghbr),]$geometry,col = "red",add = T)
# plot(grid_final[ which(is.na(grid_final$name)),]$geometry)
# plot(grid_final[ which(!bol_nghbr)[1],]$geometry)
# st_area(grid_final[ which(!bol_nghbr)[1],])
# my_polygons <- st_cast(grid_final[ which(!bol_nghbr)[1],]$geometry, "POLYGON")
# all_multipolygons <- grid_final[st_is(grid_final, "MULTIPOLYGON"), ]


write_sf(grid_final, paste0("./data/",name_of_grid))
message(paste0("Done! Grid saved to","./data/",name_of_grid))
message(paste("Total features:", nrow(grid_final)))
message(paste("Surface types:", paste(unique(grid_final$surface), collapse = ", ")))



