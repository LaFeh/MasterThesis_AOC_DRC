setwd("D:/DRC/gaussian_process_AOC")

library(terra)
library(sf)
library(dplyr)
library(units)

# ============================================================
# 1. LOAD AND PROJECT STUDY AREA
# ============================================================

relevant_regions <- read_sf("./data/Congo_relevant_provinces.shp")
relevant_regions <- relevant_regions |>
  filter(name %in% c("Ituri", "Sud-Kivu", "Nord-Kivu")) |>
  distinct()

drc_m <- relevant_regions |>
  vect() |>
  project("EPSG:102022")

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
  filter(DESIG_ENG == "National Park") |>
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
  st_crop(st_bbox(grid)) |>
  filter(fclass !="drain" & fclass != "stream")

water3 <- st_read(gpkg_path, layer = "gis_osm_water_a_free") |>
  st_transform(st_crs(grid)) |>
  st_crop(st_bbox(grid))

# Remove waterways already contained within water polygons
contained_in_water3 <- unlist(st_contains(water3, water2))
if (length(contained_in_water3) > 0) water2 <- water2[-contained_in_water3, ]

# Buffer waterways by their width (minimum 1m)
water2$width_buf <- ifelse(is.na(water2$width) | water2$width == 0, 1, water2$width)
water2 <- st_buffer(water2, units::set_units(water2$width_buf, "m"))
water2$width     <- NULL
water2$width_buf <- NULL

# Remove water features fully contained within a single grid cell (negligible for masking)
# w2_contained <- unlist(st_contains(grid, water2, sparse = TRUE))
# w3_contained <- unlist(st_contains(grid, water3, sparse = TRUE))
# if (length(w2_contained) > 0) water2 <- water2[-w2_contained, ]
# if (length(w3_contained) > 0) water3 <- water3[-w3_contained, ]

# ============================================================
# 6. ERASE WATERWAYS (water2) FROM GRID — via terra for speed
# ============================================================

# Only process cells that actually touch water2
intersects2_idx  <- unique(unlist(st_intersects(water2, grid)))
grid2_touches    <- grid[intersects2_idx, ]
grid2_no_touch   <- grid[-intersects2_idx, ]

# Convert to terra, make valid, erase
grid2_touches_terra <- makeValid(vect(grid2_touches))
water2_terra        <- makeValid(vect(water2))
water2_terra        <- disagg(water2_terra)
water2_terra        <- aggregate(water2_terra, dissolve = TRUE)
water2_terra        <- makeValid(water2_terra)

grid_without_water2_terra <- erase(grid2_touches_terra, water2_terra)
grid_without_water2_terra <- disagg(grid_without_water2_terra)
# Convert back and recombine
grid_without_water2 <- st_as_sf(grid_without_water2_terra)
grid_without_water2 <- rbind(grid2_no_touch, grid_without_water2)
grid_without_water2 <- grid_without_water2[!st_is_empty(grid_without_water2), ]

rm(grid2_touches, grid2_no_touch, grid2_touches_terra,
   water2_terra, grid_without_water2_terra)

# ============================================================
# 7. ERASE WATER POLYGONS (water3) FROM GRID
# ============================================================

# Only process cells that touch water3
intersects3_idx  <- unique(unlist(st_intersects(water3, grid_without_water2)))
grid3_touches    <- grid_without_water2[intersects3_idx, ]
grid3_no_touch   <- grid_without_water2[-intersects3_idx, ]

grid3_touches_terra <- makeValid(vect(grid3_touches))
water3_terra        <- makeValid(vect(water3))
water3_terra        <- disagg(water3_terra)
water3_terra        <- aggregate(water3_terra, dissolve = TRUE)
water3_terra        <- makeValid(water3_terra)

grid_without_water3_terra <- erase(grid3_touches_terra, water3_terra)
grid_without_water3_terra <- disagg(grid_without_water3_terra)

grid_without_water <- st_as_sf(grid_without_water3_terra)
grid_without_water <- rbind(grid3_no_touch, grid_without_water)
grid_without_water <- grid_without_water[!st_is_empty(grid_without_water), ]

rm(grid3_touches, grid3_no_touch, grid3_touches_terra,
   water3_terra, grid_without_water3_terra, grid_without_water2)

# ============================================================
# 8. BUILD WATER SHAPE LAYER
# ============================================================

water_all <- rbind(
  water2 |> dplyr::select(geom) |> mutate(surface = "water"),
  water3 |> dplyr::select(geom) |> mutate(surface = "water")
) |>
  st_make_valid()

# Clip water to grid boundary
water_all_try     <- st_intersection(water_all, grid)

grid_boundary <- st_union(grid) |> st_make_valid()
water_all     <- st_intersection(water_all, grid_boundary) |>
  st_make_valid()
water_all     <- water_all[!st_is_empty(water_all), ]


rm(water2, water3)
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
water_all$surface = "water"

grid_final           <- rbind(grid_without_water, water_all)
grid_final           <- grid_final[!st_is_empty(grid_final), ]
grid_final$cell_id   <- seq_len(nrow(grid_final))
grid_final$name      <- grid_final$adm1_name
grid_final$adm1_name <- NULL

plot(grid_final[,"surface"])
# ============================================================
# 10. SAVE
# ============================================================

write_sf(grid_final, "./data/grid_surface.shp")
message("Done! Grid saved to ./data/grid_surface.shp")
message(paste("Total features:", nrow(grid_final)))
message(paste("Surface types:", paste(unique(grid_final$surface), collapse = ", ")))
