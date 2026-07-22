# settlement data
setwd(here::here())

library(sf)
library(dplyr)
library(exactextractr)
library(terra)

# load("./data/cleaned_gdf_territory.RData")
# crs_acled_territory = st_crs(cleaned_gdf_territory)
# rm(cleaned_gdf_territory)

grid = read_sf("./data/grid_surface.shp")
#grid = st_transform(grid,crs_acled_territory)

settlements = st_read("./data/GRID3_COD_settlement_grid_v3_1_gpkg/GRID3_COD_settlement_grid_v3_1.gpkg",
                      query = "SELECT building_count,building_area,grid3_id,longitude,latitude,Shape FROM \"GRID3_COD_settlement_grid_v3_1\"")

settlements = st_transform(settlements,grid)

mat_within = st_within(settlements,st_as_sfc(st_bbox(grid)),sparse =F)
settlements_within = settlements[mat_within,]
rm(settlements)

settlements_grid = st_join(settlements_within,grid[,c("cell_id")], st_within)
settlement_grid = settlements_grid%>%group_by(cell_id)%>%summarise(building_count = sum(building_count,na.rm = T),
                                                  building_area = sum(building_area,na.rm = T))

settlement_grid = st_drop_geometry(settlement_grid)


data.table::fwrite(settlement_grid,"./data/grid_settlements.csv")


# people

#load("./data/cleaned_gdf_territory.RData")
#crs_acled_territory = st_crs(cleaned_gdf_territory)
#rm(cleaned_gdf_territory)

grid = read_sf("./data/grid_surface.shp")
#grid = st_transform(grid,crs_acled_territory)


tif_path = "./data/COD_population_v4_4_gridded/COD_population_v4_4_gridded/COD_Population_v4_4_gridded.tif"
population=rast(tif_path)

population= st_transform(population,st_crs(grid))

# crop raster to grid bounding box
population_crop <- crop(
  population,
  vect(grid),
  filename = "./data/population_crop.tif",
  overwrite = TRUE)


summary <- exact_extract(population_crop, grid, 'mean')

grid$population_mean <- summary
grid$lg_population_mean <- log(summary)

result = grid[,c("cell_id","population_mean","lg_population_mean")]
result = st_drop_geometry(grid)

data.table::fwrite(result,"./data/population.csv")

