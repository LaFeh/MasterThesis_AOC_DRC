# 01 get osmdata

library(osmdata)

# 1. Load DRC country polygon



bb = getbb("Democratic Republic of the Congo, Nord-Kivu")
q <- opq("Democratic Republic of the Congo, Nord-Kivu", timeout = 200) |>
  add_osm_feature(key = "boundary", value = "administrative") |>
  add_osm_feature(key = "admin_level", value = "4") |>
  osmdata_sf()

bb = getbb("Democratic Republic of the Congo, Nord-Kivu",format_out ="sf_polygon")
trimmed_north = q %>% trim_osmdata(bb,exclude =F)
trimmed_north = trimmed_north$osm_multipolygons
bb = getbb("Democratic Republic of the Congo, South-Kivu",format_out ="sf_polygon")
trimmed_south = q %>% trim_osmdata(bb,exclude =F)
trimmed_south = trimmed_south$osm_multipolygons
trimmed = rbind(trimmed_south,trimmed_north)

trimmed$iso= sapply(trimmed[,c("ISO3166-2")],function(x) substr(x,1,2))[,c("ISO3166-2")]
trimmed = trimmed[,c("osm_id","name","iso")]
plot(trimmed$geometry)

relevant_regions = trimmed
write_sf(trimmed, "./data/Congo_relevant_provinces.shp")


lakes <- opq("Democratic Republic of the Congo, Nord-Kivu", timeout = 200) |>
  add_osm_feature(key = "natural", value = "water") |>
  add_osm_feature(key = "water", value = "lake") |>
  osmdata_sf()

trimmed_lakes = lakes %>% trim_osmdata(bb,exclude =F)
edouard_bb  = getbb("Democratic Republic of the Congo, Lac Edouard",format_out ="sf_polygon")
trimmed_lakes_2 = lakes %>% trim_osmdata(edouard_bb,exclude =F)
trimmed_lakes_2 = trimmed_lakes_2$osm_multipolygons
trimmed_lakes_2 = trimmed_lakes_2[,c("osm_id","name")]
plot(trimmed_lakes_2$geometry,col ="lightblue")

trimmed_lakes = trimmed_lakes$osm_multipolygons
trimmed_lakes = trimmed_lakes[,c("osm_id","name")]
plot(trimmed_lakes$geometry,col ="lightblue")

trimmed_lakes = rbind(trimmed_lakes,trimmed_lakes_2)

plot(trimmed$geometry)
plot(trimmed_lakes$geometry,col ="lightblue",add =T)

write_sf(trimmed_lakes, "./data/Congo_relevant_lakes.shp")