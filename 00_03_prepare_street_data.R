# ============================================================
# Preprocessing: OSM street segments -> uniform-length segments
#                -> routable network -> segment midpoints
# Goal: prepare inputs for a kernel-based accessibility measure
#       computed along the network, per segment.
# ============================================================

create_street_splitted <- function(){
  
  
  library(sf)
  library(stplanr)
  #library(sfnetworks)
  library(dplyr)
  library(smoothr)
  
  library(nngeo)
  
  # ---- 1. Load data --------------------------------------------------
  streets = read_sf("./data/hotosm_cod_roads_osm_gpkg/roads.gpkg")%>%
    filter((adm1_name %in% c("Nord-Kivu","Sud-Kivu","Ituri"))&
             highway %in% c("trunk","primary","secondary","tertiary","primary_link","seondary_link","tertiary_link"))%>%
    mutate(geom_type = st_geometry_type(geom),
           lanes = as.numeric(lanes)) %>%
    filter(geom_type %in%c("LINESTRING","MULTILINESTRING"))
  
  # ---- 2. Reproject to a metric CRS -----------------------------------
  # Segment "length" only makes sense in a projected CRS.
  streets <- st_transform(streets, 32734)
  
  
  # ---- 3. add settlements, to remove streets that go through settlements -----------
  ## remove streets that go through settlements, there are too many and the street division of
  # settlements doesnt make som much sense
  
  settlements = st_read("./data/grid3_cod_settlement_extents_v4.gpkg")
  
  settlements = st_transform(settlements,st_crs(streets))
  mat_within = st_within(settlements,st_as_sfc(st_bbox(streets)),sparse =F)
  settlements_within = settlements[mat_within,]
  rm(settlements)
  
  # filter first otherwsie st_union takes too long
  
  # keep only big settlements
  settlements_to_be = settlements_within %>%st_drop_geometry()%>%
    group_by(mgrs_code) %>%
    filter(any(composite_class %in% c(
      "airport",
      "high buildings/hectare",
      "very high buildings/hectare",
      "large_buildings"))
    )%>% summarise(
      building_count_sum = sum(building_count, na.rm = TRUE)
    )%>%
    filter(building_count_sum > mean(building_count_sum))
  
  settlement_to_be_unioned = settlements_within %>%filter(mgrs_code %in% settlements_to_be$mgrs_code)
  settlement_to_be_unioned = settlement_to_be_unioned %>% st_simplify()


  settlements_union = st_union(settlement_to_be_unioned)
  
  settlements_union_wo_hole_terra = terra::fillHoles(terra::vect(settlements_union), inverse=FALSE)
  settlements_union_wo_hole = st_as_sf(settlements_union_wo_hole_terra)
  
  settlements_union_wo_hole = settlements_union_wo_hole |> st_simplify()|>
    st_transform(st_crs(streets)) |>
    st_make_valid() |>
    st_cast("POLYGON")
  
  
  streets_without_cities <- st_difference(streets, settlements_union)

  
  streets_without_cities2 <- streets_without_cities |>
    st_cast("MULTILINESTRING") |>
    st_cast("LINESTRING")
  
  # ---- 4. Split into uniform-length segments ---------------------------
  # Choose target_length based on your accessibility use case:
  #   ~25-50m  -> pedestrian-scale accessibility
  #   ~100-200m -> road/driving-scale accessibility
  target_length <- 10000  # metres
  
  streets_split <- line_segment(streets_without_cities2, 
                                segment_length = target_length)



  # Sanity check: distribution of segment lengths should now be tight
  seg_lengths <- as.numeric(st_length(streets_split))
  streets_split = streets_split[which(seg_lengths>= 30),]
  

  #summary(seg_lengths)
  #table(seg_lengths <30)
  
  # Give every segment a stable, unique ID -- you'll need this to join
  # accessibility values back later.
  streets_split <- streets_split %>%
    mutate(segment_id = row_number())

  streets_split$width_calculated = streets_split$width
  streets_split$width_calculated = ifelse(is.na(streets_split$width_calculated),streets_split$lanes *4,streets_split$width_calculated)
  streets_split$width_calculated = 30 # with time travel coherent
  streets_split = sf::st_buffer(streets_split,streets_split$width_calculated/2,singleSide =F,endCapStyle = "FLAT")
  streets_split$geom_type = NULL
  
  streets_split = st_difference(streets_split)


  

  streets_split2 = streets_split%>% 
    st_collection_extract(.,"POLYGON")%>%
    sf::st_cast(.,"MULTIPOLYGON")%>%
    sf::st_cast(.,"POLYGON")

  
  street_split_noholes <- st_remove_holes(streets_split2)
  
  # tif_path <-'./data/GRID3_COD_mix_travel_time_friction_surface_v1/GRID3_COD_mix_travel_time_friction_surface_v1.tif' 
  # mix_time=terra::rast(tif_path) 
  # streets_split_plot = st_transform(streets_split,st_crs(mix_time))%>%filter(adm1_name =="Nord-Kivu")
  # crop_frame= st_bbox(streets_split_plot)
  # crop_frame["xmin"] = crop_frame["xmin"] +(crop_frame["xmax"] - crop_frame["xmin"])*0.9
  # crop_frame["ymin"] = crop_frame["ymin"] +(crop_frame["ymax"] - crop_frame["ymin"])*0.9

  
  # # crop raster to grid bounding box
  # mix_time_crop <- terra::crop(
  #   mix_time,
  #   sf::st_as_sfc(crop_frame),
  #   #terra::vect(streets_split_plot),
  #   filename = "./data/mix_crop_2.tif",
  #   overwrite = TRUE
  # )
  # mix_time_crop = terra::project(mix_time_crop,"EPSG:4326")
  # mix_time_crop = log(mix_time_crop)
  # streets_split_plot = st_transform(streets_split_plot,"EPSG:4326")
  #mix_time_crop = terra::aggregate(mix_time_crop,2,mean)

  # 
  # 
  # library(leaflet)
  # leaflet(options = leafletOptions(zoomControl = T)) %>%addTiles()%>%
  #   addPolygons(
  #     data = streets_split_plot,
  #     stroke =T,
  #     fill =F
  #   )%>%
  #   addRasterImage(
  #     mix_time_crop,
  #     opacity = 0.8
  #   )
  # 
  # 
  # ---- 5. Save outputs ---------------------------------------------------
  sf::st_write(street_split_noholes, "./data/streets_split.gpkg", append = F)
  

  # streets_split.gpkg   -> uniform segments with segment_id (for joining results)
  # midpoints layer      -> one representative point per segment, ready to

  
}
