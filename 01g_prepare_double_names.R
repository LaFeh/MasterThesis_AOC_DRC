# deal with double city names
library(dplyr)
library(data.table)
library(sf)

setwd("D:/DRC/gaussian_process_AOC")
towns =data.table::fread("./data/CD_villages/CD.txt",sep ="\t",header = FALSE)


colnames(towns) = c("geonameid","name","asciiname","alternatenames","latitude","longitude","feature_class",
                         "feature_code","country_code","cc2","admin1_code","admin2_code","admin3_code","admin4_code","population",
                         "elevation","dem","timezone","modification_date")

# only consider towns in the provinces ituri = 14, nordkivu = 13, sud kivu =12, as confusion with cities 
# in other provinces is not relevant as it is not even considered
towns <- towns[
  feature_class == "P"&admin1_code %in% c(12,11,17),
  .(
    geonameid,
    name,
    asciiname,
    alternatenames = na_if(alternatenames, ""),
    latitude,
    longitude,
    feature_class,
    feature_code,
    admin1_code,
    admin2_code,
    admin3_code
  ) 
][
  , all_names := fifelse(
    is.na(alternatenames), name,
    fifelse(is.na(name), alternatenames,
            paste(alternatenames, name, sep = ","))
  )
][,all_names := strsplit(all_names,",|\\|")]

towns[, all_names := lapply(all_names, unique)]

#only duplicated names can cause confusion:
list_of_all_names = unlist(towns$all_names)
list_of_duplicated_names = list_of_all_names[which(duplicated(list_of_all_names))]

duplicated_towns = towns[vapply(all_names, function(x)
  any(x %in% list_of_duplicated_names),
  logical(1)
),,]

duplicated_towns = duplicated_towns[, !c("alternatenames")]

## rwandan border:

rwa = read_sf("./data/rwa_adm_2006_nisr_wgs1984_20181002_shp",layer = "rwa_adm0_2006_NISR_WGS1984_20181002")
regions = st_read("./data/cod_admin_boundaries.shp",layer="cod_admin0")


rwa = st_transform(rwa, st_crs(regions))

touch_line <- st_intersection(rwa, regions) |> 
  st_boundary()
touch_line <- st_collection_extract(touch_line, "LINESTRING")
# plot(regions$geometry)
# plot(rwa,add =T)
# plot(touch_line,col ="red",add =T,type ="l")
duplicated_towns = st_as_sf(duplicated_towns,coords =c("longitude","latitude"), crs = 4326)

duplicated_towns$dist_to_rwa = st_distance(duplicated_towns,touch_line)


# create groups of countries that can be mixed up:
# each town has its own group e.g. 
# one town has names:Kalimba, Mangwa. It can be confused with all towns named
# Mangwa or kalimba.
# but a town named kalimba with only the alternate name : Kalimba can
# only confused with other towns named kalimba

towns_dt <- setDT(copy(duplicated_towns))
towns_dt[, id := .I]

# Unnest list column
long <- towns_dt[, .(name = unlist(all_names)), by = id]

# Self-join on shared names
matches <- long[long, on = "name", allow.cartesian = TRUE]


# Collect twins
list_twins_group <- matches[, .(twins = list(unique(i.id))), by = id]$twins

duplicated_towns$list_twins_group = list_twins_group

duplicated_towns$dist_to_rwa_group = sapply(duplicated_towns$list_twins_group,function(x){
  sum(duplicated_towns[x,]$dist_to_rwa)
})

duplicated_towns$dist_to_rwa_percent = duplicated_towns$dist_to_rwa/duplicated_towns$dist_to_rwa_group


saveRDS(duplicated_towns, "./data/duplicated_towns.rds")

#duplicated_towns = st_as_sf(duplicated_towns,coords =c("longitude","latitude"), crs = 4326)
#duplicated_towns$admin1_code = as.factor(duplicated_towns$admin1_code)
#plot(duplicated_towns[,"admin1_code"])
#plot(duplicated_towns[which(duplicated_towns$admin1_code %in% c(12,11,17)),"admin1_code"])


