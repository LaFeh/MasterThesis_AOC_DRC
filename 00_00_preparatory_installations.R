# 1.1.1. made some changes that slow down the code considerably.
# remotes::install_version(
#   "sf",
#   version = "1.1-0",
#   repos = "https://cloud.r-project.org"
# )
if (packageVersion("sf") != "1.1.0"){
  stop("Please use packageversion 1.1.0 for sf")
}

# 1.9-1 supports different Coordinate systems EPSG:102022 doesnot work like this anymore
# remotes::install_version(
#   "terra",
#   version = "1.9-1",
#   repos = "https://cloud.r-project.org"
# )

if(packageVersion("terra") != "1.9.1"){
  stop("Please use package version 1.9.1 for terra")
}