#ooxx_starter_forserver

# Load native libraries in dependency order

dyn.load("/home/wucloud/local/lib/libudunits2.so.0")

dyn.load("/home/wucloud/local/lib/libgeos.so.3.14.0")
dyn.load("/home/wucloud/local/lib/libgeos_c.so")

dyn.load("/home/wucloud/local/lib/libproj.so")
dyn.load("/home/wucloud/local/lib/libgdal.so")

# Load R packages
library(units)
library(sf)
library(terra)
