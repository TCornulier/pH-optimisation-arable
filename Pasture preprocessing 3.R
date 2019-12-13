library(raster)
library(tidyverse)
library(sp)

bigdata_repo <- "GIS data repository"

Pasture_area <- find_onedrive(dir = bigdata_repo, path = "Created rasters/UK-pasture-area-10km-CLC-based-WGS84.tif") %>% raster()

# read in FAO slope and elevation
Elev <- find_onedrive(dir = bigdata_repo, path = "FAO elevation/GloElev5min.tif")

Slope_stack <- 