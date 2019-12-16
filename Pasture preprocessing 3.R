library(raster)
library(tidyverse)
library(sp)

bigdata_repo <- "GIS data repository"

Pasture_area <- find_onedrive(dir = bigdata_repo, path = "Created rasters/UK-pasture-area-10km-CLC-based-WGS84.tif") %>% raster()

# read in slope at 30-arc-secs from GMTED2010
Elev <- find_onedrive(dir = bigdata_repo, path = "Created rasters/UK-dem-GMTED2010-30-arc-secs-wgs84-mean-elev-m.tif")

# FAO slope data at 5-arc-mins


Slope_stack <- find_onedrive(dir = bigdata_repo, path = "")