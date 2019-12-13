library(raster)
library(tidyverse)
library(sp)

bigdata_repo <- "GIS data repository"

# running script from here using ArcGIS pre-processed files
Ras_corine_UK <- find_onedrive(dir = bigdata_repo, path = "Created rasters/Corine-land-cover-100m-UKonly-WGS84-3.tif") %>% raster()
Ras_pH_UK <- find_onedrive(dir = bigdata_repo, path = "Created rasters/Soil-grids-5km-pH-UKonly.tif") %>% raster()

# read in UK shapefile (inc DA's)
Shp_UK <- find_onedrive(dir = bigdata_repo, path = "DA shapefile/GBR_adm_shp/GBR_adm1.shp") %>% shapefile()

# adjust corine raster to show 1 for pasture, 0 for not pasture, NA for missing data
Ras_pasture <- Ras_corine_UK
Ras_pasture[Ras_corine_UK == 231] <- 1
Ras_pasture[Ras_corine_UK != 231] <- 0
Ras_pasture[is.na(Ras_corine_UK)] <- NA

library(rasterVis)

levelplot(Ras_pasture) +
  layer(sp.polygons(Shp_UK, lwd=0.1))

# resample to ~10km grid squares. resample() function only calculates means — so with this coding
# this represents fraction of new cell size under pasture. Multiply by area to get absolute area (in km2)
Ras_pasture_ag <- resample(Ras_pasture, Ras_pH_UK) * area(Ras_pH_UK)

hist(Ras_pasture_ag)
plot(Ras_pasture_ag)

# total area checks
Ras_pasture_ag %>%
  as.data.frame(xy = T) %>%
  as_tibble() %>%
  pull(layer) %>%
  sum(na.rm = T)

# write out created rasters for future use
writeRaster(Ras_pasture_ag, find_onedrive(dir = bigdata_repo, path = "Created rasters/UK-pasture-area-10km-CLC-based-WGS84-2.tif"))
