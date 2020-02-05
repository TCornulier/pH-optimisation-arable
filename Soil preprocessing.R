
library(raster)
library(sp)
library(rasterVis)
library(tidyverse)

projdata_repo <- "DEFRA Clean Growth Project/pH Optimisation/Extension for publication"
bigdata_repo <- "GIS data repository"

# read in UK shapefile
Shp_UK <- find_onedrive(dir = projdata_repo, path = "GIS data/DA shapefile/GBR_adm_shp/GBR_adm1.shp") %>% shapefile()

# read in corine land cover raster at ~100m resolution
Ras_corine_UK <- find_onedrive(dir = bigdata_repo, path = "Created rasters/Corine-land-cover-100m-UKonly-WGS84-3.tif") %>% raster()

# adjust corine raster to show 1 for pasture, 0 for not pasture, NA for missing data  (i.e. water bodies/coastline)
Ras_pasture <- Ras_corine_UK
Ras_pasture[Ras_corine_UK == 231] <- 1
Ras_pasture[Ras_pasture > 1] <- 0
# Ras_pasture[is.na(Ras_corine_UK)] <- NA # no longer needed — method updated

# adjust corine raster to show 1 for crop, 0 for not crop, NA for missing data (i.e. water bodies/coastline)
Ras_crop <- Ras_corine_UK
Ras_crop[Ras_corine_UK == 211] <- 1
# Ras_crop[Ras_corine_UK == 212] <- 1 # not needed — no data
# Ras_crop[Ras_corine_UK == 241] <- 1 # not needed — no data
Ras_crop[Ras_crop > 3] <- 0
# Ras_crop[is.na(Ras_corine_UK)] <- NA # no longer needed — method updated

# read in soil data at ~250m resolution
# all to 30cm depth despite difference in filename codes
Soil_stack <- stack(find_onedrive(dir = bigdata_repo, path = "SoilGrids250/OCSTHA_M_30cm_250m_ll.tif"), # soil SOC stocks, tonnes / ha
                    find_onedrive(dir = bigdata_repo, path = "SoilGrids250/ORCDRC_M_sl4_250m_ll.tif"), # soil C fraction, g / kg
                    find_onedrive(dir = bigdata_repo, path = "SoilGrids250/PHIHOX_M_sl4_250m_ll.tif")) # pH, index * 10

# mask to UK only
Soil_stack <- Soil_stack %>% crop(Shp_UK)
Soil_stack <- Soil_stack %>% mask(Shp_UK)

# resample to 100m
# doing this separately since doing the whole stack seems to be too much for R
Ras_OC <- Soil_stack[[1]] %>% resample(Ras_pasture)
Ras_Cpc <- Soil_stack[[2]] %>% resample(Ras_pasture)
Ras_pH <- Soil_stack[[3]] %>% resample(Ras_pasture)

# as needed to prevent data loss risk — pretty computationally costly to get to this stage
# save.image(find_onedrive(dir = projdata_repo, path = "GIS data/Soil processing workspace.RData"))
# load(find_onedrive(dir = projdata_repo, path = "GIS data/Soil processing workspace.RData"))

# estimate of soil OM in g / kg
Ras_OMpc <- Ras_Cpc / 0.58 # OM = 58% C

# use OM raster to create 1-0 raster for peat
Ras_peat <- Ras_OMpc
Ras_peat[Ras_OMpc >= 200] <- 1
Ras_peat[Ras_OMpc < 200] <- 0
levelplot(Ras_peat)

# next filter out any pasture and cropland under peat
Ras_pasture_nopeat <- Ras_pasture
Ras_pasture_nopeat[Ras_peat == 1] <- 0

Ras_crop_nopeat <- Ras_crop
Ras_crop_nopeat[Ras_peat == 1] <- 0

# ~10km raster to use as resampling template
Ras_pH_UK <- find_onedrive(dir = bigdata_repo, path = "Created rasters/Soil-grids-5km-pH-UKonly.tif") %>% raster()

# resample to ~10km grid squares. resample() function only calculates means — so with this coding
# this represents fraction of new cell size under pasture. Multiply by area to get absolute area (in km2)
# Calculate fraction not under peat as function of 
Ras_pasture_ag <- resample(Ras_pasture, Ras_pH_UK) * area(Ras_pH_UK)
Ras_pasture_nopeat_ag <- resample(Ras_pasture_nopeat, Ras_pH_UK) * area(Ras_pH_UK)
Ras_pasture_peatfrac <- Ras_pasture_nopeat_ag / Ras_pasture_ag

Ras_crop_ag <- resample(Ras_crop, Ras_pH_UK) * area(Ras_pH_UK)
Ras_crop_nopeat_ag <- resample(Ras_crop_nopeat, Ras_pH_UK) * area(Ras_pH_UK)
Ras_crop_peatfrac <- Ras_crop_nopeat_ag / Ras_crop_ag

# write out peat fraction rasters
writeRaster(Ras_pasture_peatfrac, find_onedrive(dir = bigdata_repo, path = "Created rasters/UK-pasture-fraction-not-under-peat-10km-CLC-based-WGS84.tif"))
writeRaster(Ras_crop_peatfrac, find_onedrive(dir = bigdata_repo, path = "Created rasters/UK-crop-fraction-not-under-peat-10km-CLC-based-WGS84.tif"))

# next job is to calculate SOC stocks and pH under crops and pasture
# we'll use the rasters which exclude peat so as not to let this influence results
Ras_OC_pasture <- Ras_OC
Ras_OC_pasture[Ras_pasture_nopeat == 0] <- NA

Ras_OC_crop <- Ras_OC
Ras_OC_crop[Ras_crop_nopeat == 0] <- NA

Ras_pH_pasture <- Ras_pH
Ras_pH_pasture[Ras_pasture_nopeat == 0] <- NA

Ras_pH_crop <- Ras_pH
Ras_pH_crop[Ras_crop_nopeat == 0] <- NA

Ras_OC_pasture_ag <- resample(Ras_OC_pasture, Ras_pH_UK)
Ras_OC_crop_ag <- resample(Ras_OC_crop, Ras_pH_UK)
Ras_pH_pasture_ag <- resample(Ras_pH_pasture, Ras_pH_UK)
Ras_pH_crop_ag <- resample(Ras_pH_crop, Ras_pH_UK)

# checks
hist(Ras_pH_crop_ag)
hist(Ras_pH_pasture_ag)
hist(Ras_OC_crop_ag)
hist(Ras_OC_pasture_ag)

levelplot(Ras_pH_crop_ag)
levelplot(Ras_pH_pasture_ag)
levelplot(Ras_OC_crop_ag)
levelplot(Ras_OC_pasture_ag)

# write out created rasters
writeRaster(Ras_OC_pasture_ag, find_onedrive(dir = bigdata_repo, path = "Created rasters/UK-pasture-SOC-tonnes-ha-10km-CLC-SG250-WGS84.tif"))
writeRaster(Ras_OC_crop_ag, find_onedrive(dir = bigdata_repo, path = "Created rasters/UK-crop-SOC-tonnes-ha-10km-CLC-SG250-WGS84.tif"))
writeRaster(Ras_pH_pasture_ag, find_onedrive(dir = bigdata_repo, path = "Created rasters/UK-pasture-pH-10km-CLC-SG250-WGS84.tif"))
writeRaster(Ras_pH_crop_ag, find_onedrive(dir = bigdata_repo, path = "Created rasters/UK-crop-pH-10km-CLC-SG250-WGS84.tif"))

# save workspace
save.image(find_onedrive(dir = projdata_repo, path = "GIS data/Soil processing workspace.RData"))
