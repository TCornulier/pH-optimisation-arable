library(raster)
library(tidyverse)
library(sp)

bigdata_repo <- "GIS data repository"

Ras_corine <- find_onedrive(dir = bigdata_repo, path = "Corine raster 100m/CLC2018_CLC2018_V2018_20.tif") %>% raster()
#plot(Ras_corine)

# read in pH raster as template
Ras_pH <- find_onedrive(dir = bigdata_repo, path = "SoilGrids 5km/Soil pH/Fixed/PHIHOX_M_sl4_5km_ll.tif") %>% raster()

# manually crop Corine to extent of GB only (it's a bit clunky, but much faster than reprojecting all of Europe...)
ext <- extent(3.0*10^6, 3.9*10^6, 3.0*10^6, 4.4*10^6)
Ras_corine_UK <- Ras_corine %>% crop(ext)

# reproject corine raster to WGS84
newproj <- crs(Ras_pH)
Ras_corine_UK <- Ras_corine_UK %>% projectRaster(crs = newproj)

# checks
#Ras_corine_GB %>% plot()
#Ras_pH %>% plot(add = T)

# read in UK DA shapefiles and crop pH and corine rasters
Shp_UK <- find_onedrive(dir = bigdata_repo, path = "DA shapefile/GBR_adm_shp/GBR_adm1.shp") %>% shapefile()
Ras_pH <- Ras_pH %>% crop(Shp_UK) %>% mask(Shp_UK)
Ras_corine_UK <- Ras_corine_UK %>% crop(Shp_UK) %>% mask(Shp_UK)

# adjust corine raster to show 1 for pasture, 0 for not pasture, NA for missing data
Ras_pasture <- Ras_corine_UK
Ras_pasture[Ras_corine_UK == 231] <- 1
Ras_pasture[Ras_corine_UK != 231] <- 0
Ras_pasture[is.na(Ras_corine_UK)] <- NA

# resample to ~10km grid squares. resample() function only calculates means — so with this coding
# this represents fraction of new cell size under pasture. Multiply by area to get absolute area (in km2)
Ras_pasture_ag <- resample(Ras_pasture, Ras_pH) * area(Ras_pH)

# total area checks
Ras_pasture_ag %>%
  as.data.frame(xy = T) %>%
  as_tibble() %>%
  pull(layer) %>%
  sum(na.rm = T)


# write out created rasters for future use
writeRaster(Ras_corine_UK, find_onedrive(dir = bigdata_repo, path = "Created rasters/Corine-land-cover-100m-UKonly-WGS84.tif"))
writeRaster(Ras_pasture_ag, find_onedrive(dir = bigdata_repo, path = "Created rasters/UK-pasture-area-10km-CLC-based-WGS84.tif"))

# read in raw data for Defra's grass growth model and raster for grass growth class to parameterise
# we're borrowing from another project here — so pulling project files directly from that project's repo
Dat_GGM <- find_onedrive(dir = "AgRE Calc PLC/Soil C methodology/Model update/Data preprocessing/Defra RB209 grass model",
                         path = "Grass model digitisation raw.csv") %>%
  read_csv()

Ras_GGC <- find_onedrive(dir = bigdata_repo, path = "Created rasters/Defra_RB209_grass_growth_class_UK_250m.tif") %>% raster()
plot(Ras_GGC)
plot(Shp_UK, add = T)
#Ras_GGC <- projectRaster(Ras_GGC, crs = newproj)
# something funky going on here. Raster was created with WGS84 projection, saved with none, and now won't line up..!

Ras_GGC_ag <- resample(Ras_GGC, Ras_pH)
Ras_GGC %>% plot()
Ras_pH %>% plot(add = T)
Ras_GGC_ag %>% plot()
