
library(raster)
library(tidyverse)
library(sp)

bigdata_repo <- "GIS data repository"

corine <- find_onedrive(dir = bigdata_repo, path = "Corine raster 100m/CLC2018_CLC2018_V2018_20.tif") %>% raster()


plot(corine)

UK <- find_onedrive(dir = bigdata_repo, path = "DA shapefile/GBR_adm_shp/GBR_adm1.shp") %>% shapefile()

corine_UK <- corine %>% crop(UK) %>% mask(UK)

Soil_stack <- stack(find_onedrive(dir = data_repo, path = "GIS data/SoilGrids 5km/Soil pH/Fixed/PHIHOX_M_sl4_5km_ll.tif") %>% raster(), # pH
                    find_onedrive(dir = data_repo, path = "GIS data/SoilGrids 5km/Sand content/Fixed/SNDPPT_M_sl4_5km_ll.tif"), # sand %
                    find_onedrive(dir = data_repo, path = "GIS data/SoilGrids 5km/Silt content/Fixed/SLTPPT_M_sl4_5km_ll.tif"), # silt %
                    find_onedrive(dir = data_repo, path = "GIS data/SoilGrids 5km/Clay content/Fixed/CLYPPT_M_sl4_5km_ll.tif"), # clay %
                    find_onedrive(dir = data_repo, path = "GIS data/SoilGrids 5km/OC tonnes per ha/Fixed/OCSTHA_M_sd4_5km_ll.tif")) # OC tonnes per ha
