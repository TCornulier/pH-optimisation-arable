library(raster)
library(tidyverse)
library(sp)

bigdata_repo <- "GIS data repository"

# read in pH raster as template
Ras_pH <- find_onedrive(dir = bigdata_repo, path = "SoilGrids 5km/Soil pH/PHIHOX_M_sl4_5km_ll.tif") %>% raster()

# soil data stack
Soil <- stack(find_onedrive(dir = bigdata_repo, path = "SoilGrids 5km/Sand content/Fixed/SNDPPT_M_sl4_5km_ll.tif"),
              find_onedrive(dir = bigdata_repo, path = "SoilGrids 5km/Clay content/Fixed/CLYPPT_M_sl4_5km_ll.tif"),
              find_onedrive(dir = bigdata_repo, path = "SoilGrids 5km/Depth to bedrock/Fixed/BDRICM_M_5km_ll.tif"),
              find_onedrive(dir = bigdata_repo, path = "SoilGrids 5km/OC tonnes per ha/Fixed/OCSTHA_M_30cm_5km_ll.tif"),
              find_onedrive(dir = bigdata_repo, path = "SoilGrids 5km/Bulk density/Fixed/BLDFIE_M_sl4_5km_ll.tif"))

# precipitation stack
Precip <- raster::stack()
for(i in 4:9){
  path <- find_onedrive(dir = bigdata_repo, path = paste("WorldClim data/Precipitation (mm) 30-arc-secs/wc2.0_30s_prec_", formatC(i, width=2, flag="0"), ".tif", sep=""))
  x <- raster(path)
  Precip <- addLayer(Precip, x)
}
rm(x, i , path)

# uk shapefile
Shp_UK <- find_onedrive(dir = bigdata_repo, path = "DA shapefile/GBR_adm_shp/GBR_adm1.shp") %>% shapefile()

# crop and mask to UK
Soil_UK <- Soil %>% crop(Shp_UK) %>% mask(Shp_UK)
Precip_UK <- Precip %>% crop(Shp_UK) %>% mask(Shp_UK)

# resample precipitation to correct extent + resolution
Precip_UK <- Precip_UK %>% resample(Soil_UK[[1]])

# create master stack and convert to df
Master <- stack(Soil_UK, Precip_UK)
Dat_main <- as.data.frame(Master, xy = T) %>%
  as_tibble() %>%
  drop_na()

# name properly, sum up precipitation and calculate clay fraction
precip_temp <- Dat_main %>% select(wc2.0_30s_prec_04:wc2.0_30s_prec_09)
Dat_main <- Dat_main %>%
  rename(Sand = SNDPPT_M_sl4_5km_ll,
         Clay = CLYPPT_M_sl4_5km_ll,
         Bedrock = BDRICM_M_5km_ll,
         OC = OCSTHA_M_30cm_5km_ll,
         BD = BLDFIE_M_sl4_5km_ll) %>%
  select(-(wc2.0_30s_prec_04:wc2.0_30s_prec_09)) %>%
  mutate(Precip_mm = rowSums(precip_temp),
         Silt = 100 - (Sand + Clay))
rm(precip_temp)

# assign soil type based on texture data (details here https://cran.r-project.org/web/packages/soiltexture/vignettes/soiltexture_vignette.pdf)
library(soiltexture)

Dat_soil <- Dat_main %>%
  dplyr::select(SAND = Sand, SILT = Silt, CLAY = Clay, OC) %>%
  mutate(OC = OC / 10) %>%
  as.data.frame()

Dat_main <- Dat_main %>%
  mutate(Soil_type = TT.points.in.classes(tri.data = Dat_soil,
                                          class.sys = "UK.SSEW.TT",
                                          PiC.type = "t"))

# read in soil category translations (manual) and join to main data
# borrowing from another project's directory here
Soil_cats <- read_csv(find_onedrive(dir = "AgRE Calc PLC/Soil C methodology/Model update/Data preprocessing/Defra RB209 grass model",
                                    path = "Defra soil type translations.csv"))
Dat_main <- Dat_main %>% left_join(Soil_cats %>% rename(Soil_type = cat, RB209_soiltype = trans), by = "Soil_type")

# function to calculate OM fraction based on C (t/ha) and BD (kg / m2)
OM_frac <- function(BD_kg_m2, C_t_ha){
  C_kg_m2 <- C_t_ha * 10^3 * 10^-4 * 1 / 0.3
  Cfrac <- C_kg_m2 / BD_kg_m2
  OMfrac <- Cfrac / 0.58
  return(OMfrac)
}

# calculate other variables not based on SSEW soil types and adjust RB209 definitions accordingly
Dat_main <- Dat_main %>%
  mutate(RB209_soiltype = ifelse(Bedrock <= 40, "Shallow soils", RB209_soiltype), # shallow soils <= 40cm
         OMfrac = OM_frac(BD, OC),
         RB209_soiltype = ifelse(OMfrac >= 0.1, "Organic soils", RB209_soiltype), # organic soils
         RB209_soiltype = ifelse(OMfrac >= 0.2, "Peat soils", RB209_soiltype)) # peat soils

# water availability class from Defra RB209
Water_availability <- Dat_main %>%
  distinct(RB209_soiltype) %>%
  arrange(RB209_soiltype) %>%
  mutate(SAW = c(2, 3, 1, 2, 3)) # soil available water class — 1 = low, 2 = medium, 3 = high

Dat_main <- Dat_main %>% left_join(Water_availability, by = "RB209_soiltype")         

# rainfall classes from Defra RB209 and final pasture growth class
# addition method is something I've inferred — works if you parameterise SAW from 1-3 and rain class from 0-2 or vice versa
# rain classes are <300, 300-400, >400
breaks <- c(min(Dat_main$Precip_mm) - 1, 300, 400, max(Dat_main$Precip_mm) + 1)

Dat_main <- Dat_main %>%
  mutate(RC = Precip_mm %>%
           cut(breaks = breaks, labels = F) %>%
           as.numeric(),
         GGC = SAW + RC - 1) # 1 = V poor, 2 = Poor, 3 = Av, 4 = Good, 5 = V good

Dat_main$GGC %>% qplot()

# read in raw data for Defra's grass growth model
# we're borrowing from another project here — so pulling project files directly from that project's repo
Dat_GGM <- find_onedrive(dir = "AgRE Calc PLC/Soil C methodology/Model update/Data preprocessing/Defra RB209 grass model",
                         path = "Defra RB209 final models.csv") %>%
  read_csv()

# gather data and fit models
Dat_GGM <- Dat_GGM %>%
  gather(Vpoor:Vgood, key = "GGC", value = "Yield")

GGC_models = list(Vpoor = loess(Yield ~ N_rate, data = Dat_GGM %>% filter(GGC == "Vpoor")),
                  Poor = loess(Yield ~ N_rate, data = Dat_GGM %>% filter(GGC == "Poor")),
                  Av = loess(Yield ~ N_rate, data = Dat_GGM %>% filter(GGC == "Av")),
                  Good = loess(Yield ~ N_rate, data = Dat_GGM %>% filter(GGC == "Good")),
                  Vgood = loess(Yield ~ N_rate, data = Dat_GGM %>% filter(GGC == "Vgood")))

# make pasture yield predictions
# unable to get spatially specific N estimates as yet so using BFSP N application estimate for pasture
BSFP_Nrate <- 57
Pas_yield <- function(GGC){
  predict(GGC_models[[GGC]], newdata = tibble(N_rate = BSFP_Nrate))
}

Dat_main <- Dat_main %>%
  mutate(Yield = map_dbl(GGC, Pas_yield))

# write to raster
Pasture_yield <- rasterFromXYZ(Dat_main %>% select(x, y, Yield),
                               crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
plot(Pasture_yield)
Pasture_yield
# read in area raster and resample
Pasture_area <- find_onedrive(dir = bigdata_repo, path = "Created rasters/UK-pasture-area-10km-CLC-based-WGS84.tif") %>% raster()
Pasture_area
Pasture_yield <- extend(Pasture_yield, Pasture_area)

Pasture_yield
Pasture_area
plot(Pasture_yield, add = T)
stack(Pasture_yield, Pasture_area)
plot(Pasture_area)
