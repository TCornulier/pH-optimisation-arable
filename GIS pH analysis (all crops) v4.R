# packages
library(raster)
library(tidyverse)
library(sp)
library(soiltexture)

data_repo <- "DEFRA Clean Growth Project/pH Optimisation/Extension for publication"

UK <- find_onedrive(dir = data_repo, path = "GIS data/DA shapefile/GBR_adm_shp/GBR_adm1.shp") %>% shapefile()

# read in soil raster data and stack
Soil_stack <- stack(#find_onedrive(dir = data_repo, path = "GIS data/SoilGrids 5km/Soil pH/Fixed/PHIHOX_M_sl4_5km_ll.tif"), # pH
                    find_onedrive(dir = data_repo, path = "GIS data/SoilGrids 5km/Sand content/Fixed/SNDPPT_M_sl4_5km_ll.tif"), # sand %
                    find_onedrive(dir = data_repo, path = "GIS data/SoilGrids 5km/Silt content/Fixed/SLTPPT_M_sl4_5km_ll.tif"), # silt %
                    find_onedrive(dir = data_repo, path = "GIS data/SoilGrids 5km/Clay content/Fixed/CLYPPT_M_sl4_5km_ll.tif"), # clay %
                    #find_onedrive(dir = data_repo, path = "GIS data/SoilGrids 5km/OC tonnes per ha/Fixed/OCSTHA_M_30cm_5km_ll.tif"), # OC tonnes per ha
                    find_onedrive(dir = data_repo, path = "GIS data/SoilGrids 5km/Bulk density/Fixed/BLDFIE_M_sl4_5km_ll.tif"))

# read in crop area raster data and stack
readdir <- find_onedrive(dir = data_repo, path = "GIS data/MapSPAM data/Physical area")
file.names <- dir(readdir, pattern =".tif")

Crop_area_stack <- raster::stack()
for(i in 1:length(file.names)){
  readpath <- paste(readdir, file.names[i], sep="/") # aggregate strings to create filepath
  x <- raster(readpath) # read raster
  Crop_area_stack <- addLayer(Crop_area_stack, x)
  rm(x)
  print(file.names[i])
}

# read in crop yield raster data and stack
readdir <- find_onedrive(dir = data_repo, path = "GIS data/MapSPAM data/Yield")
file.names <- dir(readdir, pattern =".tif")

Crop_yield_stack <- raster::stack()
for(i in 1:length(file.names)){
  readpath <- paste(readdir, file.names[i], sep="/") # aggregate strings to create filepath
  x <- raster(readpath) # read raster
  Crop_yield_stack <- addLayer(Crop_yield_stack, x)
  rm(x)
  print(file.names[i])
}
rm(readdir, file.names, readpath, i)

# add area layer
Area <- Soil_stack[[1]] %>% area()
Soil_stack <- addLayer(Soil_stack, Area)
rm(Area)

# consolidate
Master_stack <- stack(Soil_stack, Crop_area_stack, Crop_yield_stack)
rm(Soil_stack, Crop_area_stack, Crop_yield_stack)

# mask out to UK only
Master_stack <- Master_stack %>% crop(UK) %>% mask(UK)

# read data created using CLC raster as base —
# pasture area and yield data (created in Pasture preprocessing scripts)
# land use specific SOC stocks and pH (created in [Soil preprocessing.R])
# fraction of land use type under mineral soils (i.e. not under peat), created in [Soil preprocessing.R]
CLC_stack <- stack(find_onedrive(dir = "GIS data repository", path = "Created rasters/UK-crop-SOC-tonnes-ha-10km-CLC-SG250-WGS84.tif"),
                   find_onedrive(dir = "GIS data repository", path = "Created rasters/UK-crop-pH-10km-CLC-SG250-WGS84.tif"),
                   find_onedrive(dir = "GIS data repository", path = "Created rasters/UK-pasture-SOC-tonnes-ha-10km-CLC-SG250-WGS84.tif"),
                   find_onedrive(dir = "GIS data repository", path = "Created rasters/UK-pasture-pH-10km-CLC-SG250-WGS84.tif"),
                   find_onedrive(dir = "GIS data repository", path = "Created rasters/UK-crop-fraction-not-under-peat-10km-CLC-based-WGS84.tif"),
                   find_onedrive(dir = "GIS data repository", path = "Created rasters/UK-pasture-fraction-not-under-peat-10km-CLC-based-WGS84.tif"),
                   find_onedrive(dir = "GIS data repository", path = "Created rasters/UK-pasture-area-10km-CLC-based-WGS84-lowland-workable.tif"),
                   find_onedrive(dir = "GIS data repository", path = "Created rasters/UK-pasture-yield-RB209-10km.tif"))

# resample
CLC_stack <- CLC_stack %>% resample(Master_stack)

# consolidate
Master_stack <- stack(CLC_stack, Master_stack)
rm(CLC_stack)

# convert to dataframe
Dat_main <- Master_stack %>% as.data.frame(xy = T) %>% drop_na(SNDPPT_M_sl4_5km_ll) # nothing special about sand % except it has full land area coverage — NA = sea or water body
# we have zeros, >0s and NAs in the area data, NAs and >0s only in the yield data

Dat_main <- Dat_main %>%
  tbl_df %>%
  rename(x = x,
         y = y,
         OC_crop = UK.crop.SOC.tonnes.ha.10km.CLC.SG250.WGS84,
         OC_pasture = UK.pasture.SOC.tonnes.ha.10km.CLC.SG250.WGS84,
         pH_crop = UK.crop.pH.10km.CLC.SG250.WGS84,
         pH_pasture = UK.pasture.pH.10km.CLC.SG250.WGS84,
         Min_frac_crop = UK.crop.fraction.not.under.peat.10km.CLC.based.WGS84,
         Min_frac_pasture = UK.pasture.fraction.not.under.peat.10km.CLC.based.WGS84,
         phys_area_pasture = UK.pasture.area.10km.CLC.based.WGS84.lowland.workable,
         yield_pasture = UK.pasture.yield.RB209.10km,
         Sand = SNDPPT_M_sl4_5km_ll,
         Silt = SLTPPT_M_sl4_5km_ll,
         Clay = CLYPPT_M_sl4_5km_ll,
         BD = BLDFIE_M_sl4_5km_ll,
         Cell_area_km2 = layer) %>%
  mutate(pH_crop = pH_crop / 10,
         pH_pasture = pH_pasture / 10) # for some reason it's * 10 in original raster

glimpse(Dat_main)

# select out crops with zero area
isnt_zero <- function(x){
  y <- x == 0 | is.na(x)
  z <- F %in% y == T
  return(z)
}

Dat_main <- Dat_main %>%
  select_if(isnt_zero)

# convert pasture area from km2 to ha
Dat_main <- Dat_main %>%
  mutate(phys_area_pasture = phys_area_pasture * 10^6 / 10^4)

# reorder to something sensible
Dat_main <- Dat_main %>%
  select(x:Min_frac_pasture, Sand:Cell_area_km2, # physical data
         phys_area_pasture, phys_area_barley:phys_area_wheat, # crop area data
         yield_pasture, yield_barley:yield_wheat) # crop yield data
  
# gather up into crop types and metrics (area/yield)
# function to help rename crop variables
first_upper <- function(string){
  start <- str_sub(string, 1L, 1L) %>% str_to_upper()
  rest <- str_sub(string, 2L, -1L) %>% str_to_lower()
  whole <- paste0(start, rest)
  return(whole)
}

Dat_main <- Dat_main %>%
  gather(14:ncol(Dat_main), key = "key", value = "value") %>%
  mutate(Crop = key %>% str_replace_all("(phys_area_)|(yield_)", "") %>%
           first_upper() %>%
           str_replace_all("_", " ") %>%
           str_replace_all(" other", ", other"),
         Metric = key %>%
           str_extract("phys_area|yield")) %>%
  dplyr::select(-key) %>%
  spread(key = Metric, value = value) %>%
  rename(Area_ha = phys_area, Yield_tha = yield) %>%
  mutate(Area_ha = ifelse(Area_ha == 0, NA, Area_ha)) %>%
  drop_na(Area_ha)

# consolidate land-use-specific estimates now data is gathered
Dat_main <- Dat_main %>%
  mutate(OC = ifelse(Crop == "Pasture", OC_pasture, OC_crop),
         pH = ifelse(Crop == "Pasture", pH_pasture, pH_crop),
         Min_frac = ifelse(Crop == "Pasture", Min_frac_pasture, Min_frac_crop))

# tidy up missing data (primarily some few areas where CLC didn't have crop data, but MapSPAM does, c. 600 cells)
# only dropping a few rows at the very end
Dat_main <- Dat_main %>%
  mutate(OC = ifelse(is.na(OC), OC_pasture, OC), # first touch — replace NAs due to missing crop OC with equivalent pasture values
         OC = ifelse(is.na(OC), OC_crop, OC), # some very few where no pasture data is present, but crop data is
         pH = ifelse(is.na(pH), pH_pasture, pH), # exactly the same for pH
         pH = ifelse(is.na(pH), pH_crop, pH),
         Min_frac = ifelse(is.na(Min_frac), Min_frac_pasture, Min_frac), # and for mineral fraction
         Min_frac = ifelse(is.na(Min_frac), Min_frac_crop, Min_frac),
         Min_frac = ifelse(is.na(Min_frac), 1, Min_frac)) %>% # we have a default of 1 here
  drop_na(OC, pH) %>%
  select(x, y, Sand:BD, OC:Min_frac, Cell_area_km2:Yield_tha) # drop land-use-specific variables and reorder

# process rasters so we can make plots at DA level
template <- Master_stack[[9]] # sand % makes a good template
England <- template %>% mask(subset(UK, UK@data[["NAME_1"]]=="England"))
Northern_Ireland <- template %>% mask(subset(UK, UK@data[["NAME_1"]]=="Northern Ireland"))
Scotland <- template %>% mask(subset(UK, UK@data[["NAME_1"]]=="Scotland"))
Wales <- template %>% mask(subset(UK, UK@data[["NAME_1"]]=="Wales"))

Eng_df <- England %>% as.data.frame(xy = T) %>% drop_na(SNDPPT_M_sl4_5km_ll)
NI_df <- Northern_Ireland %>% as.data.frame(xy = T) %>% drop_na(SNDPPT_M_sl4_5km_ll)
Scot_df <- Scotland %>% as.data.frame(xy = T) %>% drop_na(SNDPPT_M_sl4_5km_ll)
Wales_df <- Wales %>% as.data.frame(xy = T) %>% drop_na(SNDPPT_M_sl4_5km_ll)

DA_dat <- bind_rows(list(England = Eng_df, `Northern Ireland` = NI_df, Scotland = Scot_df, Wales = Wales_df), .id = "DA")  %>%
  dplyr::select(-SNDPPT_M_sl4_5km_ll)

Dat_main <- left_join(Dat_main, DA_dat, by = c("x", "y"))
Dat_main <- Dat_main %>%
  select(x, y, DA, Sand:Yield_tha)

# cumulative probability distribution for pH under different crops
Dat_cdf <- Dat_main %>%
  mutate(pH = pH %>% round(1)) %>%
  group_by(pH, Crop) %>%
  summarise(n = n(),
            Area_ha = sum(Area_ha)) %>%
  arrange(Crop, pH) %>%
  group_by(Crop) %>%
  mutate(Area_cum = cumsum(Area_ha),
         Freq = Area_cum / max(Area_cum))

ggplot(Dat_cdf, aes(x = pH, y = Freq, colour = Crop)) +
  geom_line() +
  theme_classic()

Dat_cdf_av <- Dat_cdf %>%
  filter(Crop != "Pasture") %>%
  group_by(pH) %>%
  summarise(Area_ha = sum(Area_ha)) %>%
  arrange(pH) %>%
  mutate(Area_cum = cumsum(Area_ha),
         Freq = Area_cum / max(Area_cum))

ggplot(Dat_cdf %>% filter(Crop != "Rest of crops")) + # we lose 913 ha of average land, and mean we have a neat 12 crops for our facets, not unlucky 13..!
  geom_line(aes(x = pH, y = Freq), colour = "darkred") +
  geom_line(data = Dat_cdf_av, aes(x = pH, y = Freq), colour = "grey", lty = 2) +
  facet_wrap(~Crop, nrow = 4) +
  labs(x = "pH", y = "Frequency") +
  theme_classic()
ggsave(find_onedrive(dir = data_repo, path = "Output plots/pH CDFs, all crops.png"), width = 8, height = 6)

# pH relationship with yield
Dat_main %>%
  group_by(Crop) %>%
  mutate(Rel_yield = Yield_tha / sum(Yield_tha)) %>%
  ggplot(aes(x = pH, y = Rel_yield)) +
  geom_point(colour = "darkred", alpha = 0.05) +
  geom_smooth(method = "lm", colour = "grey", lty = 2) +
  facet_wrap(~Crop, nrow = 4) +
  theme_classic()

# compare to pH distributions for UK arable/grassland from PAAG (2016)
pH_PAAG <- tibble(pH = c(4.5, 5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5),
                 freq_arable = c(0, 0.01, 0.04, 0.12, 0.22, 0.24, 0.16, 0.14, 0.08),
                 freq_grass = c(0, 0.02, 0.17, 0.33, 0.27, 0.12, 0.05, 0.03, 0.01)) %>%
  mutate(cumfreq_arable = cumsum(freq_arable),
         cumfreq_grass = cumsum(freq_grass))

pH_dist_arable <- loess(cumfreq_arable ~ pH, data = pH_PAAG, span = 0.5)

Dat_cdf_av <- Dat_cdf_av %>%
  mutate(Freq_pred = predict(pH_dist_arable, pH))

Dat_cdf_av %>%
  mutate(error = Freq_pred - Freq,
         error2 = error ^ 2) %>%
  summarise(error2 = mean(error2)) %>%
  mutate(rmse = sqrt(error2))

# use soiltexture package to figure out soil type (details here https://cran.r-project.org/web/packages/soiltexture/vignettes/soiltexture_vignette.pdf)
Dat_soil <- Dat_main %>%
  dplyr::select(SAND = Sand, SILT = Silt, CLAY = Clay, OC) %>%
  mutate(OC = OC / 10,
         tot = SAND + SILT + CLAY) %>%
  mutate_at(vars(SAND:CLAY), funs(. / tot * 100)) %>% # it's all basically at 100% but soiltexture seems to require it be exact
  dplyr::select(-tot) %>%
  as.data.frame()

Dat_main <- Dat_main %>%
  mutate(Soil_type = TT.points.in.classes(tri.data = Dat_soil,
                                          class.sys = "UK.SSEW.TT",
                                          PiC.type = "t"))

# operation to add liming factor to main data
DEFRA_LF <- tibble(Soil_type = TT.points.in.classes(tri.data = Dat_soil, class.sys = "UK.SSEW.TT") %>% colnames(),
                   Liming_factor_arable = c(8, 8, 8, 8, 7.5, 7.5, 7, 7, 7, 6, 6),
                   Liming_factor_grass = c(6, 6, 6, 6, 5.5, 5.5, 5, 5, 5, 4, 4))
# write_csv(DEFRA_LF, "Defra liming factors.csv")

Liming_factor <- function(Soil_type, Land_use){
  Soil_type_search <- str_split(Soil_type, "\\W+") %>% unlist()
  LF_match <- DEFRA_LF[DEFRA_LF$Soil_type %in% Soil_type_search, ]
  LF_arable <- LF_match$Liming_factor_arable %>% mean()
  LF_grass <- LF_match$Liming_factor_grass %>% mean()
  LF <- ifelse(Land_use == "Pasture", LF_grass, LF_arable)
  return(LF)
}

Dat_main <- Dat_main %>%
  mutate(LF = map2_dbl(Soil_type, Crop, Liming_factor))

# match up yield response models to data
Dat_yieldres <- bind_rows(read_csv("Holland et al. (2019) yield curves full data.csv"),
                          read_csv("Woodlands pH rotation model parameters.csv")) %>%
  dplyr::select(-p_value, -phos_effect)

# if r2 is less than 10%, assume no yield response (a = 1, b = 0, d = 0)
# examination of the curves reveals all in this category are very flat anyway
Dat_yieldres <- Dat_yieldres %>%
  mutate(a_est = ifelse(r2 < 0.1, 1, a_est),
         b_est = ifelse(r2 < 0.1, 0, b_est),
         d_est = ifelse(r2 < 0.1, 0, d_est))

# summarise and match models to crop data
Dat_yieldres <- Dat_yieldres %>%
  group_by(crop, site) %>%
  summarise_at(vars(a_est:d_est), funs(mean(.))) %>%
  ungroup() %>%
  mutate(data_cropmatch = c("Barley", "Pasture", "Oil crops, other", "Oil crops, other", "Cereals, other",
                            "Potato", "Potato", "Potato", "Barley", "Barley",
                            "Pulses, other", "Pulses, other", NA, NA, "Cereals, other",
                            "Cereals, other", "Vegetable", "Wheat", "Rapeseed", "Rapeseed",
                            "Cereals, other", "Wheat", "Wheat"))

Dat_yieldres <- Dat_yieldres %>%
  mutate(model_no = 1:nrow(Dat_yieldres))

# yield response for pasture from Fornara et al. (2011)
# superceded by Woodlands Field pasture model (produces similar results regardless)
# Dat_yieldres_pasture <- read_csv(find_onedrive(dir = data_repo, path = "Fornara yield response.csv"))

# soil fractions, based on Holland paper for Rothamsted and Woburn, stated soil type/typical fractions for Woodlands — refine if possible
Dat_soil <- tibble(site = c("Rothamsted", "Woburn", "Woodlands"),
                   sand = c(28, 71, 63),
                   silt = c(52, 17, 25),
                   clay = c(20, 12, 12))

# function to select most appropriate yield response model for crop and soil type
# selects most similar soil type for which model is available
expt_select <- function(sand2, silt2, clay2, crop_name){
  x <- Dat_yieldres %>%
    filter(data_cropmatch == crop_name)
  
  y <- Dat_soil %>%
    filter(site %in% x$site) %>%
    mutate(SS = (sand - sand2)^2 + (silt - silt2)^2 + (clay - clay2)^2) %>%
    filter(SS == min(SS)) # selection using LSS method for sand/silt/clay fractions
  
  z <- x$model_no[match(y$site, x$site)]
  if(length(z) == 0) z <- NA
  return(z)
}

# match model to data (still feels like it takes longer than it should...)
Dat_main <- Dat_main %>%
  mutate(model_no = pmap_dbl(list(Sand, Silt, Clay, Crop), expt_select))

Dat_main <- Dat_main %>%
  left_join(Dat_yieldres %>% select(model_no, Crop = data_cropmatch, a_est, b_est, d_est), by = c("model_no", "Crop"))

# calculate relative yields at given pH and possible yields at target pH
rel_yield <- function(A, B, D, pH){
  rel_yield <- A + (B / (1 + D * pH))
  rel_yield <- ifelse(rel_yield < 0.1, 0.1, rel_yield) # line added to avoid negative or divide by zero errors. Clunky... avoid in future?
  return(rel_yield)
}

# adjust crop area to reflect fraction under mineral soils (i.e. exclude peat soils)
Dat_main <- Dat_main %>%
  mutate(Area_ha = Area_ha * Min_frac)

# function to infer target pH
target_pH <- function(Crop, pH){

  # define target with sequential logicals
  target_pH <- pH # assume baseline is no change
  target_pH <- ifelse(Crop == "Pasture" & pH < 6.0, 6.0, target_pH) # 6 target for grass (RB209)
  target_pH <- ifelse(Crop != "Pasture" & pH < 6.5, 6.5, target_pH) # 6.5 target for crops (RB209)
  
  return(target_pH)
}

# yield increases for croplands
Dat_main <- Dat_main %>%
  mutate(Rel_yield = rel_yield(a_est, b_est, d_est, pH),
         Target_pH = target_pH(Crop, pH),
         Poss_yield = rel_yield(a_est, b_est, d_est, Target_pH),
         Yield_increase = Poss_yield / Rel_yield,
         pH_diff = Target_pH - pH) %>%
  dplyr::select(-(a_est:d_est))

# add in pasture cases
# superceded by Woodlands Field grass model
#Pas_yield_fac <- Dat_yieldres_pasture$mean[1] - 1
#
#Dat_main <- Dat_main %>%
#  mutate(Rel_yield = ifelse(Crop == "Pasture",
#                            1 / (1 + Pas_yield_fac * pH_diff),
#                            Rel_yield),
#         Poss_yield = ifelse(Crop == "Pasture",
#                             Rel_yield * (1 + Pas_yield_fac * pH_diff),
#                             Poss_yield),
#         Yield_increase = ifelse(Crop == "Pasture",
#                                 Poss_yield / Rel_yield,
#                                 Yield_increase))

# load SOC response curve function derived in separate script and apply to main data
# function output is fractional
source("SOC RC function v2.R")

Dat_main <- Dat_main %>%
  mutate(SOCchange_frac = SOC_RC(ipH = pH, fpH = Target_pH))

# add in pasture cases - C sequestration predictions using data from Fornara et al. (2011)

Dat_main <- Dat_main %>%
  mutate(SOCchange_frac = ifelse(Crop == "Pasture", 1 + C_response_fac_grass * pH_diff * 20, SOCchange_frac))

# we avoided dropping crops without matched models earlier to allow us to add in pasture using a different approach;
# now we need to drop those crops which still have no data for yield etc.
Dat_main <- Dat_main %>%
  drop_na(Yield_increase)

# EI for different crops in g CO2-eq per kg, data from Feedprint for on-farm production only
# 'cereals, other' classed as oats, 'oil crops. other' classed as linseed, 'pulses, other' classed as beans
# EI for vegetables based on root crops/onions/cabbages figure from Wallen et al. (2004) (Feedprint doesn't do vegetable EFs)
# EI for pasture based on feedprint EI for grass silage
Dat_EI <- tibble(Crop = Dat_main %>% pull(Crop) %>% unique(),
                 EI = c(235, 343, 465, 1222, 226, 766, 984, 500, 349))

Dat_main <- Dat_main %>%
  left_join(Dat_EI, by = "Crop")

# calculate equivalent crop production emissions per hectare, and EI based emissions 'savings' resulting from yield improvements
Dat_main <- Dat_main %>%
  mutate(GHG_tha_nolime = EI * 10^-6 * Yield_tha * 10^3,
         EI_limed = EI / Yield_increase,
         GHG_tha_lime = EI_limed * 10^-6 * Yield_tha * 10^3,
         GHGmit_yield = GHG_tha_nolime - GHG_tha_lime)

# calculate emissions mitigation from SOC accumulation
Dat_main <- Dat_main %>%
  mutate(OC_lime = OC * SOCchange_frac,
         GHGmit_SOC = ((OC_lime - OC) * 44/12) / 20)

# calculate emissions from lime application
Dat_main <- Dat_main %>%
  mutate(Limerate = (LF * (pH_diff + 0.2)) / 5, # Assuming a 5 year interval between applications (based on a variety of data sources) + 0.2 pH unit overshoot as recommended in RB209
         Limeemb_GHG = 0.074 * Limerate, # Kool et al. (2012)
         Limedir_GHG = 0.125 * 44/12 * Limerate, # IPCC (2006)
         Dies_GHG = (336 * 0.7) / 36.9 * 3.165 * 10^-3) # Williams et al (2006) for diesel usage * DEFRA/DECC for EF


# sale values for different crops from FMH 17/18, all in 2017 GBP
# for grass, estimate is mean production cost from FMH 17/18
# linseed uses OSR values, potatoes assumes dual purpose and price is weighted according to relative yields
# vegetables takes data for potatoes — very similar to most veg prices
Dat_saleval <- tibble(Crop = Dat_main %>% pull(Crop) %>% unique(),
                      Maincrop_saleval = c(22.5, 145, 155, 325, 113, 200, 325, 113, 165),
                      Bycrop_saleval = c(0, 55, 50, 0, 0, 0, 0, 0, 50), # secondary crop e.g. straw
                      Bycrop_ratio = c(0, 0.55, 0.60, 0, 0, 0, 0, 0, 0.53)) # ratio of secondary crop to main crop yield

# join sale values to main data
Dat_main <- Dat_main %>% left_join(Dat_saleval, by = "Crop")

# calculate costs and benefits
Dat_main <- Dat_main %>%
  mutate(Crop_revenue = Yield_tha * Maincrop_saleval + Yield_tha * Bycrop_ratio * Bycrop_saleval,
         Crop_revenue_lime = Crop_revenue * Yield_increase,
         Crop_revenue_net = Crop_revenue_lime - Crop_revenue,
         
         Lime_cost = Limerate * 35, # FMH 17/18 lime cost,
         Cont_cost = Limerate * 4, # FMH 17/18 contractor cost
         
         Cost_net_ha = (Lime_cost + Cont_cost) - Crop_revenue_net
         )

#####################
# added 21/01/2020 to include N2O predictions using the Hillier/Cornulier model
#####################

####################
# augmenting main dataset with additional variables req'd for N2O model
library(ncdf4)

# 8 year dataset of wet days / month
nc_open(find_onedrive(dir = data_repo, path = "GIS data/CRU TS v4-03/cru_ts4.03.2011.2018.wet.dat.nc"))

# convert to raster brick
wet_days <- brick(find_onedrive(dir = data_repo, path = "GIS data/CRU TS v4-03/cru_ts4.03.2011.2018.wet.dat.nc"), varname = "wet")

# cropping for efficiency, but not masking until we resample
wet_days <- wet_days %>% crop(UK)

# resample to master stack values
wet_days <- wet_days %>% resample(Master_stack[[1]])

# mask to UK
wet_days <- wet_days %>% mask(UK)

# convert to df
Dat_wetdays <- wet_days %>%
  as.data.frame(xy = T) %>%
  as_tibble() %>%
  drop_na()

# gather and prep year variable
Dat_wetdays <- Dat_wetdays %>%
  gather(-x, -y, key = "Year", value = "Wet_days") %>%
  mutate(Year = Year %>%
           str_replace_all("\\..+", "") %>%
           str_replace_all("X", "") %>%
           as.numeric())

# annual sums
Dat_wetdays <- Dat_wetdays %>%
  group_by(x, y, Year) %>%
  summarise(Wet_days = sum(Wet_days),
            n = n())

Dat_wetdays$n %>% table()

# 8-year average
Dat_wetdays <- Dat_wetdays %>%
  group_by(x, y) %>%
  summarise(Wet_days_mean = mean(Wet_days),
            Wet_days_sd = sd(Wet_days),
            Wet_days_se = sd(Wet_days) / sqrt(n()),
            Wet_days_min = min(Wet_days),
            Wet_days_max = max(Wet_days)) %>%
  ungroup()

# anti_join(Dat_wetdays, Dat_main, by = c("x", "y")) # anti join checked, all required cells match fine (some non-joined cells in areas cut from Dat_main)

#ggplot(Dat_wetdays, aes(x = x, y = y, fill = Wet_days_se)) +
#  geom_raster() +
#  coord_quickmap() +
#  theme_void()

# read in WorldClim temperature data
temperature <- raster::stack()
for(i in 1:12){
  path <- find_onedrive(dir = data_repo, path = paste("GIS data/Average temperature (oC)/wc2.0_5m_tavg_", formatC(i, width=2, flag="0"), ".tif", sep=""))
  x <- raster(path)
  temperature <- addLayer(temperature, x)
}
rm(x, i , path)

# convert to UK DF and calculate degree days
library(lubridate)

temperature <- temperature %>%
  crop(UK) %>%
  resample(Master_stack[[1]]) %>%
  mask(UK)

Dat_temp <- temperature %>%
  as.data.frame(xy = T) %>%
  as_tibble() %>%
  drop_na() %>%
  gather(-x, -y, key = "Key", value = "oC") %>%
  mutate(Date = paste0("01/", str_sub(Key, start = -2L, end = -1L), "/2019") %>% dmy(), # random non-leap year
         Days = days_in_month(Date),
         Degree_days = oC * Days) %>%
  group_by(x, y) %>%
  summarise(Degree_days = sum(Degree_days)) %>%
  ungroup()

# N rate data based on analysis of BSFP reports
Dat_fert <- read_csv(find_onedrive(dir = data_repo, path = "N rate data/BSFP Fertiliser rates annual summary.csv"))
Dat_man <- read_csv(find_onedrive(dir = data_repo, path = "N rate data/BSFP manure rates summary.csv"))

Dat_man <- Dat_man %>%
  gather(3:10, key = "Year", value = "Rate") %>%
  group_by(`Crop type`, Nutrient) %>%
  summarise(Rate = mean(Rate)) %>%
  filter(Nutrient == "N (kg / ha)") %>%
  select(-Nutrient) %>%
  rename(Crop_name3 = `Crop type`, Mrate_mean = Rate)

Dat_Nrate <- Dat_main %>%
  distinct(Crop) %>%
  arrange(Crop) %>%
  mutate(Crop_name2 = c("Winter barley", "Minor cereals", "Linseed", "Grass < 5", "Potatoes", "Other arable", "Oilseed Rape", "Rootcrops and brassicae", "Wheat"),
         Crop_name3 = c("Winter sown", "Winter sown", "Spring sown", "Grass", "Spring sown", "Winter sown", "Winter sown", "Spring sown", "Winter sown")) %>%
  left_join(Dat_fert %>% select(Crop_name2, Frate_mean = Nrate_mean), by = "Crop_name2") %>%
  left_join(Dat_man %>% select(Crop_name3, Mrate_mean), by = "Crop_name3") %>%
  mutate(Nrate = Frate_mean + Mrate_mean) %>%
  select(Crop, Nrate)

#################
# add data into main workflow and calculate new variables

# join up new data for N2O model to new model dataset
Dat_model <- Dat_main %>%
  select(x, y, Crop, Clay, OC, BD, pH, SOCchange_frac, Target_pH) %>%
  left_join(Dat_wetdays %>% select(x, y, Wet_days_mean), by = c("x", "y")) %>%
  left_join(Dat_temp, by =  c("x", "y")) %>%
  left_join(Dat_Nrate, by = "Crop")

# function to calculate OC percentage based on C (t/ha) and BD (kg / m2)
OC_perc <- function(BD_kg_m2, C_t_ha){
  C_kg_m2 <- C_t_ha * 10^3 * 10^-4 * 1 / 0.3
  Cfrac <- C_kg_m2 / BD_kg_m2
  Cperc <- Cfrac * 100
  return(Cperc)
}

# calculate required variables
Dat_model <- Dat_model %>%
  mutate(Is_grass = Crop == "Pasture",
         C_perc_initial = OC_perc(BD, OC),
         C_perc_final = C_perc_initial * SOCchange_frac)

#################
# Make predictions with N2O model

# model prepared in script [Hillier-Cornulier N2O model.R]
load(find_onedrive(dir = data_repo, path = "Jon H N2O paper/H-C N2O model lite.RData"))

# we need base + fertilised predictions for initial pH + optimal pH
# i.e. 4x predictions

# baseline N2O emissions, inital pH
ipH_base <- data.frame(Fert01 = 0, lowNO3 = 0, highNO3 = 0, Grasslands = Dat_model$Is_grass, 
                       pH = Dat_model$pH, Clay = Dat_model$Clay, SOC = Dat_model$C_perc_initial,
                       WetDays.exp = Dat_model$Wet_days_mean, DegDays.exp= Dat_model$Degree_days,
                       N.rate = Dat_model$Nrate, studyID.f= " 32.58 119.702007 8  8120082007-08-152007-11-04") # some random studyID.f (the first one)
ipH_base <- df.M15c.complete(ipH_base)
Dat_model <- Dat_model %>%
  mutate(ipH_base_pred = exp(predict(jamM15c, newdata = ipH_base)))

# fertiliser-induced N2O emissions, initial pH
ipH_fert <- data.frame(Fert01 = 1, lowNO3 = 0, highNO3 = 0, Grasslands = Dat_model$Is_grass, 
                       pH = Dat_model$pH, Clay = Dat_model$Clay, SOC = Dat_model$C_perc_initial,
                       WetDays.exp = Dat_model$Wet_days_mean, DegDays.exp= Dat_model$Degree_days,
                       N.rate = Dat_model$Nrate, studyID.f= " 32.58 119.702007 8  8120082007-08-152007-11-04") # some random studyID.f (the first one)
ipH_fert <- df.M15c.complete(ipH_fert)
Dat_model <- Dat_model %>%
  mutate(ipH_fert_pred = exp(predict(jamM15c, newdata = ipH_fert)))

# baseline N2O emissions, final pH
fpH_base <- data.frame(Fert01 = 0, lowNO3 = 0, highNO3 = 0, Grasslands = Dat_model$Is_grass, 
                       pH = Dat_model$Target_pH, Clay = Dat_model$Clay, SOC = Dat_model$C_perc_final,
                       WetDays.exp = Dat_model$Wet_days_mean, DegDays.exp= Dat_model$Degree_days,
                       N.rate = Dat_model$Nrate, studyID.f= " 32.58 119.702007 8  8120082007-08-152007-11-04") # some random studyID.f (the first one)
fpH_base <- df.M15c.complete(fpH_base)
Dat_model <- Dat_model %>%
  mutate(fpH_base_pred = exp(predict(jamM15c, newdata = fpH_base)))

# fertiliser-induced N2O emissions, final pH
fpH_fert <- data.frame(Fert01 = 1, lowNO3 = 0, highNO3 = 0, Grasslands = Dat_model$Is_grass, 
                       pH = Dat_model$Target_pH, Clay = Dat_model$Clay, SOC = Dat_model$C_perc_final,
                       WetDays.exp = Dat_model$Wet_days_mean, DegDays.exp= Dat_model$Degree_days,
                       N.rate = Dat_model$Nrate, studyID.f= " 32.58 119.702007 8  8120082007-08-152007-11-04") # some random studyID.f (the first one)
fpH_fert <- df.M15c.complete(fpH_fert)
Dat_model <- Dat_model %>%
  mutate(fpH_fert_pred = exp(predict(jamM15c, newdata = fpH_fert)))


# calculate EFs x 2 (initial + final pH)
Dat_model <- Dat_model %>%
  mutate(EF_initial = (ipH_fert_pred - ipH_base_pred) / Nrate,
         EF_final = (fpH_fert_pred - fpH_base_pred) / Nrate)

# calculate direct N2O emissions and difference in kg CO2-eq / ha
Dat_model <- Dat_model %>%
  mutate(iN2O = Nrate * EF_initial * 44/28,
         fN2O = Nrate * EF_final * 44/28,
         cN2O_CO2eq = (fN2O - iN2O) * 298)

qplot(Dat_model$cN2O_CO2eq)
mean(Dat_model$cN2O_CO2eq, na.rm = T)

#####################
# add N2O predictions to main data and scale

Dat_main <- Dat_main %>%
  mutate(GHGmit_N2O = abs(as.numeric(Dat_model$cN2O_CO2eq) * 10^-3))

# calculate GHG balance in tonnes / ha
Dat_main <- Dat_main %>%
  mutate(Tot_GHGmit = GHGmit_yield + GHGmit_SOC + GHGmit_N2O, # tonnes CO2-eq / ha
         Tot_GHG = Limeemb_GHG + Limedir_GHG + Dies_GHG, # tonnes CO2-eq / ha
         GHG_balance = Tot_GHG - Tot_GHGmit) # GHG balance (sources - sinks, tonnes CO2-eq / ha)

# calculate abatement and MAC
Dat_main <- Dat_main %>%
  mutate(Abatement = -GHG_balance * Area_ha, # abatement for full crop area in grid cell, tonnes CO2-eq
         Abatement_SOConly = -(Tot_GHG - GHGmit_SOC) * Area_ha,
         Abatement_EIonly = -(Tot_GHG - GHGmit_yield) * Area_ha,
         Abatement_N2Oonly = -(Tot_GHG - GHGmit_N2O) * Area_ha,
         Cost_net = Cost_net_ha * Area_ha,
         MAC = Cost_net / Abatement)

# save .RData for plots and decision tree model
save(Dat_main, UK, Dat_cdf, SOC_dat, file = "Full model output df.RData")
