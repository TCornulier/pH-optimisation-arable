# packages
library(tidyverse)
library(raster)
library(sp)
library(soiltexture)

# read in countries shapefile
# World_countries <- shapefile("GIS data/Country shapefile/countries.shp")

# subset to UK
# UK <- World_countries %>% subset(World_countries@data$ISO3=="GBR")
# UK@bbox <- matrix(data = c(-8, 49, 6, 61), nrow = 2, ncol = 2) # redefine bounding box so we don't get Gibraltar etc.!
# rm(World_countries)
# plot(UK)

UK <- shapefile("GIS data/DA shapefile/GBR_adm_shp/GBR_adm1.shp")

# read in soil raster data and stack
Soil_stack <- stack(raster("GIS data/SoilGrids 5km/Soil pH/Fixed/PHIHOX_M_sl4_5km_ll.tif"), # pH
                    raster("GIS data/SoilGrids 5km/Sand content/Fixed/SNDPPT_M_sl4_5km_ll.tif"), # Sand %
                    raster("GIS data/SoilGrids 5km/Silt content/Fixed/SLTPPT_M_sl4_5km_ll.tif"), # Silt %
                    raster("GIS data/SoilGrids 5km/Clay content/Fixed/CLYPPT_M_sl4_5km_ll.tif"), # Clay %
                    # raster("GIS data/SoilGrids 5km/OC density/Fixed/OCDENS_M_sl4_5km_ll.tif"), # OC kg per m3
                    # raster("GIS data/SoilGrids 5km/OC g per kg/Fixed/ORCDRC_M_sl4_5km_ll.tif"), # OC g per kg
                    raster("GIS data/SoilGrids 5km/OC tonnes per ha/Fixed/OCSTHA_M_sd4_5km_ll.tif")) # OC tonnes per ha

# read in crop area raster data and stack
readdir <- "GIS data/MapSPAM data/Physical area" # set to correct directory for conversions (ensure folder named "Fixed" is created to deposit transformed rasters into)
file.names <- dir(readdir, pattern =".tif")
# file.names <- file.names[file.names %>% str_detect("barley|bean|cereal|cowpea|fruit_temperate|maize|millet|oil_crops|potato|rapeseed|sorghum|sugar_beet|vegetable")] # not sexy but it'll slim things down a bit for now

Crop_area_stack <- raster::stack()
for(i in 1:length(file.names)){
  readpath <- paste(readdir, file.names[i], sep="/") # aggregate strings to create filepath
  x <- raster(readpath) # read raster
  Crop_area_stack <- addLayer(Crop_area_stack, x)
  rm(x)
  print(file.names[i])
}

# read in crop yield raster data and stack
readdir <- "GIS data/MapSPAM data/Yield"
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

# aggregate
Master_stack <- stack(Soil_stack, Crop_area_stack, Crop_yield_stack)
rm(Soil_stack, Crop_area_stack, Crop_yield_stack)

# mask out to UK only
Master_stack <- Master_stack %>% crop(UK) %>% mask(UK)
plot(Master_stack[[1]])
# plot(UK, add=T)

# convert to dataframe
Dat_main <- Master_stack %>% as.data.frame(xy = T) %>% drop_na(PHIHOX_M_sl4_5km_ll)
# we have zeros, >0s and NAs in the area data, NAs and >0s only in the yield data

Dat_main <- Dat_main %>%
  tbl_df %>%
  rename(x = x,
         y = y,
         pH = PHIHOX_M_sl4_5km_ll,
         Sand = SNDPPT_M_sl4_5km_ll,
         Silt = SLTPPT_M_sl4_5km_ll,
         Clay = CLYPPT_M_sl4_5km_ll,
         OC = OCSTHA_M_sd4_5km_ll,
         Cell_area_km2 = layer) %>%
  mutate(pH = pH / 10) # for some reason it's * 10 in original raster

glimpse(Dat_main)

# select out crops with zero area
isnt_zero <- function(x){
  y <- x == 0 | is.na(x)
  z <- F %in% y == T
  return(z)
}

Dat_main <- Dat_main %>%
  select_if(isnt_zero)

# gather up into crop types and metrics (area/yield)
# function to help rename crop variables
first_upper <- function(string){
  start <- str_sub(string, 1L, 1L) %>% str_to_upper()
  rest <- str_sub(string, 2L, -1L) %>% str_to_lower()
  whole <- paste0(start, rest)
  return(whole)
}

Dat_main <- Dat_main %>%
  gather(9:ncol(Dat_main), key = "key", value = "value") %>%
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
  group_by(pH) %>%
  summarise(Area_ha = sum(Area_ha)) %>%
  arrange(pH) %>%
  mutate(Area_cum = cumsum(Area_ha),
         Freq = Area_cum / max(Area_cum))

ggplot(Dat_cdf) +
  geom_line(aes(x = pH, y = Freq), colour = "darkred") +
  geom_line(data = Dat_cdf_av, aes(x = pH, y = Freq), colour = "grey", lty = 2) +
  facet_wrap(~Crop, nrow = 4) +
  labs(x = "pH", y = "Frequency") +
  theme_classic()
# ggsave("Output plots/pH CDFs, all crops.png", width = 8, height = 6)

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

Liming_factor <- function(Soil_type){
  Soil_type_search <- str_split(Soil_type, "\\W+") %>% unlist()
  LF_match <- DEFRA_LF[DEFRA_LF$Soil_type %in% Soil_type_search, ]
  LF_arable <- LF_match$Liming_factor_arable %>% mean()
  return(LF_arable)
}

Dat_main <- Dat_main %>%
  mutate(LF = sapply(Soil_type, Liming_factor))
Dat_main$Soil_type %>% table()

# match up yield response models to data
Dat_yieldres <- read_csv("Holland et al. (2019) yield curves full data.csv")

# function to find whether Rothamsted or Woburn has most similar soil type to grid cell (data based on Holland et al. (2019))
expt_selector <- function(sand, silt, clay){
  Roth <- c(28, 52, 20)
  Wob <- c(71, 17, 12)
  x <- (Roth - c(sand, silt, clay))^2 %>% sum()
  y <- (Wob - c(sand, silt, clay))^2 %>% sum()
  if(x <= y) z <- "Rothamsted"
  if(x > y) z <- "Woburn"
  return(z)
}

Dat_main <- Dat_main %>%
  mutate(Holland_exptmatch = pmap_chr(list(Sand, Silt, Clay), expt_selector))

# match crop types and import yield response parameters from Holland et al. (2019)
Dat_cropmatch <- tibble(Crop = Dat_main$Crop %>% unique(),
                        Holland_cropmatch = c("Spring barley", "Spring oats", NA, NA,
                                              "Linseed", "Potato", "Spring beans", "Winter oilseed rape",
                                              NA, NA, NA, "Winter wheat"))

Dat_yieldres <- Dat_yieldres %>%
  group_by(crop, site) %>%
  summarise_at(vars(a_est:d_est), funs(mean(.))) %>%
  dplyr::select(Holland_cropmatch = crop, Holland_exptmatch = site, a_est, b_est, d_est)

Dat_main <- Dat_main %>% 
  left_join(Dat_cropmatch, by = "Crop") %>%
  left_join(Dat_yieldres, by = c("Holland_cropmatch", "Holland_exptmatch"))

# calculate relative yields at given pH and possible yields at target pH
rel_yield <- function(A, B, D, pH){
  rel_yield <- A + (B / (1 + D * pH))
  rel_yield <- ifelse(rel_yield < 0.1, 0.1, rel_yield) # line added to avoid negative or divide by zero errors. Clunky... avoid in future?
  return(rel_yield)
}

target_pH <- function(pH, OC){
  is_peat <- (OC / 0.58) > 20 # assumes 20% threshold for peaty soil (as in RB209) and OM of 58% C
  is_below <- pH < 6.7 # target pH for continuous arable should be 6.7
  target_pH <- ifelse(is_peat == F & is_below == T, 6.7, pH)
  return(target_pH)
}

Dat_main <- Dat_main %>%
  mutate(Rel_yield = rel_yield(a_est, b_est, d_est, pH),
         Target_pH = target_pH(pH, OC / 10),
         Poss_yield = rel_yield(a_est, b_est, d_est, Target_pH),
         Yield_increase = Poss_yield / Rel_yield,
         pH_diff = Target_pH - pH) %>%
  dplyr::select(-(a_est:d_est)) %>%
  na.omit()

qplot(Dat_main$Yield_increase)

# load SOC response curve function derived in separate script and apply to main data
# function output is fractional
source("SOC RC function v2.R")

Dat_main <- Dat_main %>%
  mutate(SOCchange_frac = SOC_RC(ipH = pH, fpH = Target_pH))

# EI for different crops in g CO2-eq per kg, data from Feedprint for on-farm production only
# 'cereals, other' classed as oats, 'oil crops. other' classed as linseed, 'pulses, other' classed as beans
Dat_EI <- tibble(Crop = Dat_main %>% pull(Crop) %>% unique(),
                 EI = c(343, 465, 1222, 226, 766, 984, 349))

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

# calculate emissions from lime application and mitigation balance
Dat_main <- Dat_main %>%
  mutate(Limerate = (LF * (Target_pH + 0.2 - pH)) / 5, # Assuming a 5 year interval between applications (based on a variety of data sources)
         Limeemb_GHG = 0.074 * Limerate, # Kool et al. (2012)
         Limedir_GHG = 0.125 * 44/12 * Limerate, # IPCC (2006)
         Dies_GHG = (336 * 0.7) / 36.9 * 3.165 * 10^-3, # Williams et al (2006) for diesel usage * DEFRA/DECC for EF
         
         Tot_GHGmit = GHGmit_yield + GHGmit_SOC,
         Tot_GHG = Limeemb_GHG + Limedir_GHG + Dies_GHG,
         GHG_balance = Tot_GHG - Tot_GHGmit)

# sale values for different crops from FMH 17/18, all in 2017 GBP
# linseed uses OSR values, potatoes assumes dual purpose and price is weighted according to relative yields
Dat_saleval <- tibble(Crop = Dat_main %>% pull(Crop) %>% unique(),
                      Maincrop_saleval = c(145, 155, 325, 113, 200, 325, 165),
                      Bycrop_saleval = c(55, 50, 0, 0, 0, 0, 50), # secondary crop e.g. straw
                      Bycrop_ratio = c(0.55, 0.60, 0, 0, 0, 0, 0.53)) # ratio of secondary crop to main crop yield

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

# calculate abatement and MAC
Dat_main <- Dat_main %>%
  mutate(Abatement = -GHG_balance * Area_ha,
         Abatement_SOConly = -(Tot_GHG - GHGmit_SOC) * Area_ha,
         Abatement_EIonly = -(Tot_GHG - GHGmit_yield) * Area_ha,
         Cost_net = Cost_net_ha * Area_ha,
         MAC = Cost_net / Abatement)

# read in DA shapefiles and process so we can make plots at DA level
DAs <- shapefile("GIS data/DA shapefile/GBR_adm_shp/GBR_adm1.shp")
# ggplot() + geom_polygon(data = DAs, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + coord_quickmap + theme_void()
template <- Master_stack[[1]]
England <- template %>% mask(subset(DAs, DAs@data[["NAME_1"]]=="England"))
Northern_Ireland <- template %>% mask(subset(DAs, DAs@data[["NAME_1"]]=="Northern Ireland"))
Scotland <- template %>% mask(subset(DAs, DAs@data[["NAME_1"]]=="Scotland"))
Wales <- template %>% mask(subset(DAs, DAs@data[["NAME_1"]]=="Wales"))

Eng_df <- England %>% as.data.frame(xy = T) %>% drop_na(PHIHOX_M_sl4_5km_ll)
NI_df <- Northern_Ireland %>% as.data.frame(xy = T) %>% drop_na(PHIHOX_M_sl4_5km_ll)
Scot_df <- Scotland %>% as.data.frame(xy = T) %>% drop_na(PHIHOX_M_sl4_5km_ll)
Wales_df <- Wales %>% as.data.frame(xy = T) %>% drop_na(PHIHOX_M_sl4_5km_ll)

DA_dat <- bind_rows(list(England = Eng_df, `Northern Ireland` = NI_df, Scotland = Scot_df, Wales = Wales_df), .id = "DA")  %>%
  dplyr::select(-PHIHOX_M_sl4_5km_ll)

Dat_main <- left_join(Dat_main, DA_dat, by = c("x", "y"))


