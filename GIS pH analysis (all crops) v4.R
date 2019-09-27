# set wd 
setwd("~/Documents/SRUC/DEFRA Clean Growth Project/pH Optimisation/Extension for publication")

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


# plot plot plot

# abatement map for UK
Dat_summ1 <- Dat_main %>%
  group_by(x, y) %>%
  summarise(Abatement = sum(Abatement)) %>%
  mutate(Abatement_fac = cut(Abatement,
                             breaks = c(-10000, -1000, -100, -10, 10, 100, 1000, 100000),
                             labels = c("Net emissions, > 1 kilotonne",
                                        "Net emissions, > 100 tonnes",
                                        "Net emissions, > 10 tonnes",
                                        "Net neutral effect",
                                        "Net abatement, > 10 tonnes",
                                        "Net abatement, > 100 tonnes",
                                        "Net abatement, > 1 kilotonne")))

levels(Dat_summ1$Abatement_fac)
qplot(Dat_summ1$Abatement_fac)

Dat_summ1 %>% filter(is.na(Abatement_fac)) %>% nrow()

ggplot() +
  geom_raster(data = Dat_summ1 %>% mutate(Abatement = ifelse(Abatement > 2500, 2500, Abatement)), aes(x = x, y = y, fill = Abatement)) +
  #geom_polygon(data = UK, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5) +
  xlim(c(-10, 5)) +
  ylim(c(49, 62)) +
  scale_fill_gradient2(low = "darkred", mid = "lightgrey", high = "darkgreen") +
  labs(fill = "Abatement\npotential\n(tonnes)") +
  coord_quickmap() +
  theme_void()
# ggsave("Output plots/Abatement map UK.png", width = 8, height = 7)

ggplot() +
  geom_raster(data = Dat_summ1, aes(x = x, y = y, fill = Abatement_fac), alpha = 0.7) +
  #geom_polygon(data = UK, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5) +
  scale_fill_brewer(type = "div", palette = "RdBu") +
  labs(fill = expression("AP (CO"[2]*"-eq)")) +
  coord_quickmap() +
  theme_void()
# ggsave("Output plots/Abatement map UK discrete.png", width = 8, height = 7)

# abatement map per hectare for UK
Dat_main %>%
  group_by(x, y) %>%
  summarise(Abatement = sum(Abatement),
            Area_ha = sum(Area_ha),
            Abatement_ha = Abatement / Area_ha) %>%
  ggplot() +
  geom_raster(aes(x = x, y = y, fill = Abatement_ha), alpha = 0.7) +
  geom_polygon(data = UK, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5) +
  scale_fill_gradient2(low = "darkred", mid = "lightgrey", high = "darkgreen") +
  labs(fill = expression("Abatement\npotential\n(tonnes CO"[2]*"-eq ha"^{-1}*")")) +
  coord_quickmap() +
  theme_void()
#ggsave("Output plots/Abatement map UK per ha.png", width = 8, height = 7)

# MAC map for UK
Dat_summ2 <- Dat_main %>%
  filter(GHG_balance <= -0.1) %>% # only measures with reliable mitigation (i.e. < 100 kg CO2-eq / ha / year) included
  group_by(x, y) %>%
  summarise(Tot_cost = sum(Cost_net),
            Abatement = sum(Abatement)) %>%
  ungroup() %>%
  mutate(MAC = Tot_cost / Abatement) %>%
  filter(MAC >= quantile(MAC, 0.05),
         MAC <= quantile(MAC, 0.95)) # filter out divide-by-nearly-zero mad numbers
qplot(Dat_summ2$MAC)

Dat_summ2$MAC %>% summary()

ggplot() +
  geom_raster(data = Dat_summ2, aes(x = x, y = y, fill = MAC)) +
  #geom_polygon(data = UK, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5) +
  #scale_fill_gradient(high = "red", low = "darkgreen") +
  labs(fill = expression('Marginal\nabatement\ncost\n(£ tonne CO'[2]*'-eq'^{-1}*')')) +
  coord_quickmap() +
  theme_void()
# ggsave("Output plots/MAC map UK.png", width = 8, height = 7)

# crop map for UK
Dat_summ3 <- Dat_main %>%
  group_by(x, y) %>%
  mutate(Dom_crop = Area_ha == max(Area_ha)) %>%
  filter(Dom_crop == T) %>%
  dplyr::select(x, y, Crop)

Dat_summ3$Crop[834] <- "Oil crops, other" # cheeky hack to stop this getting dropped altogether - now the colour palette matches the MACCs

qplot(Dat_summ3$Crop)
ggplot() +
  geom_raster(data = Dat_summ3, aes(x = x, y = y, fill = Crop), alpha = 0.7) +
  #geom_polygon(data = UK, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5) +
  scale_fill_brewer(type = "qual", palette = "Set3") +
  coord_quickmap() +
  theme_void()
# ggsave("Output plots/Dominant arable crop map UK.png", width = 8, height = 7)

# how much land area is lost by removing unmatched crops?
Dat_cdf %>%
  filter(Crop %in% Dat_main$Crop == F) %>%
  group_by(Crop) %>%
  summarise(Area_kha = sum(Area_ha) * 10^-3)

# UK full MACC
Dat_main %>%
  filter(GHG_balance <= -0.1) %>% # only measures with reliable mitigation (i.e. < 100 kg CO2-eq / ha / year) included
  dplyr::select(MAC, Abatement, Crop) %>%
  filter(MAC >= quantile(MAC, 0.05),
         MAC <= quantile(MAC, 0.95)) %>%
  arrange(MAC) %>%
  mutate(Abatement = Abatement * 10^-3,
         xmax = cumsum(Abatement),
         xmin = lag(xmax, default = 0),
         ymin = ifelse(MAC < 0, MAC, 0),
         ymax = ifelse(MAC > 0, MAC, 0),
         xav = (xmin + xmax) / 2) %>%
  ggplot() +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Crop), colour = NA, alpha = 0.9) +
  scale_fill_brewer(type="qual", palette="Set3", guide=guide_legend(title = NULL)) +
  labs(x = expression('Abatement potential (kt CO'[2]*'eq year'^{-1}*')'),
       y = expression('Marginal abatement cost (GBP tonne CO'[2]*'-eq'^{-1}*')')) +
  theme_classic()
# ggsave("Output plots/UK full MACC.png", width = 8, height = 5)

# devolved administration MACCs
Dat_main %>%
  filter(GHG_balance <= -0.1) %>% # only measures with reliable mitigation (i.e. < 100 kg CO2-eq / ha / year) included
  dplyr::select(MAC, Abatement, Crop, DA) %>%
  filter(MAC >= quantile(MAC, 0.05),
         MAC <= quantile(MAC, 0.95)) %>%
  group_by(DA) %>%
  arrange(MAC) %>%
  mutate(Abatement = Abatement * 10^-3,
         xmax = cumsum(Abatement),
         xmin = lag(xmax, default = 0),
         ymin = ifelse(MAC < 0, MAC, 0),
         ymax = ifelse(MAC > 0, MAC, 0),
         xav = (xmin + xmax) / 2) %>%
  ggplot() +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Crop), colour = NA, alpha = 0.9) +
  scale_fill_brewer(type="qual", palette="Set3", guide=guide_legend(title = NULL)) +
  labs(x = expression('Abatement potential (kt CO'[2]*'eq year'^{-1}*')'),
       y = expression('Marginal abatement cost (GBP tonne CO'[2]*'-eq'^{-1}*')')) +
  facet_wrap(~DA, nrow = 2) +
  theme_classic()
# ggsave("Output plots/DA MACCs.png", width = 8, height = 5)

# table 1 (defra liming factors)
DEFRA_LF %>%
  gather(-Soil_type, key = `Land use`, value = `Liming factor`) %>%
  spread(key = Soil_type, value = `Liming factor`) %>%
  mutate(`Land use` = `Land use` %>% str_replace("Liming_factor_", "") %>% first_upper()) %>%
  write_csv("Output plots/Table 1 (Defra_LFs).csv")

# table 1 caption
tibble(abbrev = DEFRA_LF$Soil_type) %>%
  mutate(full1 = abbrev %>%
           str_to_lower() %>%
           str_replace_all("cl", "clay ") %>%
           str_replace_all("sa", "sand ") %>%
           str_replace_all("si", "silt ") %>%
           str_replace_all("lo", "loam ") %>%
           str_replace_all(" $", "") %>%
           str_replace(" ", "y ") %>%
           str_replace_all("clayy", "clayey") %>%
           first_upper(),
         full2 = paste(abbrev, full1, sep = " = ")) %>%
  pull(full2) %>%
  str_c(collapse = "; ")

# yield increase boxplot
Dat_main %>% ggplot(aes(x = Crop, y = Yield_increase, fill = DA)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_brewer(palette = "Set3") +
  ylim(c(1, 1.8)) +
  labs(x = "", y = "Limed yield (fractional)", fill = "") +
  coord_flip() +
  theme_classic()
# ggsave("Output plots/Fractional yield increase.png", width = 8, height = 4)

# yield increase columns
order <- Dat_main %>%
  filter(GHG_balance <= -0.1) %>%
  group_by(Crop) %>%
  mutate(Yield_inc_tha = Yield_tha * (Yield_increase - 1)) %>%
  summarise(Yield_inc_t = sum(Yield_inc_tha)) %>%
  arrange(Yield_inc_t) %>%
  pull(Crop)

Dat_main %>%
  filter(GHG_balance <= -0.1) %>%
  mutate(Yield_inc_ktha = Yield_tha * (Yield_increase - 1) * 10^-3) %>%
  group_by(Crop, DA) %>%
  summarise(Yield_inc_kt = sum(Yield_inc_ktha)) %>%
  ungroup() %>%
  mutate(Crop = factor(Crop, levels = order)) %>%
  ggplot(aes(x = Crop, y = Yield_inc_kt)) +
  geom_col(aes(fill = DA), position = position_stack()) +
  labs(x = "", y = "Additional crop production (kt)", fill = "") +
  scale_fill_brewer(palette = "Set3") +
  coord_flip() +
  theme_classic()
# ggsave("Output plots/Additional crop production.png", width = 8, height = 3)

# table 2
write_csv(Dat_saleval %>% dplyr::select(-Bycrop_ratio), "Output plots/Table 2.csv")

# results descriptives
d1 <- Dat_main %>% pull(Area_ha) %>% sum() * 10^-3 # total area (kha)
print(d1)

d2 <- Dat_main %>% filter(GHG_balance <= -0.1) %>% pull(Area_ha) %>% sum() * 10^-3 # area with abatement
print(d2)

d2 / d1 # area with abatement
Dat_main %>% filter(GHG_balance <= -0.1) %>% pull(Limerate) %>% mean() # average 5-year lime rate
Dat_main %>% filter(GHG_balance <= -0.1) %>% pull(Abatement) %>% sum() * 10^-3 # abatement in kt

# overall yield increase in kt
Dat_main %>%
  mutate(Yield_inc_ktha = Yield_tha * (Yield_increase - 1) * 10^-3,
         Yield_inc_kt = Yield_inc_ktha * Area_ha) %>%
  pull(Yield_inc_kt) %>%
  sum()
# equal to about 362 kg / ha over ENTIRE area


# yield increase in kt only in areas where net abatement is possible
Dat_main %>%
  filter(GHG_balance <= -0.1) %>%
  mutate(Yield_inc_ktha = Yield_tha * (Yield_increase - 1) * 10^-3,
         Yield_inc_kt = Yield_inc_ktha * Area_ha) %>%
  pull(Yield_inc_kt) %>%
  sum()

# emissions + abatement
Dat_main %>% filter(GHG_balance <= -0.1) %>% mutate(GHGmit_SOC = GHGmit_SOC * Area_ha) %>% pull(GHGmit_SOC) %>% sum() * 10^-3 # SCS total
Dat_main %>% filter(GHG_balance <= -0.1) %>% mutate(GHGmit_yield = GHGmit_yield * Area_ha) %>% pull(GHGmit_yield) %>% sum() * 10^-3 # EI reduction total
Dat_main %>% filter(GHG_balance <= -0.1) %>% mutate(Tot_GHG = Tot_GHG * Area_ha) %>% pull(Tot_GHG) %>% sum() * 10^-3 # GHG emissions total
Dat_main %>% filter(GHG_balance <= -0.1) %>% mutate(Limedir_GHG = Limedir_GHG * Area_ha) %>% pull(Limedir_GHG) %>% sum() * 10^-3 # direct lime emissions total
Dat_main %>% filter(GHG_balance <= -0.1) %>% mutate(GHGmit_SOC = GHGmit_SOC / Area_ha) %>% pull(GHGmit_SOC) %>% mean()

# area between Hamilton's magic 5—6.7 bracket
d3 <- Dat_main %>% pull(Area_ha) %>% sum() # total area
d4 <- Dat_main %>% filter(pH >= 5, pH <= 6.7) %>% pull(Area_ha) %>% sum()
d4
d4/d3

# costs and benefits
d5 <- Dat_main %>% filter(GHG_balance <= -0.1) %>% mutate(x = Area_ha * (Lime_cost + Cont_cost)) %>% pull(x) %>% sum()
d5 / (d2 * 10^3)
d6 <- Dat_main %>% filter(GHG_balance <= -0.1) %>% mutate(x = Area_ha * Crop_revenue_net) %>% pull(x) %>% sum()
d6 / (d2 * 10^3)

d7 <- Dat_main %>% filter(GHG_balance <= -0.1) %>% filter(MAC >= quantile(MAC, 0.05), MAC <= quantile(MAC, 0.95)) %>% mutate(x = Area_ha * MAC) %>% pull(x) %>% sum()
d7 / (d2 * 10^3)

Dat_main %>% filter(GHG_balance <= -0.1) %>% filter(MAC >= quantile(MAC, 0.05), MAC <= quantile(MAC, 0.95)) %>% pull(MAC) %>% quantile(c(0.025, 0.975))

d8 <- Dat_main %>% filter(GHG_balance <= -0.1) %>% filter(MAC >= quantile(MAC, 0.05), MAC <= quantile(MAC, 0.95)) %>% nrow()
d9 <- Dat_main %>% filter(GHG_balance <= -0.1) %>% filter(MAC >= quantile(MAC, 0.05), MAC <= quantile(MAC, 0.95)) %>% filter(MAC <= 66.1) %>% nrow()
d9/d8

Dat_main %>%
  filter(GHG_balance <= -0.1) %>%
  filter(MAC <= 66.1) %>%
  mutate(abatement_frac = Abatement / sum(Abatement)) %>%
  group_by(DA) %>%
  summarise(abatement_frac = sum(abatement_frac))

# bar plot showing abatement crops/DAs
order <- Dat_main %>%
  filter(GHG_balance <= -0.1) %>%
  group_by(Crop) %>%
  summarise(Area_ha = sum(Area_ha)) %>%
  arrange(Area_ha) %>%
  pull(Crop)

Dat_main %>%
  filter(GHG_balance <= -0.1) %>%
  group_by(DA, Crop) %>%
  summarise(Area_ha = sum(Area_ha) * 10^-3) %>%
  mutate(Crop = factor(Crop, levels = order)) %>%
  ggplot(aes(x = Crop, y = Area_ha, fill = DA)) +
  geom_col(position = position_stack()) +
  scale_fill_brewer(palette = "Set3") +
  labs(x = "",
       y = "Area with net abatement available ('000 ha)",
       fill = "") +
  coord_flip() +
  theme_classic()
# ggsave("Output plots/Headline area x crop x DA.png", width = 8, height = 3)
         