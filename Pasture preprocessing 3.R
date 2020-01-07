library(raster)
library(tidyverse)
library(sp)

bigdata_repo <- "GIS data repository"
output_repo <- "DEFRA Clean Growth Project/pH Optimisation/Extension for publication/Output plots"

Pasture_area <- find_onedrive(dir = bigdata_repo, path = "Created rasters/UK-pasture-area-10km-CLC-based-WGS84.tif") %>% raster()

# read in elevation at 30-arc-secs from GMTED2010
Elev <- find_onedrive(dir = bigdata_repo, path = "Created rasters/UK-dem-GMTED2010-30-arc-secs-wgs84-mean-elev-m.tif") %>% raster()

# FAO slope data at 5-arc-mins
file_names <- dir(find_onedrive(dir = bigdata_repo, path = "FAO slope"), pattern = ".tif$")
Slope <- raster::stack()
for(i in 1:length(file_names)){
  x <- find_onedrive(dir = paste(bigdata_repo, "FAO slope", sep = "/"),  path = file_names[i]) %>% raster()
  Slope <- stack(Slope, x)
}

# masking shapefile
Shp_UK <- find_onedrive(dir = bigdata_repo, path = "DA shapefile/GBR_adm_shp/GBR_adm0.shp") %>% shapefile()

# crop and mask slopes data
Slope <- Slope %>% crop(Shp_UK) %>% mask(Shp_UK)

# slope values are (nonsensically) "100 percentages" i.e. 100x the percentage of area in a given cell with
# slopes in the defined range. Divide by 10^4 to get fractional area
Slope <- Slope * 10^-4

# stop NA values causing calculation problems later
Slope[is.na(Slope)] <- 0

# ALC 1988 states that land of up to 11 degrees is suitable for agricultural equipment
# equivalent to layers 1:4 + 1/5 of layer 5, assuming uniform distribution
Slope_workable <- Slope[[1]] + Slope[[2]] + Slope[[3]] + Slope[[4]] + Slope[[5]] * 0.2

# how much of that workable slope corresponds to pasture?
Slope_workable <- Slope_workable %>% extend(Pasture_area)
Pasture_workable <- Pasture_area * Slope_workable
plot(Pasture_workable)
cellStats(Pasture_workable, sum) / cellStats(Pasture_area, sum)

# that's it for pasture on workable slopes. Now to filter out anything which is in the uplands (> 200m)

# create a binary raster for lowland areas (<=200m)
Lowland <- Elev
Lowland[Elev <= 200] <- 1
Lowland[Elev > 200] <- 0
Lowland %>% plot()

# resample as fractional area to resolution of pasture area raster
Lowland_ag <- Lowland %>% resample(Pasture_area)
Lowland_ag %>% plot()

# multiply by workable pasture raster to define workable + lowland areas
Pasture_workable_lowland <- Pasture_workable * Lowland_ag
cellStats(Pasture_workable_lowland, sum) / cellStats(Pasture_area, sum)
Pasture_workable_lowland %>% plot()

# write out created raster
writeRaster(Pasture_workable_lowland, find_onedrive(dir = bigdata_repo, path = "Created rasters/UK-pasture-area-10km-CLC-based-WGS84-lowland-workable.tif"))

# plot for paper
Dat_main <- stack(Pasture_area, Pasture_workable_lowland, area(Pasture_area)) %>%
  mask(Shp_UK) %>%
  as.data.frame(xy = T) %>%
  as_tibble() %>%
  drop_na()

colnames(Dat_main) <- c("x", "y", "Total_pasture_area", "Workable_lowland_pasture_area", "Cell_area")

Dat_main %>%
  mutate_at(vars(Total_pasture_area:Workable_lowland_pasture_area), funs(. / Cell_area * 100)) %>%
  select(-Cell_area) %>%
  gather(-x, -y, key = "Area_type", value = "Area_pc") %>%
  mutate(Area_type = Area_type %>%
           str_replace_all("_", " ") %>%
           str_replace_all(" pasture", "\npasture")) %>%
  ggplot() +
  geom_raster(aes(x = x, y = y, fill = Area_pc)) +
  facet_wrap(~Area_type, nrow = 1) +
  geom_polygon(data = Shp_UK, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5) +
  xlim(c(-10, 5)) +
  ylim(c(49, 62)) +
  scale_fill_gradient(low = "lightgrey", high = "darkgreen") +
  labs(fill = "Area\n(% of cell)") +
  coord_quickmap() +
  theme_void()
ggsave(find_onedrive(dir = output_repo, path = "Pasture area comparison map.png"), width = 8, height = 7)

# descriptives for paper
d1 <- Dat_main %>% pull(Total_pasture_area) %>% sum() * 10^2 * 10^-3 # total area, in '000 ha
d1
d2 <- Dat_main %>% pull(Workable_lowland_pasture_area) %>% sum() * 10^2 * 10^-3 # workable area, in '000 ha
d2
d2 /d1 # fraction workable


