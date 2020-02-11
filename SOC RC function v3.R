# script to derive pH -> SOC response curve function for use in modelling studies
library(tidyverse)

data_repo <- "DEFRA Clean Growth Project/pH Optimisation/Extension for publication"

#####################################
# SOC responses to liming in cropland
#####################################
# data from Paradelo et al. (2015) for cropland
# limited to temperate climates, long term trials (>= 20 years)
# original data sources = Simek et al. (1999) + C. Chenu (pers. comm.)
Dat_crop <- tibble(Cont_pH = c(5.3, 5.5, 5.0, 4.9, 5.8, 5.8),
                   Treat_pH = c(6.1, 6.3, 6.4, 6.2, 8.5, 8.7),
                   Cont_SOC = c(10.5, 9.4, 11.4, 10.9, 5.7, 5.7),
                   Treat_SOC = c(11, 10.4, 11.8, 12, 6.7, 6.1))

Dat_crop <- Dat_crop %>%
  mutate(pH_diff = Treat_pH - Cont_pH,
         pH_av = (Treat_pH + Cont_pH) / 2,
         SOC_RR = (Treat_SOC / Cont_SOC) - 1,
         SOC_RR_unit = SOC_RR / pH_diff)

# wider pH differences = lower SOC response per unit increase
# appears to be that response curve flattens off at ~ pH 6-7 - see Kemmit et al. (2006)
# too bad we can't model it directly
Dat_crop %>%
  ggplot(aes(x = pH_diff, y = SOC_RR_unit)) +
  geom_point() +
  geom_smooth(method = "lm")

# linear model to capture changes in SOC response ratio
SOC_RR_model <- lm(SOC_RR_unit ~ pH_diff, data = Dat_crop)

# example plot
example <- tibble(pH_diff = seq(from = 0.1, to = 3, length.out = 30))
example %>%
  mutate(SOC_RR_unit = predict(SOC_RR_model, newdata = example),
         SOC_RR = SOC_RR_unit * pH_diff + 1) %>%
  ggplot(aes(x = pH_diff, y = SOC_RR)) +
  geom_line()

# final function
Crop_SOC_RR_year <- function(pH_diff){
  SOC_RR_unit <- predict(SOC_RR_model, newdata = data.frame(pH_diff = pH_diff))
  SOC_RR_year <- (SOC_RR_unit * pH_diff) / 20
  return(SOC_RR_year)
}

#######################################
# added to include a SOC response for grass
#######################################
Grass_pH <- read_csv(find_onedrive(dir = data_repo, path = "Fornara final pH data.csv"))
Dat_grass <- read_csv(find_onedrive(dir = data_repo, path = "Fornara soil C data raw.csv"))

#Dat_grass %>%
#  gather(-year, key = type, value = soil_C) %>%
#  ggplot(aes(x = year, y = soil_C, colour = type, group = type)) +
#  geom_line() +
#  geom_smooth(method = "lm", se = F)

# linear models for both
treat_model <- lm(treatment ~ year, data = Dat_grass)
cont_model <- lm(control ~ year, data = Dat_grass)

summary(treat_model) # p < .05
summary(cont_model) # ns, which is good — variation comes from other factors

# subtract slopes to control for (ns but slight) difference in starting stocks and determine annual response
# divide by mean starting stocks to give fractional response
# divide by pH difference to give fractional C response per unit pH change
Grass_SOC_RR_year <- (as.numeric(treat_model$coefficients[2] - cont_model$coefficients[2]) / mean(c(Dat_grass$control[1], Dat_grass$treatment[1]))) / (Grass_pH$pH[2] - Grass_pH$pH[1])

# write out final functions/factors for use in main model
save(Crop_SOC_RR_year, Grass_SOC_RR_year, file = "SOC response functions.RData")
