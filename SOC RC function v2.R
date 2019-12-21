
# script to derive pH -> SOC response curve function for use in modelling studies
library(tidyverse)

data_repo <- "DEFRA Clean Growth Project/pH Optimisation/Extension for publication"

set.seed(2605)
sim_n = round(10^3 / 4, 0)
Roth_dat <- tibble(pH = c(rnorm(n = sim_n, mean = 3.45, sd = 0.01),
                          rnorm(n = sim_n, mean = 4.58, sd = 0.31),
                          rnorm(n = sim_n, mean = 5.72, sd = 0),
                          rnorm(n = sim_n, mean = 6.19, sd = 0.23)),
                   SOC = c(rnorm(n = sim_n, mean = 8.5, sd = 0.1),
                           rnorm(n = sim_n, mean = 10.4, sd = 0.1),
                           rnorm(n = sim_n, mean = 11.2, sd = 0.1),
                           rnorm(n = sim_n, mean = 10.7, sd = 0.1))
)

sim_n <- round(10^3 / 4, 0)
Wob_dat <- tibble(pH = c(rnorm(n = sim_n, mean = 3.67, sd = 0.18),
                         rnorm(n = sim_n, mean = 4.59, sd = 0.2),
                         rnorm(n = sim_n, mean = 5.37, sd = 0.13),
                         rnorm(n = sim_n, mean = 6.1, sd = 0.06)),
                  SOC = c(rnorm(n = sim_n, mean = 6.8, sd = 0.1),
                          rnorm(n = sim_n, mean = 7.5, sd = 0.1),
                          rnorm(n = sim_n, mean = 7.7, sd = 0.1),
                          rnorm(n = sim_n, mean = 7.6, sd = 0.1)))

sim_n <- round(10^3 / 13, 0)
Tu_dat <- tibble(pH = c(rnorm(n = sim_n, mean = 6.46, sd = 0.59),
                        rnorm(n = sim_n, mean = 6.09, sd = 0.33),
                        rnorm(n = sim_n, mean = 6.78, sd = 0.57),
                        rnorm(n = sim_n, mean = 7.07, sd = 0.47),
                        rnorm(n = sim_n, mean = 7.01, sd = 0.46),
                        rnorm(n = sim_n, mean = 6.99, sd = 0.40),
                        rnorm(n = sim_n, mean = 6.89, sd = 0.35),
                        rnorm(n = sim_n, mean = 6.85, sd = 0.34),
                        rnorm(n = sim_n, mean = 6.88, sd = 0.31),
                        rnorm(n = sim_n, mean = 6.97, sd = 0.33),
                        rnorm(n = sim_n, mean = 6.91, sd = 0.27),
                        rnorm(n = sim_n, mean = 6.89, sd = 0.27),
                        rnorm(n = sim_n, mean = 7.07, sd = 0.40)),
                 SOC = c(rnorm(n = sim_n, mean = 14.71, sd = 3.45),
                         rnorm(n = sim_n, mean = 14.51, sd = 2.29),
                         rnorm(n = sim_n, mean = 14.87, sd = 3.80),
                         rnorm(n = sim_n, mean = 21.18, sd = 4.88),
                         rnorm(n = sim_n, mean = 23.70, sd = 5.94),
                         rnorm(n = sim_n, mean = 24.38, sd = 5.14),
                         rnorm(n = sim_n, mean = 23.66, sd = 3.97),
                         rnorm(n = sim_n, mean = 24.76, sd = 4.12),
                         rnorm(n = sim_n, mean = 24.93, sd = 4.15),
                         rnorm(n = sim_n, mean = 24.90, sd = 4.21),
                         rnorm(n = sim_n, mean = 24.06, sd = 2.62),
                         rnorm(n = sim_n, mean = 25.78, sd = 1.96),
                         rnorm(n = sim_n, mean = 27.95, sd = 3.26)))

#ggplot(Wob_dat, aes(x = pH, y = SOC)) +
#  geom_point(colour = "darkred", alpha = 0.5) +
#  geom_smooth(method = "loess", span = 0.9)

#ggplot(Roth_dat, aes(x = pH, y = SOC)) +
#  geom_point(colour = "darkred", alpha = 0.5) +
#  geom_smooth(method = "loess", span = 0.9)

#ggplot(Tu_dat, aes(x = pH, y = SOC)) +
#  geom_point(colour = "darkred", alpha = 0.5) +
#  geom_smooth(method = "loess", span = 0.9)

SOC_dat <- bind_rows(list(Rothamsted = Roth_dat, Woburn = Wob_dat, Tu = Tu_dat), .id = "Experiment")

SOC_dat <- SOC_dat %>%
  mutate(SOC_av = mean(SOC),
         SOC_sd = sd(SOC),
         pH_av = mean(pH),
         pH_sd = sd(pH)) %>%
  group_by(Experiment) %>%
  mutate(SOC_groupav = mean(SOC),
         SOC_groupsd = sd(SOC),
         pH_groupav = mean(pH),
         pH_groupsd = sd(pH),
         SOC_std = (SOC - SOC_groupav) / SOC_groupsd,
         pH_std = (pH - pH_groupav) / pH_groupsd) %>%
  ungroup() %>%
  mutate(SOC_std2 = (SOC_std * SOC_sd) + SOC_av,
         pH_std2 = (pH_std * pH_sd) + pH_av) %>%
  sample_n(nrow(SOC_dat), replace = F)

mean(SOC_dat$SOC_std2)

#ggplot(SOC_dat) +
#  geom_point(aes(x = pH_std2, y = SOC_std2, colour = Experiment), alpha = 0.1) +
#  geom_smooth(aes(x = pH_std2, y = SOC_std2, colour = Experiment), method = "loess", span = 1, lty = 2, size = 0.5, colour = "black", se = T) +
#  scale_x_continuous(breaks = 3:8) +
#  xlim(c(3, 8)) +
#  #ylim(c(0, 30)) +
#  labs(x = "pH, standardised",
#       y = expression("SOC stocks (g kg"^{-1}*"), standardised")) +
#  theme_classic()
#ggsave(find_onedrive(dir = data_repo, path = "Output plots/SOC model plot.png"), width = 8, height = 5)

#ggplot(SOC_dat %>% filter(Experiment != "Tu")) +
#  geom_point(aes(x = pH_std2, y = SOC_std2, colour = Experiment), alpha = 0.1) +
#  geom_smooth(aes(x = pH_std2, y = SOC_std2, colour = Experiment), method = "loess", span = 1, lty = 2, size = 0.5, colour = "black", se = T) +
#  scale_x_continuous(breaks = 3:8) +
#  xlim(c(3, 8)) +
#  ylim(c(0, 30)) +
#  labs(x = "pH, standardised",
#       y = expression("SOC stocks (g kg"^{-1}*"), standardised")) +
#  theme_classic()
#ggsave(find_onedrive(dir = data_repo, "Output plots/SOC model plot.png"), width = 8, height = 4)

SOC_RC_model <- loess(SOC_std2 ~ pH_std2, data = SOC_dat %>% filter(Experiment != "Tu"), span = 1)

SOC_RC <- function(ipH, fpH){
  iSOC <- predict(SOC_RC_model, newdata = ipH)
  fSOC <- predict(SOC_RC_model, newdata = fpH)
  cSOC <- fSOC / iSOC
  return(cSOC)
}

# added to include a SOC response for grass
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
C_response_fac_grass <- (as.numeric(treat_model$coefficients[2] - cont_model$coefficients[2]) / mean(c(Dat_grass$control[1], Dat_grass$treatment[1]))) / (Grass_pH$pH[2] - Grass_pH$pH[1])

# tidy up
rm(cont_model,
   Dat_grass,
   Grass_pH,
   Roth_dat,
   SOC_dat,
   treat_model,
   Tu_dat,
   Wob_dat,
   sim_n)
