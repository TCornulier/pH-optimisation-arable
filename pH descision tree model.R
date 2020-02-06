# script to be run following main model script [GIS pH analysis (all crops) v4.R]

library(fastDummies)
library(rpart)

# load workspace
load("Full model output df.RData")

# drop out that one row where N2O model misfired
Dat_main <- Dat_main %>% filter(!is.na(Abatement))

# pull out primary data and classifier for simplified decision tree model
Dat_model <- Dat_main %>%
  mutate(has_abatement = as.numeric(GHG_balance <= -0.1),
         is_cost_effective = as.numeric(MAC <= 66.1),
         #is_cost_effective = as.numeric(MAC <= 0),
         has_ce_abatement = as.numeric(has_abatement + is_cost_effective == 2)) %>%
  select(pH:OC, Crop, Yield_tha, has_ce_abatement)

# one-hot encode crops
Dat_model <- Dat_model %>%
  mutate(Crop = Crop %>% str_replace_all("\\W", "") %>% str_to_lower()) %>%
  dummy_cols() %>%
  select(pH:OC, Yield_tha, Crop_barley:Crop_wheat, has_ce_abatement)

# split datasets to train and test
set.seed(2605)
Dat_train <- Dat_model %>%
  sample_frac(0.7, replace = F)
Dat_test <- setdiff(Dat_model, Dat_train)

# create classifier
classifier <- rpart(has_ce_abatement ~ ., data = Dat_train)

# predictions
ypred <- predict(classifier, newdata = Dat_test[-ncol(Dat_test)])
preds <- tibble(actual = Dat_test$has_ce_abatement, predict_prob = ypred) %>%
  mutate(predict_class = as.numeric(predict_prob >= 0.5))

# confusion matrix
confmat <- table(preds$actual, preds$predict_class) # preds across top, actual down side
print(confmat)
(confmat[1, 1] + confmat [2, 2]) / sum(confmat) # prediction accuracy
confmat[2, 1] / sum(confmat) # false negative
confmat[1, 2] / sum(confmat) # false positive

# plot decision tree
plot(classifier)
text(classifier)
text(classifier, pos = 1)

plot(classifier)
text(classifier, pos = 3)
