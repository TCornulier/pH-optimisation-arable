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
  select(Sand:pH, Crop, Yield_tha, has_ce_abatement)

# one-hot encode crops
Dat_model <- Dat_model %>%
  mutate(Crop = Crop %>% str_replace_all("\\W", "") %>% str_to_lower()) %>%
  dummy_cols() %>%
  select(Sand, Clay, BD, OC, pH, Yield_tha, Crop_barley:Crop_wheat, has_ce_abatement) # dropping Crop and Clay variables

# encode y as factor
Dat_model <- Dat_model %>%
  mutate(has_ce_abatement = as.factor(has_ce_abatement))

# split datasets to train and test
set.seed(260592)
Dat_train <- Dat_model %>%
  sample_frac(0.7, replace = F)
Dat_test <- setdiff(Dat_model, Dat_train)

# create classifier
control <- rpart.control(minsplit = 300, minbucket = 100, maxdepth = 10)
classifier <- rpart(has_ce_abatement ~ ., data = Dat_train, control = control)

# predictions
ypred <- predict(classifier, newdata = Dat_test[-ncol(Dat_test)])
preds <- tibble(actual = Dat_test$has_ce_abatement, predict_prob = ypred) %>%
  mutate(predict_class = as.numeric(predict_prob[, "1"] >= 0.5))

# confusion matrix
confmat <- table(preds$actual, preds$predict_class) # preds across top, actual down side
print(confmat)
(confmat[1, 1] + confmat [2, 2]) / sum(confmat) # prediction accuracy
confmat[2, 1] / sum(confmat) # false negative
confmat[1, 2] / sum(confmat) # false positive

# plot decision tree
par(mar = c(0, 0, 0, 0))
plot(classifier)
text(classifier)
