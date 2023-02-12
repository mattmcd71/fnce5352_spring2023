
# ------------------------------------------------------------------------------
# Part 3

# ------------------------------------------------------------------------------
# Recipes (slide 12)

library(tidyverse)
library(AmesHousing)
library(tidymodels)
ames <- 
  make_ames() %>% 
  # Remove quality-related predictors
  dplyr::select(-matches("Qu"))
set.seed(4595)
data_split <- initial_split(ames, strata = "Sale_Price")

ames_train <- training(data_split)
ames_test  <- testing(data_split)
mod_rec <- recipe(Sale_Price ~ Longitude + Latitude, data = ames_train) %>%
  step_log(Sale_Price, base = 10)

# ------------------------------------------------------------------------------
# Recipes and Categorical Predictors (slide 13)

mod_rec <- recipe(
  Sale_Price ~ Longitude + Latitude + Neighborhood, 
  data = ames_train
) %>%
  step_log(Sale_Price, base = 10) %>%
  
  # Lump factor levels that occur in 
  # <= 5% of data as "other"
  step_other(Neighborhood, threshold = 0.05) %>%
  
  # Create dummy variables for _any_ factor variables
  step_dummy(all_nominal())

# ------------------------------------------------------------------------------
# Preparing the Recipe (slide 15)

mod_rec_trained <- prep(mod_rec, training = ames_train, verbose = TRUE)

# ------------------------------------------------------------------------------
# Preparing the Recipe (slide 16)

mod_rec_trained

# ------------------------------------------------------------------------------
# Getting the Values (slide 17)

ames_test_dummies <- bake(mod_rec_trained, new_data = ames_test)
names(ames_test_dummies)


# ------------------------------------------------------------------------------
# Hands-On: Zero-Variance Filter(slide 20)

# Instead of using `step_other()`, take 10 minutes and research how to eliminate 
# any zero-variance predictors using the recipe reference site.
# (https://tidymodels.github.io/recipes/reference/index.html)

mod_rec2 <- recipe(
  Sale_Price ~ Longitude + Latitude + Neighborhood, 
  data = ames_train
) %>%
  step_log(Sale_Price, base = 10) %>%
  
  # Create dummy variables for _any_ factor variables
  step_dummy(all_nominal()) %>%

  step_nzv(all_predictors())
  

# Re-run the recipe with this step.
mod_rec2_trained <- prep(mod_rec2, training = ames_train, verbose = TRUE)
ames_train_dummies2 <- juice(mod_rec2_trained)

# What were the results?

# Do you prefer either of these approaches to the other?

# ------------------------------------------------------------------------------
# Interactions (slide 22)

price_breaks <- (1:6)*(10^5)

ggplot(
  ames_train, 
  aes(x = Year_Built, y = Sale_Price)
) + 
  geom_point(alpha = 0.4) +
  scale_y_log10() + 
  geom_smooth(method = "loess")

# ------------------------------------------------------------------------------
# Interactions (slide 23)

library(MASS) # to get robust linear regression model

ggplot(
  ames_train, 
  aes(x = Year_Built, 
      y = Sale_Price)
) + 
  geom_point(alpha = 0.4) +
  scale_y_log10() + 
  facet_wrap(~ Central_Air, nrow = 2) +
  geom_smooth(method = "rlm") 

# ------------------------------------------------------------------------------
# Interactions (slide 24)

mod1 <- lm(log10(Sale_Price) ~ Year_Built + Central_Air,                          data = ames_train)
mod2 <- lm(log10(Sale_Price) ~ Year_Built + Central_Air + Year_Built:Central_Air, data = ames_train)
anova(mod1, mod2)

# ------------------------------------------------------------------------------
# Interactions in Recipes (slide 25)

recipe(Sale_Price ~ Year_Built + Central_Air, data = ames_train) %>%
  step_log(Sale_Price) %>%
  step_dummy(Central_Air) %>%
  step_interact(~ starts_with("Central_Air"):Year_Built) %>%
  prep(training = ames_train) %>%
  juice() %>%
  # select a few rows with different values
  slice(153:157)

# ------------------------------------------------------------------------------
# Bivariate Data for PCA

data(segmentationData, package = "caret")

segmentationData <- segmentationData[, c("EqSphereAreaCh1", "PerimCh1", "Class", "Case")]
names(segmentationData)[1:2] <- paste0("Predictor", LETTERS[1:2])

segmentationData$Class <- factor(ifelse(segmentationData$Class == "PS", "One", "Two"))

bivariate_data_train <- subset(segmentationData, Case == "Train")
bivariate_data_test  <- subset(segmentationData, Case == "Test")

bivariate_data_train$Case <- NULL
bivariate_data_test$Case  <- NULL

# ------------------------------------------------------------------------------
# A Bivariate Example (slide 27)

library(ggthemes)
ggplot(bivariate_data_test, 
       aes(x = PredictorA, 
           y = PredictorB,
           color = Class)) +
  geom_point(alpha = .3, cex = 1.5) + 
  theme(legend.position = "top") +
  scale_colour_calc() 

# ------------------------------------------------------------------------------
# A Bivariate Example (slide 27)

bivariate_rec <- recipe(Class ~ PredictorA + PredictorB, 
                        data = bivariate_data_train) %>%
  step_BoxCox(all_predictors())

bivariate_rec <- prep(bivariate_rec, training = bivariate_data_train, verbose = FALSE)

inverse_test <- bake(bivariate_rec, new_data = bivariate_data_test, everything())

ggplot(inverse_test, 
       aes(x = 1/PredictorA, 
           y = 1/PredictorB,
           color = Class)) +
  geom_point(alpha = .3, cex = 1.5) + 
  theme(legend.position = "top") +
  scale_colour_calc() +
  xlab("1/A") + ylab("1/B") 


# ------------------------------------------------------------------------------
# Back to the Bivariate Example - Recipes (slide 347)

bivariate_pca <- 
  recipe(Class ~ PredictorA + PredictorB, data = bivariate_data_train) %>%
  step_BoxCox(all_predictors()) %>%
  step_normalize(all_predictors()) %>% # center and scale
  step_pca(all_predictors()) %>%
  prep(training = bivariate_data_test, verbose = FALSE)

pca_test <- bake(bivariate_pca, new_data = bivariate_data_test)

# Put components axes on the same range
pca_rng <- extendrange(c(pca_test$PC1, pca_test$PC2))

ggplot(pca_test, aes(x = PC1, y = PC2, color = Class)) +
  geom_point(alpha = .2, cex = 1.5) + 
  theme(legend.position = "top") +
  scale_colour_calc() +
  xlim(pca_rng) + ylim(pca_rng) + 
  xlab("Principal Component 1") + ylab("Principal Component 2") 

# ------------------------------------------------------------------------------
# See pca_rotation.R for slide 37

# ------------------------------------------------------------------------------
# Longitude (slide 39)

ggplot(ames_train, 
       aes(x = Longitude, y = Sale_Price)) + 
  geom_point(alpha = .5) + 
  geom_smooth(
    method = "lm", 
    formula = y ~ splines::bs(x, 5), 
    se = FALSE
  ) + 
  scale_y_log10()

# ------------------------------------------------------------------------------
# Latitude(slide 40)

ggplot(ames_train, 
       aes(x = Latitude, y = Sale_Price)) + 
  geom_point(alpha = .5) + 
  geom_smooth(
    method = "lm", 
    formula = y ~ splines::ns(x, df = 5), 
    se = FALSE
  ) + 
  scale_y_log10()

# ------------------------------------------------------------------------------
# Linear Models Again (slide 41)

ames_rec <- 
  recipe(Sale_Price ~ Bldg_Type + Neighborhood + Year_Built + 
           Gr_Liv_Area + Full_Bath + Year_Sold + Lot_Area +
           Central_Air + Longitude + Latitude,
         data = ames_train) %>%
  step_log(Sale_Price, base = 10) %>%
  step_BoxCox(Lot_Area, Gr_Liv_Area) %>%
  step_other(Neighborhood, threshold = 0.05)  %>%
  step_dummy(all_nominal()) %>%
  step_interact(~ starts_with("Central_Air"):Year_Built) %>%
  step_ns(Longitude, Latitude, deg_free = 5)

# ------------------------------------------------------------------------------
# Combining the Recipe with a Model (slide 42)

ames_rec <- prep(ames_rec)

spec_lin_reg <- linear_reg()
spec_lin_reg

lm_mod <- set_engine(spec_lin_reg, "lm")

lm_fit <- 
  lm_mod %>% 
  fit(Sale_Price ~ ., data = juice(ames_rec))   # The recipe puts Sale_Price on the log scale

glance(lm_fit$fit)

holdout_data <- bake(ames_rec, ames_test, all_predictors())

# but let's not do this
# predict(lm_fit, new_data = holdout_data)

# ------------------------------------------------------------------------------
# An example (slide 45)

ames_wfl <- 
  workflow() %>% 
  add_recipe(ames_rec) %>% 
  add_model(lm_mod)
ames_wfl

# ------------------------------------------------------------------------------
# 1-Step fitting and predicting (slide 46)

ames_wfl_fit <- fit(ames_wfl, ames_train)
# #broken
# predict(ames_wfl_fit, ames_test %>% slice(1:5))

# ------------------------------------------------------------------------------
# Part 4

# ------------------------------------------------------------------------------
# Cross-Validating Using {rsample} (slide 10)

set.seed(2453)
cv_splits <- vfold_cv(ames_train) #10-fold is default
cv_splits

cv_splits$splits[[1]]

cv_splits$splits[[1]] %>% analysis() %>% dim()
cv_splits$splits[[1]] %>% assessment() %>% dim()

# ------------------------------------------------------------------------------
# Resampling a 5-NN model (slide 13)

knn_mod <- 
  nearest_neighbor(neighbors = 5) %>% 
  set_engine("kknn") %>% 
  set_mode("regression")

knn_wfl <- 
  workflow() %>% 
  add_model(knn_mod) %>% 
  add_formula(log10(Sale_Price) ~ Longitude + Latitude)

## fit(knn_wfl, data = ames_train)

# ------------------------------------------------------------------------------
# Resampling a 5-NN model (slide 14)

knn_res <-
  cv_splits %>%
  mutate( workflows = map(splits, ~ fit( knn_wfl, data = analysis(.x)) ) ) 
  #mutate( workflows = map(splits, function(x) fit(knn_wfl, data=analysis(x))))
knn_res

# ------------------------------------------------------------------------------
# Compute Overall RMSE estimate (slide 22)

knn_pred <-                                              
  map2_dfr(knn_res$workflows, knn_res$splits,     
           ~ predict(.x, assessment(.y)),
           #function(x, y) predict(x, assessment(y)),
           .id = "fold")                                 

# map_dbl(1:10, function(x) x+3)
# map_dbl(1:10, ~ .x + 3)

prices <-  
  map_dfr(knn_res$splits,  
          ~ assessment(.x) %>% dplyr::select(Sale_Price)) %>%  
  mutate(Sale_Price = log10(Sale_Price))

rmse_estimates <- 
  knn_pred %>%  
  bind_cols(prices) %>% 
  group_by(fold) %>% 
#  summarise(rmse = rmse(across(), Sale_Price, .pred), .groups='drop')
  do(rmse = rmse(., Sale_Price, .pred)) %>% 
  unnest(cols = c(rmse)) 

mean(rmse_estimates$.estimate)

# ------------------------------------------------------------------------------
# Easy resampling using the {tune} package (slide 26)

easy_eval <- fit_resamples(knn_wfl, resamples = cv_splits, control = control_resamples(save_pred = TRUE))
easy_eval

# ------------------------------------------------------------------------------
# Getting the statistics and predictions (slide 27)

collect_predictions(easy_eval) %>% 
  arrange(.row) %>% 
  slice(1:5) 

collect_metrics(easy_eval)

collect_metrics(easy_eval, summarize = FALSE) %>% 
  slice(1:10)

# ------------------------------------------------------------------------------
# Making Regular Grids (slide 35)

penalty()
mixture()

glmn_param <- parameters(penalty(), mixture())

glmn_param

glmn_grid <- 
  grid_regular(glmn_param, levels = c(10, 5))

glmn_grid %>% slice(1:4)

# ------------------------------------------------------------------------------
# Non-Regular Grids (slide 36)

set.seed(7454)
glmn_sfd <- grid_max_entropy(glmn_param, size = 50)
glmn_sfd %>% slice(1:4)

# ------------------------------------------------------------------------------
# Modifying Parameter Sets (slide 37)

glmn_set <- parameters(lambda = penalty(), mixture())

# The ranges can also be set by their name:
glmn_set <- 
  update(glmn_set, lambda = penalty(c(-5, -1)))

# Some parameters depend on data dimensions:
mtry()

rf_set <- parameters(mtry(), trees())
rf_set

# Sets the range of mtry to be the number of predictors
finalize(rf_set, mtcars %>% dplyr::select(-mpg))

# ------------------------------------------------------------------------------
# Hands-On: K-NN Grids

knn_param <- 
  parameters(neighbors(), weight_func(), dist_power())

knn_sfd <- grid_max_entropy(knn_param, size=50)

# ------------------------------------------------------------------------------
# Tagging Tuning parameters (slide 40)

library(tune)
knn_mod <- 
  nearest_neighbor(neighbors = tune('K'), weight_func = tune()) %>% 
  set_engine("kknn") %>% 
  set_mode("regression")

parameters(knn_mod)

# Max's code

knn_param <- 
  parameters(neighbors(), weight_func(), dist_power())

grid_random(knn_param)
# or 
grid_random(neighbors(), weight_func(),  dist_power()) 

grid_vals <- grid_max_entropy(knn_param, size = 50)

grid_vals %>% ggplot(aes(x = neighbors, y = dist_power, col = weight_func)) + 
  geom_point()

set.seed(522)
knn_grid <- knn_mod %>% parameters() %>% grid_regular(levels = c(15, 5))
ctrl <- control_grid(verbose = TRUE)

ames_wfl2 <- 
  workflow() %>% 
  add_recipe(ames_rec) %>% 
  add_model(knn_mod)

knn_tune <- 
  tune_grid(ames_wfl2, resamples = cv_splits, grid = knn_grid, control = ctrl)

knn_tune
knn_tune$.metrics[[1]]
show_best(knn_tune, metric = "rmse")

knn_tune %>% 
  collect_metrics() %>% 
  dplyr::filter(.metric == "rmse") %>% 
  ggplot(aes(x = n, y = mean, col = weight_func)) + 
  geom_point() + 
  geom_line() +
  theme(legend.position = "top") + 
  ylim(c(0.07, 0.12))

# ------------------------------------------------------------------------------
# Tagging Tuning parameters (slide 41)

nearest_neighbor(neighbors = tune("K"), weight_func = tune("weights")) %>% 
  set_engine("kknn") %>% 
  set_mode("regression") %>% 
  parameters()

# ------------------------------------------------------------------------------
# Grid Search (slide 42)
library(tune)
knn_mod <- 
  nearest_neighbor(neighbors = tune(), weight_func = tune()) %>% 
  set_engine("kknn") %>% 
  set_mode("regression")

parameters(knn_mod)


set.seed(522)
knn_grid <- knn_mod %>% 
  parameters() %>% 
  grid_regular(levels = c(15, 5))
ctrl <- control_grid(verbose = TRUE)
#broken
# knn_tune <- tune_grid(
#   knn_mod,
#   resamples = cv_splits, 
#   grid = knn_grid, 
#   control = ctrl
# )

# ------------------------------------------------------------------------------
# The Results (slide 44)

# knn_tune

# results for the first fold:
# knn_tune$.metrics[[1]]

# ------------------------------------------------------------------------------
# Resampled Performance Estimates (slide 45)

# show_best(knn_tune, metric = "rmse")
