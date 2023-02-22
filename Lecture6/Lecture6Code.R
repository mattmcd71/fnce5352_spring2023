# ------------------------------------------------------------------------------
# Part 5

# ------------------------------------------------------------------------------
# Hands-On: Explore the Data (slide 4)

library(tidymodels)

data(Chicago)

library(lubridate)

Chicago_copy <- 
  Chicago %>% 
  mutate(day = wday(date, label = TRUE, abbr = FALSE),
         month = month(date))

Chicago_copy %>% ggplot(aes(x=ridership)) + 
  geom_density() + 
  facet_grid(day ~month)

Chicago_copy %>% 
  ggplot(aes(x = day, y = ridership)) + 
  geom_boxplot()

Chicago_copy %>% 
  ggplot(aes(x = day, y = ridership)) + 
  geom_violin()

Chicago_copy %>% 
  ggplot(aes(x = factor(Bears_Home), y = ridership)) + 
  geom_boxplot() + 
  facet_wrap(~ day)

corr_mat <- cor(Chicago[, 1:21])

library(corrplot)
corrplot(corr_mat, order = "hclust")

# ------------------------------------------------------------------------------
# A Recipe (slide 15)

library(stringr)

# define a few holidays

us_hol <- 
  timeDate::listHolidays() %>% 
  str_subset("(^US)|(Easter)")

chi_rec <-
  recipe(ridership ~ ., data = Chicago) %>%
  step_holiday(date, holidays = us_hol) %>%
  step_date(date) %>%
  step_rm(date) %>%
  step_dummy(all_nominal()) %>%
  step_zv(all_predictors())
# step_normalize(one_of(!!stations))  #<<
# step_pca(one_of(!!stations), num_comp = tune()) #<<

# ------------------------------------------------------------------------------
# Resampling (slide 16)

chi_folds <- rolling_origin(Chicago, initial = 364 * 15, assess = 7 * 4, skip = 7 * 4, cumulative = FALSE)
chi_folds %>% nrow()

# ------------------------------------------------------------------------------
# Linear Regression Analysis (slide 20)

simplestmodel <- lm(ridership ~ . - date, data = Chicago)

cor(Chicago$Irving_Park, Chicago$Belmont)
plot(Chicago$Irving_Park, Chicago$Belmont)

lm(ridership ~ Irving_Park, data=Chicago) %>% summary
lm(ridership ~ Belmont, data=Chicago) %>% summary
lm(ridership ~ Irving_Park + Belmont, data=Chicago) %>% summary


# ------------------------------------------------------------------------------
# Tuning the Model (slide 26)

glmn_grid <- expand.grid(penalty = 10^seq(-3, -1, length = 20), mixture = (0:5)/5)

# ------------------------------------------------------------------------------
# Tuning the Model (slide 27)

glmn_rec <- chi_rec %>% step_normalize(all_predictors())

glmn_mod <-
  linear_reg(penalty = tune(), mixture = tune()) %>% 
  set_engine("glmnet")

# Save the assessment set predictions
ctrl <- control_grid(save_pred = TRUE)

glmn_tune <-
  tune_grid(
    glmn_mod,
    preprocessor = glmn_rec,
    resamples = chi_folds,
    grid = glmn_grid,
    control = ctrl
  )

# ------------------------------------------------------------------------------
# Plotting the Resampling Profile (slide 30)

rmse_vals <-
  collect_metrics(glmn_tune) %>%
  filter(.metric == "rmse")

rmse_vals %>%
  mutate(mixture = format(mixture)) %>%
  ggplot(aes(x = penalty, y = mean, col = mixture)) +
  geom_line() +
  geom_point() +
  scale_x_log10()

# There is `autoplot(glmn_tune)` but the grid
# structure works better with the code above.

# ------------------------------------------------------------------------------
# Capture the Best Values (slide 31)

show_best(glmn_tune, metric = "rmse")

best_glmn <-
  select_best(glmn_tune, metric = "rmse")
best_glmn

# ------------------------------------------------------------------------------
# Residual Analysis (slide 32)

glmn_pred <- collect_predictions(glmn_tune)
glmn_pred

# ------------------------------------------------------------------------------
# Observed Versus Predicted Plot (slide 33)

# Keep the best model
glmn_pred <-
  glmn_pred %>%
  inner_join(best_glmn, by = c("penalty", "mixture"))

ggplot(glmn_pred, aes(x = .pred, y = ridership)) +
  geom_abline(col = "green") +
  geom_point(alpha = .3) +
  coord_equal()

# ------------------------------------------------------------------------------
# Which training set points had the worst results? (slide 34)

large_resid <- 
  glmn_pred %>% 
  mutate(resid = ridership - .pred) %>% 
  arrange(desc(abs(resid))) %>% 
  slice(1:4)

library(lubridate)
Chicago %>% 
  slice(large_resid$.row) %>% 
  dplyr::select(date) %>% 
  mutate(day = wday(date, label = TRUE)) %>% 
  bind_cols(large_resid)

# ------------------------------------------------------------------------------
# Creating a Final Model (slide 35)

glmn_rec_final <- prep(glmn_rec)

glmn_mod_final <- finalize_model(glmn_mod, best_glmn)
glmn_mod_final

glmn_fit <- 
  glmn_mod_final %>% 
  fit(ridership ~ ., data = juice(glmn_rec_final))

glmn_fit

# ------------------------------------------------------------------------------
# Using the glmnet Object (slide 36)

library(glmnet)
plot(glmn_fit$fit, xvar = "lambda")

# predict(object$fit) Noooooooooooooo!

# ------------------------------------------------------------------------------
# A glmnet Coefficient Plot (slide 37)

library(ggrepel)

# Get the set of coefficients across penalty values
tidy_coefs <-
  broom::tidy(glmn_fit$fit) %>%
  dplyr::filter(term != "(Intercept)") %>%
  dplyr::select(-step, -dev.ratio)

# Get the lambda closest to tune's optimal choice
delta <- abs(tidy_coefs$lambda - best_glmn$penalty)
lambda_opt <- tidy_coefs$lambda[which.min(delta)]

# Keep the large values
label_coefs <-
  tidy_coefs %>%
  mutate(abs_estimate = abs(estimate)) %>%
  dplyr::filter(abs_estimate >= 1.1) %>%
  distinct(term) %>%
  inner_join(tidy_coefs, by = "term") %>%
  dplyr::filter(lambda == lambda_opt)

# plot the paths and highlight the large values
tidy_coefs %>%
  ggplot(aes(x = lambda, y = estimate, group = term, col = term, label = term)) +
  geom_vline(xintercept = lambda_opt, lty = 3) +
  geom_line(alpha = .4) +
  theme(legend.position = "none") +
  scale_x_log10() +
  geom_text_repel(data = label_coefs, aes(x = .005))

# ------------------------------------------------------------------------------
# glmnet Variable Importance (slide 39)

library(vip)

vip(glmn_fit, num_features = 20L, 
    # Needs to know which coefficients to use
    lambda = best_glmn$penalty)

# ------------------------------------------------------------------------------
# MARS in via {parsnip} and {tune} (slide 53)

mars_mod <-  mars(prod_degree = tune())

# We'll decide via search:
mars_mod <-  
  mars(num_terms = tune("mars terms"), prod_degree = tune(), prune_method = "none") %>% 
  set_engine("earth") %>% 
  set_mode("regression")

mars_rec <- 
  chi_rec %>% 
  step_normalize(one_of(!!stations)) %>% 
  step_pca(one_of(!!stations), num_comp = tune("pca comps"))

# ------------------------------------------------------------------------------
# Parameter Ranges (slide 69)

chi_wflow <-
  workflow() %>%
  add_recipe(mars_rec) %>%
  add_model(mars_mod)



chi_set <-
  parameters(chi_wflow) %>%
  update(
    `pca comps`  =  num_comp(c(0, 20)), # 0 comps => PCA is not used 
    `mars terms` = num_terms(c(2, 100)))

chi_grid <- chi_set %>%
  grid_regular(levels=c(10, 2, 7))

ctrl <- control_grid(verbose = TRUE)

#this takes a long time
# mars_tune_grid <-
#   tune_grid(
#     chi_wflow,
#     resamples = chi_folds,
#     grid = chi_grid,
#     metrics = metric_set(rmse),
#     control = ctrl
#   )
# 
# saveRDS(mars_tune_grid, 'mars_tune_grid.rds')
readRDS('mars_tune_grid.rds')

# ------------------------------------------------------------------------------
# # Running the Optimization (slide 70)
# 
# # library(doMC)
# #broken
# registerDoMC(cores = 8)
# 
# ctrl <- control_bayes(verbose = TRUE, save_pred = TRUE)
# 
# # Some defaults:
# #   - Uses expected improvement with no trade-off. See ?exp_improve().
# #   - RMSE is minimized
# set.seed(7891)
# mars_tune <-
#   tune_bayes(
#     chi_wflow,
#     resamples = chi_folds,
#     iter = 25,
#     param_info = chi_set,
#     metrics = metric_set(rmse),
#     initial = 4,
#     control = ctrl
#   )
# 
# 
# # ------------------------------------------------------------------------------
# # Performance over iterations (slide 72)
# 
# autoplot(mars_tune, type = "performance")
# 
# 
# # ------------------------------------------------------------------------------
# # Performance versus parameters (slide 73)
# 
# autoplot(mars_tune, type = "marginals")
# 
# # ------------------------------------------------------------------------------
# # Parameters over iterations (slide 74)
# 
# autoplot(mars_tune, type = "parameters")
# 
# # ------------------------------------------------------------------------------
# # Results (slide 75)
# 
# show_best(mars_tune, maximize = FALSE)
# 
# # ------------------------------------------------------------------------------
# # Assessment Set Results (Again) (slide 79)
# 
# mars_pred <-
#   mars_tune_grid %>%
#   collect_predictions() %>%
#   inner_join(
#     select_best(mars_tune, maximize = FALSE),
#     by = c("mars terms", "prod_degree", "pca comps")
#   )
# 
# ggplot(mars_pred, aes(x = .pred, y = ridership)) +
#   geom_abline(col = "green") +
#   geom_point(alpha = .3) +
#   coord_equal()

# ------------------------------------------------------------------------------
# Finalizing the recipe and model (slide 80)

best_mars <- select_best(mars_tune_grid, "rmse")
best_mars

final_mars_wfl <- finalize_workflow(chi_wflow, best_mars)

# No formula is needed since a recipe is embedded in the workflow
final_mars_wfl <- fit(final_mars_wfl, data = Chicago)

# ------------------------------------------------------------------------------
# Variable importance (slide 81)

final_mars_wfl %>% 
  # Pull out the model
  pull_workflow_fit() %>%
  vip(num_features = 20L, type = "gcv")
  
