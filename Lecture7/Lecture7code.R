
library(tidyverse)
library(tidymodels)

# ------------------------------------------------------------------------------
# Part 6

transp <- 
  element_rect(fill = "transparent", colour = NA)

thm <- theme_bw() + 
  theme(
    panel.background = transp, 
    plot.background = transp,
    legend.background = transp,
    legend.key = transp,
    legend.position = "top"
  )

theme_set(thm)

# ------------------------------------------------------------------------------
# Illustrative Example (slide 5)

two_class_example %>% head(4)

# ------------------------------------------------------------------------------
# Class Prediction Metrics (slide 6)

two_class_example %>% 
  conf_mat(truth = truth, estimate = predicted)

two_class_example %>% 
  accuracy(truth = truth, estimate = predicted)

# ------------------------------------------------------------------------------
# The Receiver Operating Characteristic (ROC) Curve (slide 10)

roc_obj <- 
  two_class_example %>% 
  roc_curve(truth, Class1)

two_class_example %>% roc_auc(truth, Class1)

autoplot(roc_obj) + thm

# ------------------------------------------------------------------------------
# Amazon Review Data (slide 14)

library(modeldata)
data(small_fine_foods)

# ------------------------------------------------------------------------------
# Optional step: remove zero-variance predictors (slide 36)

library(textrecipes)

count_to_binary <- function(x) {
  factor(ifelse(x != 0, "present", "absent"),
         levels = c("present", "absent"))
}
text_rec <-
  recipe(score ~ product + review, data = training_data) %>%
  update_role(product, new_role = "id") %>%
  step_mutate(review_raw = review) %>%
  step_textfeature(review_raw) %>%
  step_tokenize(review) %>%
  step_stopwords(review)  %>%
  step_stem(review) %>%
  step_texthash(review, signed = FALSE, num_terms = 1024) %>%
  step_mutate_at(starts_with("review_hash"), fn = count_to_binary) %>%
  step_zv(all_predictors())

# ------------------------------------------------------------------------------
# Resampling and Analysis Strategy (slide 26)

set.seed(8935)
text_folds <- vfold_cv(training_data, strata = "score")

# ------------------------------------------------------------------------------
# {recipe} and {parsnip} objects (slide 34)

library(textfeatures)
library(textrecipes)

tree_rec <-
  recipe(score ~ product + review, data = training_data) %>%
  update_role(product, new_role = "id") %>%
  step_mutate(review_raw = review) %>%
  step_textfeature(review_raw) %>%               
  step_tokenize(review) %>%
  step_stopwords(review)  %>%
  step_stem(review) %>%
  step_texthash(review, signed = FALSE, num_terms = tune()) %>%
  step_zv(all_predictors()) 

# tree_rec %>% prep() %>% juice() %>% View()

# and 

cart_mod <- 
  decision_tree(cost_complexity = tune(), min_n = tune()) %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

ctrl <- control_grid(save_pred = TRUE)

# ------------------------------------------------------------------------------
# Model tuning (slide 35)

cart_wfl <- 
  workflow() %>% 
  add_recipe(tree_rec) %>% 
  add_model(cart_mod)

set.seed(2553)
#broken
cart_tune <-
  tune_grid(
    cart_wfl,
    text_folds,
    grid = 10,
    metrics = metric_set(roc_auc),
    control = ctrl
  )

# saveRDS(cart_tune, here::here('Lecture7', 'cart_tune.rds'))
# cart_tune <- read_rds(here::here('Lecture7', 'cart_tune.rds'))
show_best(cart_tune, metric = "roc_auc")

# ------------------------------------------------------------------------------
# Parameter profiles (slide 36)

autoplot(cart_tune)

# ------------------------------------------------------------------------------
# Plotting ROC curves (slide 37)

cart_pred <- collect_predictions(cart_tune)
cart_pred %>% slice(1:5)

cart_pred %>%
  inner_join(select_best(cart_tune)) %>%
  group_by(id) %>%
  roc_curve(score, .pred_great) %>%
  autoplot()

# ------------------------------------------------------------------------------
# A single (but approximate) ROC curve (slide 38)

auc_curve_data <- function(x) {
  collect_predictions(x) %>% 
    inner_join(select_best(x, "roc_auc")) %>% 
    roc_curve(score, .pred_great)
}

approx_roc_curves <- function(...) {
  curves <- map_dfr(list(...), auc_curve_data, .id = "model")
  default_cut <- 
    curves %>% 
    group_by(model) %>% 
    arrange(abs(.threshold - .5)) %>% 
    slice(1)
  ggplot(curves) +
    aes(y = sensitivity, x = 1 - specificity, col = model) +
    geom_abline(lty = 3) + 
    geom_step(direction = "vh") + 
    geom_point(data = default_cut) + 
    coord_equal()
}

# Use named arguments for better labels
approx_roc_curves(CART = cart_tune)

# ------------------------------------------------------------------------------
# Hands-On: Down-Sampling (slide 39)

# Looking at the ROC curve, the default cutoff may not be optimal if FP and FN errors are about equal.

# We could pick a better cutoff or fit another model using sub-class sampling.

# The latter approach would balance the data prior to model fitting.

# The most common method would be to down-sample the data.

# This is fairly controversial (at least in statistical circles).

# Let's take 20m and refit the model code above with a recipe that includes downsampling.

# link to recipes documentation: https://tidymodels.github.io/recipes/reference/index.html


# ------------------------------------------------------------------------------
# C5.0 (slide 50)

C5_mod <- 
  boost_tree(trees = tune(), min_n = tune()) %>% 
  set_engine("C5.0") %>% 
  set_mode("classification")

C5_wfl <- update_model(cart_wfl, C5_mod)

# We will just modify our CART grid and add 
# a new parameter: 
set.seed(5793)
C5_grid <- 
  collect_metrics(cart_tune) %>% 
  dplyr::select(min_n, num_terms) %>% 
  mutate(trees = sample(1:100, 10))

C5_tune <-
  tune_grid(
    C5_wfl,
    text_folds,
    grid = C5_grid,
    metrics = metric_set(roc_auc),
    control = ctrl
  )
# saveRDS(C5_tune, here::here('Lecture7', 'C5_tune.rds'))
# C5_tune <- read_rds(here::here('Lecture7', 'C5_tune.rds'))

# ------------------------------------------------------------------------------
# Comparing models (slide 51)

approx_roc_curves(CART = cart_tune, C5 = C5_tune)

show_best(C5_tune)

autoplot(C5_tune)


# ------------------------------------------------------------------------------
# Finalizing the recipe and model (slide 52)

best_C5 <- select_best(C5_tune)
best_C5

# no prep-juice calls!
C5_wfl_final <- 
  C5_wfl %>%
  finalize_workflow(best_C5) %>% 
  fit(data = training_data)

# saveRDS(C5_wfl_final, here::here('Lecture7', 'C5_wfl_final.rds'))
# C5_wfl_final <- read_rds(here::here('Lecture7', 'C5_wfl_final.rds'))

# ------------------------------------------------------------------------------
# Predicting the test set (slide 53)

test_probs <- 
  predict(C5_wfl_final, testing_data, type = "prob") %>% 
  bind_cols(testing_data %>% dplyr::select(score)) %>% 
  bind_cols(predict(C5_wfl_final, testing_data))

roc_auc(test_probs, score, .pred_great)

conf_mat(test_probs, score, .pred_class)

roc_values <- 
  roc_curve(test_probs, score, .pred_great)

autoplot(roc_values)

# ------------------------------------------------------------------------------
# Extra Slides


# ------------------------------------------------------------------------------
# Naive Bayes recipe and fit (slide 66)

count_to_binary <- function(x) {
  factor(ifelse(x != 0, "present", "absent"),
         levels = c("present", "absent"))
}

nb_rec <- 
  tree_rec %>%
  step_mutate_at(starts_with("review_hash"), fn = count_to_binary)

library(discrim)

nb_mod <- naive_Bayes() %>% set_engine("klaR")

nb_tune <-
  tune_grid(
    nb_rec,
    nb_mod,
    text_folds,
    grid = tibble(num_terms = floor(2^seq(8, 12, by = 0.5))),
    metrics = metric_set(roc_auc),
    control = ctrl
  )

# ------------------------------------------------------------------------------
# Naive Bayes results (slide 67)

autoplot(nb_tune) +
  scale_x_continuous(trans = log2_trans())

approx_roc_curves(CART = cart_tune, C5 = C5_tune, 
                  "Naive Bayes" = nb_tune)

# ------------------------------------------------------------------------------
# {tidypredict} and {modeldb} (slide 69)

library(tidypredict)
library(dbplyr)

lin_reg_fit <- lm(Sepal.Width ~ ., data = iris)

# R code
tidypredict_fit(lin_reg_fit)

# SQL code
tidypredict_sql(lin_reg_fit, con = simulate_dbi())

# ------------------------------------------------------------------------------
# Multiclass Metrics With yardstick (slide 70)

library(emo)

up <- ji("white_check_mark")
down <- ji("rage")

prec_example <- tibble(
  truth = factor(c(up, down, up, down, down), levels = c(up, down)),
  estimate = factor(c(up, down, up, up, down), levels = c(up, down))
)

prec_example

precision(prec_example, truth, estimate)

# ------------------------------------------------------------------------------
# Macro Averaging (slide 72)

precision(prec_multi, truth, estimate)

# ------------------------------------------------------------------------------
# Caveats (slide 73)

precision(prec_multi, truth, estimate, estimator = "macro_weighted")

#end broken