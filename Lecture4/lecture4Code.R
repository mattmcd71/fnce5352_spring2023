
# ------------------------------------------------------------------------------
# Applied Machine Learning - RStudio::conf, 2020
# Max Kuhn (max@rstudio.com) and Davis Vaughan (davis@rstudio.com)

# ------------------------------------------------------------------------------
# Part 1

library(tidyverse)
library(tidymodels)

# read_delim("http://bit.ly/2whgsQM", delim = "\t", guess_max = 2000)

# ------------------------------------------------------------------------------

thm <- theme_bw() + 
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA), 
    plot.background = element_rect(fill = "transparent", colour = NA),
    legend.position = "top",
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA)
  )
theme_set(thm)

# ------------------------------------------------------------------------------
# Some Example Data Manipulation Code (slide 11)

library(tidyverse)

ames_prices <- here::here('Lectures4thru8', "AmesPrices.txt") %>%
  read_delim(delim = "\t", guess_max = 2000) %>%
  rename_at(vars(contains(' ')), list(~gsub(' ', '_', .))) %>%
  dplyr::rename(Sale_Price = SalePrice) %>%
  dplyr::filter(!is.na(Electrical)) %>%
  dplyr::select(-Order, -PID, -Garage_Yr_Blt)

ames_prices %>%
  group_by(Alley) %>%
  summarize(
    mean_price = mean(Sale_Price / 1000),
    n = sum(!is.na(Sale_Price))
  )
# ------------------------------------------------------------------------------
# Examples of purrr::map* (slide 12)

# purrr loaded with tidyverse or tidymodels package

mini_ames <- ames_prices %>%
  dplyr::select(Alley, Sale_Price, Yr_Sold) %>%
  dplyr::filter(!is.na(Alley))

head(mini_ames, n = 5)

by_alley <- split(mini_ames, mini_ames$Alley)
# map(.x, .f, ...)
map(by_alley, head, n = 2)

for (i in 1:length(by_alley)) {
  head(by_alley[[i]], n=2) %>% print
}

# ------------------------------------------------------------------------------
# Examples of purrr::map* (slide 13)

map(by_alley, nrow)

map_int(by_alley, nrow)

map(
  by_alley, 
  ~summarise(.x, max_price = max(Sale_Price))
)

# ------------------------------------------------------------------------------
# purrr and list-columns (slide 14)

ames_lst_col <- nest(mini_ames, data = c(Sale_Price, Yr_Sold))
ames_lst_col

ames_lst_col %>%
  mutate(
    n_row = map_int(data, nrow),
    max   = map_dbl(data, ~ max(.x$Sale_Price))
  )

# ------------------------------------------------------------------------------
# Hands-on: Quick Data Investigation (slide 15)
# check out: http://jse.amstat.org/v19n3/decock/DataDocumentation.txt

library(tidyverse)
library(AmesHousing)
ames <- make_ames()

theme_set(theme_bw())

# outliers
ggplot(ames, aes(x = Lot_Area)) + 
  geom_histogram()

ggplot(ames, aes(x = Lot_Area)) + 
  geom_histogram() + 
  scale_x_log10()

str(ames$Condition_1)

ggplot(ames, aes(x = Condition_1)) + 
  geom_bar() + 
  coord_flip() 

grep("SF$", names(ames), value = TRUE)

ggplot(ames, aes(x = Total_Bsmt_SF, y = First_Flr_SF)) + 
  geom_point(alpha = .3)

ggplot(ames, aes(x = Lot_Area)) + 
  geom_histogram() + 
  facet_wrap(~Lot_Shape) + 
  scale_x_log10()

ggplot(ames, aes(x = Gr_Liv_Area, Sale_Price)) + 
  geom_point(alpha = .3) + 
  scale_x_log10() + 
  scale_y_log10() + 
  facet_wrap(~Bldg_Type) + 
  geom_smooth(method = lm)

ames %>% ggplot(aes(x=Sale_Price)) + geom_density()



# ------------------------------------------------------------------------------
# Part 2

# ------------------------------------------------------------------------------
# Ames Housing Data (slide 5)

ames <- 
  make_ames() %>% 
  # Remove quality-related predictors
  dplyr::select(-matches("Qu"))
nrow(ames)

set.seed(12345)
sample(x=1:10, size=8, replace=FALSE)

set.seed(12345)
training_indices <- sample(1:nrow(ames), size=floor(nrow(ames)*0.8))
training_data <- ames[training_indices,]
testing_data <- ames[-training_indices,]

library(ggplot2)
ggplot(training_data, aes(x=Sale_Price)) + geom_density() +  
  geom_density(data=testing_data, aes(x=Sale_Price), color='red')

# resample functions
# Make sure that you get the same random numbers
set.seed(4595)
data_split <- initial_split(ames, strata = "Sale_Price")

ames_train <- training(data_split)
ames_test  <- testing(data_split)

nrow(ames_train)/nrow(ames)

ames_train %>%
  ggplot(aes(x=Sale_Price)) + 
  geom_density() + scale_x_log10() +
  geom_density(data=ames_test, aes(x=Sale_Price), color='red')

# ------------------------------------------------------------------------------
# Ames Housing Data (slide 6)

data_split

training(data_split)

# ------------------------------------------------------------------------------
# A Linear Regression Model (slide 11)

simple_lm <- lm(log10(Sale_Price) ~ Longitude + Latitude, data = ames_train)

summary(simple_lm)

simple_lm_values <- augment(simple_lm)
names(simple_lm_values)

# ------------------------------------------------------------------------------
# parsnip in Action (slide 13)

spec_lin_reg <- linear_reg()
spec_lin_reg

lm_mod <- set_engine(spec_lin_reg, "lm")
lm_mod

lm_fit <- fit(
  lm_mod,
  log10(Sale_Price) ~ Longitude + Latitude,
  data = ames_train
)

lm_fit

# ------------------------------------------------------------------------------
# Different interfaces (slide 14)

ames_train_log <- ames_train %>%
  mutate(Sale_Price_Log = log10(Sale_Price))

fit_xy(
  lm_mod,
  y = ames_train_log$Sale_Price_Log,
  x = ames_train_log %>% dplyr::select(Latitude, Longitude)
)

# ------------------------------------------------------------------------------
# Alternative Engines (slide 15)

spec_stan <- 
  spec_lin_reg %>%
  # Engine specific arguments are passed through here
  set_engine("stan", chains = 4, iter = 1000)

# Otherwise, looks exactly the same!
fit_stan <- fit(
  spec_stan,
  log10(Sale_Price) ~ Longitude + Latitude,
  data = ames_train
)

coef(fit_stan$fit)

coef(lm_fit$fit)

# ------------------------------------------------------------------------------
# Different models (slide 16)

fit_knn <- 
  nearest_neighbor(mode = "regression", neighbors = 5) %>%
  set_engine("kknn") %>% 
  fit(log10(Sale_Price) ~ Longitude + Latitude, data = ames_train)
fit_knn


# ------------------------------------------------------------------------------
# Predictions (slide 18)

# Numeric predictions always in a df
# with column `.pred`

test_pred <- 
  lm_fit %>%
  predict(ames_test) %>%
  bind_cols(ames_test) %>%
  mutate(log_price = log10(Sale_Price))

test_pred %>% 
  dplyr::select(log_price, .pred) %>% 
  ggplot(aes(x=log_price, y=.pred)) + geom_point()

# ------------------------------------------------------------------------------
# Estimating Performance (slide 19)

# yardstick loaded by tidymodels

perf_metrics <- metric_set(rmse, rsq, ccc)

# A tidy result back:
test_pred  %>% 
  perf_metrics(truth = log_price, estimate = .pred)

test_pred %>%
dplyr::select(log_price, .pred) %>%
  ggplot(aes(x=log_price, y=.pred)) + geom_point()

