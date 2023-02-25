library(tidyverse)
library(tidymodels)

#READ IN THE DATA

credit_data <- read_csv(here::here('ConsumerCredit', 'ConsumerCred-train.csv'))
credit_data <- credit_data %>% mutate(SeriousDlqin2yrs = factor(SeriousDlqin2yrs))

#TRAIN/TEST SPLIT

set.seed(12345)

data_split <- initial_split(credit_data, strata = SeriousDlqin2yrs)
cred_train <- training(data_split)

cred_train %>% ggplot(aes(x=SeriousDlqin2yrs, y=age)) +
  geom_boxplot()

###CREATE CV SPLITS

set.seed(2453)
cred_cv_splits <- vfold_cv(cred_train) #10-fold is default


## SET THE MODEL
logreg_mod <- 
  logistic_reg() %>%
  set_mode('classification') %>%
  set_engine('glm')

## CREATE A WORKFLOW 
##Note: you could do some pre-processing here
logreg_wfl <- 
  workflow() %>%
  add_model(logreg_mod) %>%
  add_formula(SeriousDlqin2yrs ~  age)

###FIT THE MODEL
logreg_cv_fit <- fit_resamples(logreg_wfl, 
                  resamples = cred_cv_splits, 
                  control = control_resamples(save_pred = TRUE))

### ALTERNATIVE
glm_logreg <- glm(SeriousDlqin2yrs ~ age, 
                  data=cred_train,
                  family=binomial(link='logit'))

## EVALUATE MODEL PERFORMANCE
collect_metrics(logreg_cv_fit)      

logreg_preds <- collect_predictions(logreg_cv_fit)

logreg_preds %>% 
  roc_curve(SeriousDlqin2yrs, .pred_1, event_level='second') %>%
  autoplot()

### USE THE MODEL TO MAKE PREDICTIONS
##note: once we find a really good model, we might want to see how it works on testing(data_split)
final_fit <- fit(logreg_wfl, cred_train)
cred_eval <- read_csv(here::here('ConsumerCredit', 'ConsumerCred-test.csv'))

cred_eval$probability <- predict(final_fit, cred_eval, type='prob')$.pred_1
#or we could use the predict function on our glm mod

cred_eval %>% 
  select(id, probability) %>%
  write_csv('my-awesome-submission.csv')

###SOME THINGS WE DIDN'T HANDLE:
##    - unbalanced sample
##    - missing data
##    - outliers
##    - feature engineering - transformations


