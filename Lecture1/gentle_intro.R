# From RStudio webinar, a gentle introduction to tidy statistics
# https://www.rstudio.com/resources/webinars/a-gentle-introduction-to-tidy-statistics-in-r/


# Math

3 + 3

# Base R Functions + Objects

x <- 3 + 5

y <- c(1, 2, 3, 4, 5)

seq(0, 10, by=2)

# Custom Functions

add_pi <- function(x){
  x + 3.14
}

add_pi(3)

# libraries + functions

library(ggplot2)

ggplot(mtcars, aes(x=hp, y=mpg)) +
  geom_point() +
  geom_smooth(method = lm, se=TRUE) + 
  labs(x='Horsepower', 
       y='Miles per Gallon (MPG)', 
       title='Fuel efficiency decreases as Displacement increases')


# Tidyverse

library(tidyverse)

# dplyr + tidyr - data cleaning and transformation
# ggplot - plots

# the %>% (aka the "pipe") function

sqrt(2)
sqrt(exp(sqrt(2)))

2 %>% 
  sqrt %>%
  exp %>%
  sqrt


df_cars <- as_tibble(mtcars)

small_df <- df_cars %>%
  mutate(cyl = factor(cyl)) %>%
  select(mpg, cyl, disp, hp)

# small_df <- select(mutate(df_cars, cyl=factor(cyl)), mpg, cyl, disp, hp)


small_df %>% 
  ggplot(aes(x=cyl, y= mpg, color=cyl)) +
  geom_boxplot() + 
  geom_jitter()

View(small_df)

rm(list=ls())
