rm(list =ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

# Begin analysis ----------------------------------------------------------
library(tidyverse)
x <- read_csv("fastfood_calories.csv")


# Input mean function
input_mean <- function(x){
  return(round(ifelse(is.na(x), mean(x, na.rm = TRUE), x),2))
}

y <- suppressWarnings(x %>% select_if(is_numeric) %>%
  mutate_all(input_mean) %>%
  mutate(item = x$item, 
         restaurant = x$restaurant) %>%
  select(restaurant, item, calories:calcium))

x_1 <- x %>%
  select(restaurant, item,calories, total_fat, total_carb, protein, sugar) %>%
  mutate(c_total_fat = total_fat -mean(total_fat, na.rm = TRUE),
         c_total_carb = total_carb - mean(total_carb, na.rm = TRUE),
         c_protein = protein - mean(protein, na.rm = TRUE),
         c_sugar = sugar - mean(sugar, na.rm = TRUE))

##
fit_1 <- lm(calories~c_total_fat + c_total_carb + c_protein + c_sugar, data = x_1)
arm::display(fit_1, detail = TRUE, digits = 1)

confint.default(fit_1) %>% round(1)

x_1 <- x_1 %>% mutate(predicted_calories =predict(fit_1, x_1),
  residuals = calories - predict(fit_1, x_1))

x_1 %>% 


