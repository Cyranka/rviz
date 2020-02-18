remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/rviz/logistic_curve/")
library(tidyverse);library(readxl);library(nls2)


x <- read_excel("s007.xls", sheet = 2) %>%
    mutate(census = row_number()-1)

x %>%
    ggplot(aes(x = census, y = pop)) + 
    geom_point()

nls_model <- nls(pop~SSlogis(census,Asym,xmid,scal), data = x)
nls_model

x %>%
    modelr::add_predictions(nls_model) %>%
    ggplot(aes(x = census,  y= pop)) + 
    geom_line() + 
    geom_line(aes(x = census, y = pred), color = "red")

# Manual iteration --------------------------------------------------------
x2 <- x %>%
    mutate(transformed_variable = log((65.309 - pop)/pop))
    
x2 %>%
    ggplot(aes(x = census, y = transformed_variable)) + 
    geom_point()

##estimate parameters manually
lm_model <- lm(transformed_variable ~ census, data = x2)
 
##
x2 %>%
    mutate(estimates = (65.309/(1 + exp(2.9012258 + (-0.1410038*census))))) %>%
    ggplot(aes(x = census, y = pop)) + 
    geom_line() + 
    geom_line(aes(x = census, y = estimates), color ="red")