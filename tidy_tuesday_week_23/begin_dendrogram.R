rm(list =ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

# Begin analysis ----------------------------------------------------------
library(tidyverse)
library(dynamicTreeCut)
x <- read_csv("fastfood_calories.csv") %>%
    mutate(item = rtweet::plain_tweets(item))

# Input mean function
input_mean <- function(x){
    return(round(ifelse(is.na(x), mean(x, na.rm = TRUE), x),2))
}

y <- suppressWarnings(x %>% select_if(is_numeric) %>%
                          mutate_all(input_mean) %>%
                          mutate(item = x$item, 
                                 restaurant = x$restaurant) %>%
                          select(restaurant, item, calories:protein))

##Retrieve Distance Matrix
set.seed(100)
d <- y %>% select(calories:protein) %>%
    mutate_all(funs(scale(.,center = FALSE,scale = TRUE))) %>%
    sample_frac(0.1) %>%
    dist()

##Apply cluster solution
cc <- hclust(d, method = "average")
plot(cc, cex = 0.5)

cutree(cc, k=6)
table(cutree(cc, k=6))
