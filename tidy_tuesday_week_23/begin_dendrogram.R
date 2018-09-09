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
    sample_frac(0.2) %>%
    dist()

##Apply cluster solution
cc <- hclust(d, method = "average")
plot(cc, cex = 0.5)

cutree(cc, k=10)
table(cutree(cc, k=10))

##Get principal components to plot
set.seed(100)
pcFit <- prcomp(y %>% select(calories:protein) %>%
    mutate_all(funs(scale(.,center = FALSE,scale = TRUE))) %>%
    sample_frac(0.2))

pcFit$x[,1:2] %>% as_tibble() %>%
    mutate(cluster = cutree(cc, k = 10)) %>%
    filter(cluster <3) %>%
    ggplot(aes(x= PC1, y = PC2, color = as.character(cluster))) +
    geom_point(show.legend = FALSE) + theme_minimal()
    