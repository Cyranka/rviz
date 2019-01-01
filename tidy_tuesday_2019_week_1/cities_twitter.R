remove(list = ls())
options(scipen = 999)
library(tidyverse);library(lubridate);library(tidytext);library(viridis)

x <- readRDS("rstats_tweets.rds")


url_regex <- '(http|https)[^([:blank:]|\\"|<|&|#\n\r)]+'

x <- x %>% 
    mutate(created_at = ymd(str_extract(x$created_at,
                                        "[0-9]{4}-[0-9]{2}-[0-9]{2}")),
           text = rtweet::plain_tweets(str_replace(text, url_regex, ""))) %>%
    filter(year(created_at) >= 2009)


###
##This function needs improvement to find a better way of removing duplicates and to tag and search cities abroad with the same name as us cities
##basically, this goes through the 1,000 most populous cities/locations and try to match them using regular expressions.
cities_twitter_2 <- function(data_frame){
    library(scales)
    cities <- readxl::read_excel("us_cities_database_with_state.xlsx")
    y <- data_frame
    city_number <- function(k){
        l <- grep(pattern = tolower(cities$city_ascii[k]),tolower(y$location))
        l2 <- length(l)
        print(k)
        return(l2)
    }
    
    
    count <- sapply(1:1000, city_number)
    locations_top <- as.data.frame(cbind(cities[1:1000,],count))
    locations_top <- arrange(locations_top, desc(count))
    locations_top <- filter(locations_top, count > 0)
    locations_top <- locations_top %>% select(city_ascii, lat, lng, population_proper, count) %>%
        arrange(desc(count), desc(population_proper)) %>%
        mutate(duplicates = duplicated(city_ascii)) %>%
        filter(duplicates == FALSE) %>% select(-duplicates) %>%
        dplyr::rename(lon = lng)
    locations_top$percent <- round(locations_top$count/sum(locations_top$count),4)
    locations_top$percent <- percent(locations_top$percent)
    locations_top <- locations_top %>% dplyr::select(city_ascii,population_proper, lat, lon, count, percent) %>%
        magrittr::set_colnames(c("name", "pop", "lat","lon", "count", "percent")) %>% as_tibble()
    return(locations_top)
}


loc <- cities_twitter_2(x)

