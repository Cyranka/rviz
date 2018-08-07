remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("")
library(tidyverse);library(tidytext)

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


##Loop through file, return matched locations, and save files that will be used for maps
x <- read_csv("hadley_api.csv")
loc <- cities_twitter_2(x)
loc %>% select(name, count, percent) %>% write_csv("locations_for_table.csv")
loc %>% write_csv("locations_for_map.csv")

create_twitter_city_map <- function(csv_file, define_title,define_color, latitude_column= "lat", longitude_column ="lon", size_column = "count"){
  library(plotly)
  library(shiny)
  
  ##Bubble Map##
  df2 <- readr::read_csv(csv_file)
  
  g <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    showland = TRUE,
    landcolor = toRGB("white"),
    subunitwidth = 1,
    countrywidth = 1,
    subunitcolor = toRGB("black"),
    countrycolor = toRGB("black")
  )
  
  
  p <- plot_geo(df2, locationmode = 'USA-states', sizes = c(100, 5000)) %>% add_markers(
    x = ~lon, y = ~lat, size = ~count,color = ~count,colors = c(define_color),hoverinfo = "text",
    text = ~paste(df2$name, "<br />", df2$count/1e6, " million") 
  )  %>% layout(title = define_title, geo = g,showlegend = FALSE, width = 2000,height = 1000) %>%
    hide_colorbar()
  
  p2 <<- p
}  

p2 <- create_twitter_city_map("locations_for_map.csv", "Matched US locations map for @hadleywickham's twitter followers",define_color = "#018270")
htmlwidgets::saveWidget(p2, "twitter_city_map.html", selfcontained = FALSE)
webshot::webshot("twitter_city_map.html", file = "newplot.png")