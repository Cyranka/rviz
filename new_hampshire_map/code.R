remove(list = ls())
options(scipen = 999)


library(tidyverse)
library(urbnmapr)
library(leaflet)
library(htmltools)
library(sf)

setwd("/Users/francisco06121988/Desktop/rviz/new_hampshire_map/")
x <- readxl::read_excel("results_2016_gop.xlsx") %>%
    janitor::clean_names() %>%
    filter(str_detect(town," of ", negate = TRUE)) %>%
    mutate(trump = ifelse(town == "Ellsworth", 0.55,trump))

palette <- colorNumeric(
    palette = "Reds",na.color = "#c7c7c7",
    domain = x$trump,reverse = FALSE,
)


# Read shapefile ----------------------------------------------------------
nh_sf <- sf::read_sf("New_Hampshire_Political_Boundaries/New_Hampshire_Political_Boundaries.shp")

nh_sf %>%
    left_join(x, by = c("pbpNAME" = "town")) %>%
    leaflet() %>%
    addProviderTiles("CartoDB.DarkMatter",
                     options = providerTileOptions(opacity = 1)) %>%
    addPolygons(
        color = ~palette(trump),
        weight = 0.75,fillOpacity = 0.75,
        label = ~paste0("Town: ", pbpNAME),
        group = "Choropleth",
        highlightOptions = highlightOptions(weight = 1,
                                            color = "red",
                                            bringToFront = TRUE)
    )%>%
    addLegend("bottomright",
              pal = palette,
              values = ~trump,
              title = "2016 GOP Primary: Trump %",
              group = "Legend",na.label = "",
              labFormat = labelFormat(transform =function(x){round(100*x)}))


# Democrat ----------------------------------------------------------------
dem <- readxl::read_excel("results_2016_dem.xlsx") %>%
    janitor::clean_names() %>%
    filter(str_detect(town," of ", negate = TRUE))

my_palette <- colorNumeric(
    palette = "Blues",
    domain = dem$sanders,reverse = FALSE,
)

nh_sf %>%
    left_join(dem, by = c("pbpNAME" = "town")) %>%
    leaflet() %>%
    addProviderTiles("CartoDB.DarkMatter",
                     options = providerTileOptions(opacity = 1)) %>%
    addPolygons(
        color = ~my_palette(sanders),
        weight = 0.75,fillOpacity = 0.75,
        label = ~paste0("Town: ", pbpNAME),
        group = "Choropleth",
        highlightOptions = highlightOptions(weight = 1,
                                            color = "blue",
                                            bringToFront = TRUE)
    )%>%
    addLegend("bottomright",
              pal = my_palette,
              values = ~sanders,
              title = "2016 Dem. Primary: Sanders %",
              group = "Legend",na.label = "",
              labFormat = labelFormat(transform =function(x){round(100*x)}))