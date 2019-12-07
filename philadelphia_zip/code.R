remove(list =ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/rviz/philadelphia_zip/")
library(tidyverse);library(leaflet)


tickets <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-03/tickets.csv")
philly_shape <- sf::read_sf("Zipcodes_Poly/Zipcodes_Poly.shp")

top_violations <- tickets %>%
    group_by(violation_desc) %>%
    tally(sort = TRUE) 

by_zip <- tickets %>%
    group_by(zip_code) %>%
    tally(sort = TRUE) %>% drop_na()%>%
    mutate(zip_code = as.character(zip_code))

palette <- colorNumeric(
    palette = "YlOrRd",
    domain = log(by_zip$n,base = 10)
)

tickets <- tickets %>%
    mutate(day = lubridate::ymd(lubridate::floor_date(issue_datetime, unit = "day")),
           tod = paste0(lubridate::hour(issue_datetime),":",lubridate::minute(issue_datetime)))

set.seed(300)
philly_shape %>%
    inner_join(by_zip,
               by = c("CODE" = "zip_code")) %>%
    leaflet() %>%
    addProviderTiles("CartoDB") %>%
    addPolygons(
        color = ~palette(log(n,base = 10)),
        weight = 1,
        label = ~paste0("Zip: ", CODE),
        group = "Choropleth",
        highlightOptions = highlightOptions(weight = 1,
                                            color = "black",
                                            bringToFront = TRUE)
    ) %>%
    addLegend("bottomright",
              pal = palette,
              values = ~log(n,10),
              title = "Total tickets by zip code",
              group = "Legend",
              labFormat = labelFormat(transform =function(x){round(10**x,0)})) %>%
    addCircleMarkers(data = tickets %>% sample_frac(0.0005),
                     radius = 0.5,
                     color = "#706F6F",
                     popup =  ~paste0("Violation: ", str_to_title(violation_desc),"<br>",
                                      "Date: ", day,"<br>",
                                      "Time: ", tod,"<br>",
                                      "Agency: ", issuing_agency,"<br>",
                                      "Fine: ", fine),
                     group = "Sample tickets") %>%
    addLayersControl(
        baseGroups = c("OSM (default)"),
        overlayGroups = c("Choropleth", "Legend", "Sample tickets")
    )

