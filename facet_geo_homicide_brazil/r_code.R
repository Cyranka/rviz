remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/rviz/facet_geo_homicide_brazil/")
library(readxl);library(tidyverse);library(geofacet)

x <- read_excel("wiki_data.xlsx") %>%
    gather(year, rate, -name) %>%
    mutate(name = str_trim(name)) %>%
    arrange(name) 

br_facet <- geofacet::br_states_grid1

x <- x %>% group_by(year) %>%
    mutate(year_median = median(rate,na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(diff_from_median = rate - year_median)

x %>%
    mutate(year = parse_number(year)) %>%
    ggplot(aes(x = year, y = rate, fill = diff_from_median)) + 
    geom_col() + 
    geofacet::facet_geo(~name, grid = br_facet) + 
    theme_minimal() + 
    scale_x_continuous(breaks = c(2000,2008,2017)) + 
    theme(
        text = element_text(family = "Roboto Condensed"),
        axis.text = element_text(family = "Roboto Condensed"),
        axis.title = element_text(family = "Roboto Condensed"),
        plot.title = element_text(family = "Roboto Condensed", size = 16, face = "bold"),
        plot.subtitle = element_text(family = "Roboto Condensed", size = 10, face = "bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size =0.2),
        legend.title = element_text(family = "Roboto Condensed", size = 7),
        legend.text = element_text(family = "Roboto Condensed",size = 7),
        legend.position = c(0.8,1),
        legend.direction = "horizontal"
    ) + 
    labs(x = "\nYear",
         y = "Homicides per 100,000\n",
         title = "Changes in homicide rate in Brazilian states between 2000 and 2017",
         subtitle = "Colors indicate difference to the median state homicide rate",
         caption = "Source: Wikipedia in Portuguese"
         ) + 
    scale_fill_gradient2(low = "royalblue4",
                         high = "firebrick3",
                         mid = "gray98",
                         midpoint = 0,
                         breaks = c(-25,-10,0,10,25,39)) + 
    guides(fill = guide_colorbar(title = "Difference to median homicide rate",
                                 title.position = "top",
                                 title.hjust = 0.5,barheight = 0.3,
                                 barwidth = 10))