remove(list =ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harro.cyranka/Desktop/rviz/")
library(tidyverse);library(socviz)

x <- readr::read_csv("https://raw.githubusercontent.com/5harad/openpolicing/master/results/data_for_figures/combined_data.csv") %>%
    mutate(location = str_to_title(location)) %>%
    rename(county = location)

total_stops <-x %>% group_by(state, county) %>%
    summarise(total_stops = sum(stops_per_year, na.rm = TRUE))

plot_data <-  x %>%
    select(county, state, driver_race, arrest_rate) %>%
    spread(driver_race, arrest_rate) %>%
    filter(!is.na(White),
           Black >0) %>%  
    mutate(region = case_when(
        state %in% c("FL", "NC", "TX", "SC") ~ "South",
        state %in% c("CT","MA", "RI", "NJ", "VT") ~ "Northeast",
        state %in% c("MT", "OH","WI", "IL") ~ "Midwest",
        TRUE ~ "West"
    )) %>%filter(state %in% c("FL"))

# Map for Florida ----------------------------------------------

fl_map <- plot_data %>%
    right_join(urbnmapr::counties %>%
                   filter(state_abbv == "FL"), by = c("state" = "state_abbv",
                                          "county" = "county_name")) %>%
    ggplot(aes(x = long, y = lat, group = group, fill = (Black+Hispanic)/2*100)) + 
    geom_polygon(color = "gray10", size =0.1) + 
    scale_fill_gradient2(low = "steelblue3",mid = "gray90",
                         midpoint = 5.5,na.value = "gray90",
                         high = "firebrick3",
                         breaks = c(0,2,4,6,8,10,12,14),
                         labels = paste0(c(0,2,4,6,8,10,12,14),"%")) + 
    hrbrthemes::theme_ipsum() + 
    labs(title = "Average arrest rates for non-whites in Florida",
         subtitle = "Escambia county has the highest average arrest rate",
         caption = "Tidy Tuesday 2019: Week 12\nStanford open policing project") + 
    theme(
        text= element_text(face = "bold"),
        plot.title = element_text(size = 18), 
        plot.subtitle = element_text(size = 11),
        legend.position ="bottom",
        legend.title = element_text(size = 10,face = "bold"),
        legend.text = element_text(size = 8, face = "bold"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size =0.02, color = "black"),
        plot.background = element_rect(fill = "antiquewhite2")
    ) + 
    guides(fill = guide_colorbar(
        title = "Average arrest rates for non-whites",
        title.position = "top" ,
        title.hjust = 0.5,
        barwidth = 20,
        barheight = 0.5
    ))

k <- plot_data %>%
    mutate(avg = (Black+Hispanic)/2,
           diff = avg - White,
           county = str_replace(county, " County","")) %>%
    top_n(30, diff) %>%
    ggplot(aes(x = reorder(county, diff), y = diff)) + geom_col(fill = "firebrick3") + 
    coord_flip() + 
    scale_y_continuous(labels = scales::percent) + 
    hrbrthemes::theme_ipsum() + 
    labs(x = "County", y = "Difference",
         title = "Difference between average non-white arrest rates and white arrest rates",
         subtitle = "Data refers to the 30 counties where the difference is the largest",
         caption = "Tidy Tuesday 2019: Week 12\nStanford open policing project") + 
    theme(
        text= element_text(family = "Roboto",face = "bold"),
        plot.title = element_text(size = 18), 
        plot.subtitle = element_text(size = 11),
        axis.text = element_text(size = 8),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size =0.02, color = "black"),
        plot.background = element_rect(fill = "antiquewhite2")
    ) 

cowplot::plot_grid(fl_map, k, nrow = 2)