remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

library(tidyverse);library(geofacet);library(lubridate)
setwd("/Users/francisco06121988/Desktop/rviz/covid_geofacet/")

library(tidyverse);library(geofacet);library(lubridate)
# Brazil map --------------------------------------------------------------
br <- readxl::read_excel("Book1.xlsx", sheet = 1) %>%
    janitor::clean_names() %>%
    gather(state, total, -date,-stat) %>%
    mutate(total = ifelse(is.na(total), 0,total),
           state = str_to_upper(state),
           date = ymd(str_extract(date, "2020-[0-9]{2}-[0-9]{2}"))) %>%
    select(-stat)

br %>%
    ggplot(aes(x = date, y =(total + 1))) + 
    geom_area(fill = "indianred2") +
    geom_line(color= "indianred4") + 
    scale_y_log10(labels = scales::comma) + 
    scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
    geofacet::facet_geo(~state, grid = br_states_grid1) +
    labs(x = "", y = "",
         title = "Total COVID-19 cases in Brazil by state (cumulative)",
         subtitle = "Y-axis in log-scale",
         caption = "#FunctionFriday: Brazil grid\nSource: Wikipedia\nRetrieved on July 22,2020") + 
    theme_bw() + 
    theme(
        text = element_text(family = "Calibri"),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 14, face = "bold"),
        strip.background = element_rect(color = "gray90"),
        panel.grid.major = element_line(color = "gray80", size =0.1),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size =0.5),
        axis.ticks = element_blank()
    )
    
# Australia map -----------------------------------------------------------
aus <- readxl::read_excel("Book1.xlsx", sheet = 2) %>%
    janitor::clean_names() %>%
    gather(state, total, -date) %>%
    mutate(total = ifelse(is.na(total), 0,total),
           state = str_to_upper(state),
           date = ymd(str_extract(date, "2020-[0-9]{2}-[0-9]{2}")),
           total = parse_number(str_replace_all(total, "\\[[a-z]{1,3}\\]",""))) %>%
    filter(date >= ymd("2020-03-01"))

aus %>%
    ggplot(aes(x = date, y =(total + 1))) + 
    geom_area(fill = "dodgerblue1") +
    geom_line(color= "dodgerblue3") + 
    scale_y_log10(labels = scales::comma) + 
    scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
    geofacet::facet_geo(~state, grid = aus_grid1) +
    labs(x = "", y = "",
         title = "Total COVID-19 cases in Australia by state (cumulative)",
         subtitle = "Y-axis in log-scale",
         caption = "#FunctionFriday: Australia grid\nSource: Wikipedia\nRetrieved on July 22,2020") + 
    theme_bw() + 
    theme(
        text = element_text(family = "Calibri"),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 14, face = "bold"),
        strip.background = element_rect(color = "gray90"),
        panel.grid.major = element_line(color = "gray80", size =0.1),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size =0.5),
        axis.ticks = element_blank()
    )


