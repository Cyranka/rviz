remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/rviz/tidy_tuesday_week_30/")
library(tidyverse);library(lubridate);library(ggridges)


##Need to remove duplicates
x <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-10-23/movie_profit.csv") %>%
    mutate(release_date = mdy(release_date))%>%
    filter(!worldwide_gross ==0) %>%
    mutate(year = year(release_date)) %>%
    filter(year >=1980) %>%
    select(-X1)


my_breaks <- x %>%
    mutate(prof_over_budget = (domestic_gross + worldwide_gross)/production_budget)

x %>%
    mutate(prof_over_budget = (domestic_gross + worldwide_gross)/production_budget) %>%
    group_by(genre, year) %>%
    summarise(median_prof = median(prof_over_budget, na.rm = TRUE)) %>%
    ggplot(aes(x = year,
               y = genre,
               fill = log(median_prof,2))) + 
    geom_tile(color = "gray20") + 
    labs(x = "Year",
         y = "Genre",
         title = "Median gross over profit across genres since 1980",
         subtitle = "Horror movies in 1980 had the highest median gross over profit") + 
    scale_x_continuous(limits = c(1979,2018),
                       breaks = c(seq(1980, 2016,by = 3),2018)) + 
    scale_fill_viridis_c(
        option = "A",
        breaks = c(-0.25,0.5,1,2,3,4,5,5.5),
        labels = round(2**c(-0.25,0.5,1,2,3,4,5,5.5),1)) + 
    theme_minimal() + 
    theme(
        text = element_text(family = "Roboto"),
        plot.title = element_text(size = "15", face = "bold"),
        plot.subtitle = element_text(size = 12),
        legend.position = "bottom",
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.1,color = "gray20"),
        axis.text.y = element_text(margin = margin(l = 1))
    ) + 
    guides(fill = guide_colorbar(title = "Median gross over profit",
                                 title.position = "top",
                                 title.hjust = 0.5,barwidth = 15,barheight = 0.5
                                 ))
    


