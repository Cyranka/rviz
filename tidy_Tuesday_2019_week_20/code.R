 remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/rviz/")
library(tidyverse)

wine_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv") %>%
    select(-X1)

# Start -------------------------------------------------------------------
wine_ratings %>% 
    mutate(country = ifelse(country == "US", "United States", country)) %>%
    filter(!is.na(price)) %>%
    filter(!is.na(country)) %>%
    group_by(country) %>% filter(n()>50) %>%
    ggplot(aes(x = reorder(country, price,FUN = median), y = price)) + 
    labs(x = "Country\n", y = "\nPrice", 
         title = "Distribution of wine prices by country",
         subtitle = "Countries sorted by median price",
         caption = "Only countries with more than 50 wines rated are displayed") + 
    geom_boxplot(outlier.alpha = 0.2,
                 alpha  =0.5,
                 outlier.color = "cyan",
                 color = "white",
                 fill = "gray40"
                 ) + coord_flip() + 
    scale_y_log10(labels = scales::comma) + 
    hrbrthemes::theme_modern_rc(axis_title_size = 15) + 
    theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "white",size = 0.05,linetype = 1)
    ) 



