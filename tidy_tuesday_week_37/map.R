rm(list =ls())

library(tidyverse)
library(lubridate)
library(viridis)
x <- read_csv("nyc_restaurants.csv")


# Basic variable manipulation ----------------------------------------------

x <- x %>% mutate_if(is.character, rtweet::plain_tweets)

x <- x %>% mutate(dba = str_to_title(dba),
                  boro = str_to_title(boro),
                  inspection_date = lubridate::mdy(inspection_date)) %>%
    arrange(dba, inspection_date) 

# Get most recent grade ---------------------------------------------------
##Get rid of entry errors: year > 1900

x_1 <- x %>% group_by(dba) %>%
    filter(lubridate::year(inspection_date) > 2014) %>% 
    ungroup()


y <- read_csv("deleted_islands_2.csv")


x_2 <- x_1 %>%
    group_by(zipcode,boro, grade) %>%
    tally() %>%
    filter(!is.na(grade)) %>% 
    group_by(zipcode, boro) %>%
    mutate(prop_a = n/sum(n)) %>%
    filter(grade == "A" & boro != "Missing")

y %>% left_join(x_2, by = c("ZIPCODE" = "zipcode")) %>%
    ggplot(aes(X, Y, group = ZIPCODE,
               fill = prop_a)) + 
    geom_polygon(color = "black", size = 0.1) + 
    scale_fill_viridis(alpha = 1, option = "A",
                       breaks = seq(0.4,1,by = 0.1),
                       labels = c("40%", "50%", "60%","70%",
                                  "80%", "90%", "100%")) + 
    theme_minimal() + 
    theme(
        axis.text = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(size = 35, face = "bold"),
        plot.subtitle = element_text(size = 28, face = "bold"),
        plot.caption = element_text(size = 20, face = "bold"),
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 20, face = "bold"),
        plot.background = element_rect(fill = "gray40")
    ) + 
    labs(fill = "Proportion of A grades") + 
    guides(fill = guide_colorbar(title.position = "top",
                                 title.hjust = 0.5,
                                 barwidth = 30,barheight = 1)) + 
    labs(title = "Proportion of A grades by zip code",
         subtitle = "Gray areas represent missing",
         caption = "\nTidy tuesday week 37: NYC Restaurant Inspections")