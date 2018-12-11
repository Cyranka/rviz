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

x_1 %>% mutate(month = floor_date(inspection_date,unit = "month")) %>%
    group_by(month, boro, grade) %>%
    tally() %>%
    filter(!is.na(grade)) %>% 
    group_by(month, boro) %>%
    mutate(prop_a = n/sum(n)) %>%
    filter(grade == "A" & boro != "Missing") %>%
    ggplot(aes(x = month, y = prop_a, color = boro)) + 
    geom_line(show.legend = FALSE) + 
    geom_point(show.legend = FALSE) + 
    geom_smooth(method = "lm", se = FALSE, show.legend = FALSE,size = 0.5,
                linetype = 2) +
    facet_wrap(~boro, nrow = 5, scales = "free_x") + 
    theme_minimal() + 
    theme(
        text = element_text(color = "white", family = "Arial"),
        strip.text = element_text(color = "white", size = 11, face = "bold"),
        axis.text = element_text(color = "white", face = "bold", size = 11),
        axis.title = element_text(color = "white", size = 12, face = "bold"),
        plot.background = element_rect(fill = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(size =0.05, color = "white", linetype = 2),
        panel.grid.major.x = element_line(size = 0.05,color = "white", linetype = 2),
        axis.line.x = element_line(size =0.5, color = "white"),
        plot.title = element_text(size = 15, face = "bold"),
        plot.subtitle = element_text(size = 13, face = "bold"),
        plot.caption = element_text(size = 10)
    ) + 
    scale_color_viridis(discrete = TRUE, option = "D",begin = 0.4,end = 0.9) + 
    labs(x = "\nTime", y = "% of A grades",
         title = "Monthly proportion of A grades by Borough",
         subtitle = "Dotted line represents linear trend",
         caption = "Tidy tuesday week 37: NYC Restaurant Inspections\nMissing values were removed") + 
    scale_y_continuous(limits = c(0.4,1),
                       breaks = c(0.5,0.6,0.7,0.8,0.9,1),
                       labels = c("50%","60%","70%","80%","90%","100%"))