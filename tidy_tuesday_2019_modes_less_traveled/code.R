remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

library(tidyverse)
commute_mode <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-05/commute.csv") %>%
    mutate(state = case_when(
        state == "Ca" ~ "California",
        state == "Massachusett" ~ "Massachusett",
        TRUE ~ state
    ),
    state_region = case_when(
        state == "District of Columbia" ~ "South",
        city == "West Springfield Town city" ~ "Northeast",
        city == "El Paso de Robles (Paso Robles) city" ~ "West",
        TRUE ~ state_region
    )) 

averages <- commute_mode %>%
    group_by(state_region,
             city_size,mode) %>%
    summarize(mean_percent = mean(percent, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(city_size = factor(city_size, levels = c("Small",
                                                    "Medium",
                                                    "Large")))

commute_mode %>%
    group_by(state_region,
             mode) %>%
    summarize(mean_percent = mean(percent, na.rm = TRUE)) %>%
    arrange(mode, desc(mean_percent))


# Biking to work ----------------------------------------------------------
bike <- averages %>%
    filter(mode == "Bike") %>%
    ggplot(aes(x = fct_rev(factor(state_region,
                          levels = c("West",
                                     "Northeast",
                                     "North Central",
                                     "South"))),
               y = mean_percent, 
               fill = city_size,
               label = round(mean_percent,1))) +
    geom_col(position = position_dodge2(),
             color = "gray90", 
             width = 0.5) + 
    scale_y_continuous(limits = c(0,1.5),
                       breaks = c(0,0.4,0.8,1.2, 1.5),
                       labels = c("0.0%","0.4%","0.8%", "1.2%", "1.5%")) + 
    labs(x = "",y = "%\n", fill = "City size",
         title = "Biking to work by region and city size: 2008-2012",
         subtitle = "(Data based on sample. Regions ordered from highest to lowest)",
         caption = "Source: U.S. Census Bureau, American Community Survey, 2008-2012") + 
    theme_minimal(base_family = "Roboto Condensed",base_size = 14) + 
    theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "black",
                                        size = 0.02),
        legend.position = "bottom",
        plot.title = element_text(color = "darkorchid4", face = "bold",
                                  size = 16)
    ) + 
    guides(fill = guide_legend(title.position = "top", 
                                title.hjust = 0.5,
                                label.position = "bottom",keywidth = 3,
                               keyheight = 0.5)) + 
    scale_fill_manual(values = c("khaki",
                                 "indianred1",
                                 "gray64"))

# Walking to work ---------------------------------------------------------
walk <- averages %>%
    filter(mode == "Walk") %>%
    ggplot(aes(x = fct_rev(factor(state_region,
                                  levels = c("Northeast",
                                             "North Central",
                                             "West",
                                             "South"))),
               y = mean_percent, 
               fill = city_size,
               label = mean_percent)) + 
    geom_col(position = position_dodge2(),
             color = "gray90", 
             width = 0.5) + 
    scale_y_continuous(limits = c(0,10),
                       breaks = c(0,2.5,5,7.5,10),
                       labels = c("0.0%","2.5%","5.0%", "7.5%", "10.0%")) + 
    labs(x = "",y = "%\n", fill = "City size",
         title = "Walking to work by region and city size: 2008-2012",
         subtitle = "(Data based on sample. Regions ordered from highest to lowest)",
         caption = "Source: U.S. Census Bureau, American Community Survey, 2008-2012") + 
    theme_minimal(base_family = "Roboto Condensed",base_size = 14) + 
    theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "black",
                                        size = 0.02),
        legend.position = "bottom",
        plot.title = element_text(color = "darkorchid4", face = "bold",
                                  size = 16)
    ) + 
    guides(fill = guide_legend(title.position = "top", 
                               title.hjust = 0.5,
                               label.position = "bottom",keywidth = 3,
                               keyheight = 0.5)) + 
    scale_fill_manual(values = c("khaki",
                                 "indianred1",
                                 "gray64"))

cowplot::plot_grid(bike, walk, nrow = 2)


