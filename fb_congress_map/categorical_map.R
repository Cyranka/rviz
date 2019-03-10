remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/rviz/fb_congress_map/")
library(USAboundaries);library(tidyverse);library(sf)

# Retrieve files ----------------------------------------------------------
setwd("files/")
x <- lapply(list.files(pattern = "\\.csv"), function(i)read_csv(i))
x <- bind_rows(x) %>%
    mutate(district = str_replace_all(district,"US\\:",""),
           state_abbr = str_extract(district,"[A-Z]{2}"),
           cd115fp = str_extract(district, "[0-9]{2}"))

y <- x %>%
    group_by(district) %>%
    mutate(pct = Count/sum(Count)) %>%
    ungroup() %>%
    select(-Count) %>%
    spread(Politics, pct)

set.seed(4)
winners <- x %>%
    mutate(group = case_when(
        Politics %in% c("Very Liberal", "Liberal") ~ "Left",
        Politics %in% c("Very Conservative", "Conservative") ~ "Right",
        TRUE ~ "Moderate"
    )) %>%
    group_by(district,group) %>%
    summarise(
        total = sum(Count)
    ) %>%
    group_by(district) %>%
    mutate(pct = total/sum(total)) %>%
    arrange(district, desc(total)) %>%
    top_n(1,total) %>%
    sample_n(1) %>%
    select(district, group) %>%
    rename(largest_group = group)


z <- y %>% inner_join(winners)

# Create map --------------------------------------------------------------
map_data <- USAboundaries::us_congressional(resolution = "low") %>%
    inner_join(z %>% select(state_abbr, cd115fp, largest_group))
    

continental <- map_data %>% filter(!state_abbr %in% c("HI", "AK")) %>%
    mutate(largest_group = factor(largest_group,
                                  levels = c("Left", "Moderate", "Right"))) %>%
    ggplot(aes(fill = largest_group)) +
    labs(title = "Largest political group on Facebook (March 2017)",
         subtitle = "Data collected from the Facebook Marketing API",
         caption = "Right represents Conservatives and Very Conservatives in each district\nLeft represents Liberals and Very Liberals in each district"
         ) + 
    geom_sf(color = "gray40", alpha = 0.8) + 
    coord_sf(crs = 6350, datum = NA)  + 
    theme_void() + 
    theme(
        text = element_text(family = "Roboto"),
        panel.grid = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 15),
        legend.title = element_text(face = "bold")
    ) + 
    scale_fill_manual(values = c("dodgerblue3",
                                 "gainsboro",
                                 "firebrick2")) + 
    guides(fill = guide_legend(title = "Largest political group",
                              title.position = "top",
                              title.hjust = 0.5,
                              label.position = "bottom",
                              keywidth = 3,
                              keyheight = 0.5))
    

