remove(list = ls())
options(scipen = 999)


library(tidyverse)
library(urbnmapr)


x <- readxl::read_excel("data.xlsx") %>%
    janitor::clean_names() %>%
    filter(str_detect(county," of ", negate = TRUE))

fips <- readxl::read_excel("cofipsia.xls", sheet = 2)

y <- x %>% 
    mutate(county_fips = as.character(fips$fips)) %>%
    gather(candidate, total, -county,-county_fips) 




y %>% filter(candidate == "sanders") %>%
    inner_join(counties) %>%
    ggplot(aes(x =long, y = lat, fill = total,group = group)) + 
    geom_polygon(color = "black", size =0.05) + 
    scale_fill_viridis_c(option = "A",
                         labels = scales::percent, 
                         begin = 0.1, end = 0.9)+ 
    hrbrthemes::theme_ipsum_rc() + 
    theme(
        panel.background = element_rect(fill = "gray90", color = "gray90"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title  = element_blank()
    ) + 
    labs(title = "Bernie Sanders performance in the 2016 Iowa caucus",
         subtitle = "Sanders had his best performance in Jefferson County (72.7% of SDE)",
         fill = "Sanders %",
         caption = "Data: Boston Globe") + 
    guides(fill = guide_colorbar(ticks = FALSE, barheight = 10))


# Winner by county --------------------------------------------------------
y %>%
    spread(candidate, total) %>%
    mutate(winner = ifelse(clinton > sanders, "Clinton", "Sanders")) %>%
    inner_join(counties) %>%
    ggplot(aes(x =long, y = lat, fill = winner,group = group)) + 
    geom_polygon(color = "black", size =0.05) + 
    hrbrthemes::theme_ipsum_rc() + 
    theme(
        panel.background = element_rect(fill = "gray90", color = "gray90"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title  = element_blank()
    ) + 
    labs(title = "Winners of the 2016 Iowa caucus by county",
         subtitle = "Sanders won 40 counties in the 2016 primary",
         fill = "Winner",
         caption = "Data: Boston Globe") + 
    scale_fill_manual(values = c("skyblue2","seagreen1"))
    
y %>%
    spread(candidate, total) %>%
    mutate(winner = ifelse(clinton > sanders, "Clinton", "Sanders")) %>%
    count(winner)