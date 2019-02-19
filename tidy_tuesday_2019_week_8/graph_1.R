remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

library(tidyverse)

x <- read_csv("phd_by_field.csv")

props_by_year <- x %>%
    group_by(broad_field, year) %>%
    summarise(total_phds = sum(n_phds, na.rm = TRUE)) %>%
    group_by(year) %>% arrange(year) %>%
    mutate(prop = total_phds/sum(total_phds))

props_by_year %>%
    ggplot(aes(y = prop,
               x = reorder(broad_field, prop),
               fill = prop)) + 
    geom_col(show.legend = FALSE) +
    coord_flip() + 
    facet_wrap(~year, ncol = 1, scales = "free_x") + 
    labs(x = "Field", y = "% of total",
         title = "Distribution of doctorates awarded by field",
         subtitle = "Data grouped by broad field") + 
    scale_y_continuous(labels = scales::percent,
                       limits = c(0,0.41)) + 
    theme_minimal() + 
    theme(
        plot.title = element_text(size = 15,face = "bold",hjust = 0),
        plot.subtitle = element_text(size = 12, hjust = 0),
        text =element_text(family = "Roboto", color = "white"),
        axis.text = element_text(family = "Roboto", color = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.1,color = "gray40"),
        plot.background = element_rect(fill = "gray20"),
        strip.background = element_rect(fill = "gray30",color = "gray30",size = 0.5),
        strip.text = element_text(family = "Roboto", color = "white", size= 8)
    ) + 
    scale_fill_viridis_c(option = "B", begin = 0.5,end = 0.8,direction = -1)