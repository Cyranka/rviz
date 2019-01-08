rm(list = ls())
options(scipen = 999)
options(stringsAsFactors = FALSE)

library(tidyverse);library(viridis)

x <- read_csv("IMDb_Economist_tv_ratings.csv")

x %>% filter(share > 0 & av_rating >6) %>%
    ggplot(aes(x = share, y= av_rating)) + 
    geom_point(aes(color = av_rating),
               size = 4,
               alpha = 0.2) + 
    scale_x_log10() +
    geom_density_2d(size = 1, color = "gray90",alpha = 0.7) + 
    theme_minimal() + 
    theme(
        plot.background = element_rect(fill=  "black"),
        panel.grid = element_line(size = 0.1,linetype = 2),
        panel.grid.minor = element_blank()
    ) + 
    scale_color_viridis(option = "A") + 
    labs(x = "Share (log 10 scale)",
         y = "Average rating") 