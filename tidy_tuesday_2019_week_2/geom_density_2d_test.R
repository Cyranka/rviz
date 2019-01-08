rm(list = ls())
options(scipen = 999)
options(stringsAsFactors = FALSE)

library(tidyverse);library(viridis)


##
x <- read_csv("IMDb_Economist_tv_ratings.csv")

y <- x %>% filter(share >0 & av_rating > 6)

##
euc_dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
com <- c(mean(y$av_rating),mean(log(y$share,base = 10)))

y <- y %>% mutate(dist_from_center = sapply(1:nrow(y), function(i)euc_dist(c(y$av_rating[i],log(y$share[i],10)),com)))


##
y%>%
    ggplot(aes(x = share, y= av_rating)) + 
    geom_point(aes(color = dist_from_center),
               size = 4,
               alpha = 0.8) + 
    scale_x_log10() +
    geom_density_2d(size = 1, color = "white",alpha = 0.9) + 
    theme_minimal() + 
    theme(
        plot.title = element_text(size = 17, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 12),
        axis.title.x = element_text(size = 13, face = "bold"),
        axis.title.y = element_text(size = 13, face = "bold"),
        axis.text.y = element_text(size = 12, color = "white"),
        axis.text.x = element_text(size = 12, color = "white"),
        text = element_text(color = "white", family = "Roboto"),
        legend.position = "bottom",
        plot.background = element_rect(fill=  "gray40"),
        panel.grid = element_line(size = 0.1,linetype = 2),
        panel.grid.minor = element_blank(),
        legend.text = element_text(face = "bold"),
        legend.title = element_text(face = "bold")
    ) + 
    scale_color_viridis(option = "D",
                        begin = 0.3,
                        breaks = c(0.1,2.5),
                        labels = c("Points closer to the mean", "Points further from the mean")) + 
    labs(x = "Share (log 10 scale)",
         y = "Average rating",
         color = "Distance from the mean vector",
         title = "2-D density estimate of the distribution of average rating and share",
         subtitle = "Plot created from 2,212 observations",
         caption = "Tidy tuesday 2019 - Week 2\nTV Ratings") + 
  guides(color = guide_colorbar(title.position = "top",title.hjust = 0.5,
                                barwidth = 15,barheight = 1))
  