remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

library(tidyverse);library(viridis)
x <- read_csv("baltimore_bridges.csv")


x <- x %>% filter(yr_built >= 1910) %>%
  mutate(decade = (yr_built - yr_built %%10)) 


x %>% ggplot(aes(x = decade,
                 y = avg_daily_traffic,
                 color = factor(bridge_condition, levels = c("Poor", "Fair", "Good")))) + 
  geom_jitter(width = 2.5, alpha =0.6, size = 2, show.legend = FALSE) + 
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 15,face = "bold"),
    plot.subtitle = element_text(size = 12, face = "bold"),
    plot.background = element_rect(fill = "black"),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white", face = "bold", size = 12),
    axis.text.x = element_text(angle = 45),
    strip.text = element_text(color = "white", face = "bold", size = 12),
    axis.title.y = element_text(size = 15, face= "bold"),
    axis.title.x = element_text(size = 15, face = "bold"),
    panel.grid.minor = element_blank(),
    #axis.line.x = element_line(size = 0.5,color = "white"),
    panel.grid.major = element_line(size =0.1, linetype = 2),
    strip.background = element_rect(color = "gray", fill = "black")
  ) + 
  facet_wrap(~factor(bridge_condition, levels = c("Poor", "Fair", "Good")), scales = "free", nrow = 1) + 
  labs(y = "Average daily traffic\n",
       x = "\nDecade built",
       title = "Dailly traffic by decade built faceted by bridge condition",
       subtitle = "Bridges built before 1910 were excluded") + 
  scale_x_continuous(limits = c(1900, 2020),
                     breaks = seq(1910, 2010, by = 10)) + 
  scale_y_continuous(limits = c(0,250000),
                     breaks = seq(0,250000, 50000),
                     labels = prettyNum(seq(0,250000, 50000), big.mark = ",")) + 
  guides(color = guide_legend(title.position = "top",
                              title.hjust = 0.5)) + 
  scale_color_manual(values = c("brown1","yellow","green"))
  