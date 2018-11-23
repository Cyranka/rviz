library(tidyverse)

ggplot(aes(x = x, y= x), data = tibble(x = 1:50, y = 1:50)) +
  geom_segment(aes(x = 15, y = 10, xend = 35, yend  = 10), size = 3, lineend = "butt", color = "white") + 
  geom_segment(aes(x = 15, y = 10, xend = 25, yend  = 40), size = 3, lineend = "butt", color = "white") + 
  geom_segment(aes(x = 35, y = 10, xend = 25, yend  = 40), size = 3, lineend = "butt", color = "white") + 
  geom_segment(aes(x = 0 , y = 20, xend = 21, yend =  27.7), size = 2, lineend = "butt", color = "white") + 
  geom_segment(aes(x = 30 , y = 25, xend = 50, yend =  20), size = 4, lineend = "butt", color = "purple") + 
  geom_segment(aes(x = 30 , y = 26, xend = 50, yend =  21), size = 4, lineend = "butt", color = "darkturquoise") + 
  geom_segment(aes(x = 29.5 , y = 27, xend = 50, yend =  22), size = 4, lineend = "butt", color = "forestgreen") + 
  geom_segment(aes(x = 29 , y = 28.3, xend = 50, yend =  23), size = 4, lineend = "butt", color = "gold2") + 
  geom_segment(aes(x = 28.5 , y = 29.3, xend = 50, yend =  24), size = 4, lineend = "butt", color = "darkorange2") + 
  geom_segment(aes(x = 28.3 , y = 30.5, xend = 50, yend =  25), size = 4, lineend = "butt", color = "firebrick2") + 
  geom_segment(aes(x = 35, y = 10, xend = 25, yend  = 40), size = 3, lineend = "butt", color = "white") + 
  scale_y_continuous(limits = c(0,50)) + 
  scale_x_continuous(limits = c(0,50)) + 
  theme_minimal() + 
  theme(
    text = element_text(family = "Roboto"),
    plot.background = element_rect(fill = "black"),
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  ) + 
  annotate(geom = "text", x = 35,y = 0, label = "Pink Floyd: The Dark Side of the Moon", color = "white",
           fontface = "bold", family = "Arial")