remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)


library(tidyverse);library(readxl)
x <- read_csv("dma_coordinates.csv") %>%
  magrittr::set_colnames(c("unique_id", "lat", "long","order", "sub_polygon_id","dma")) %>%
  mutate(group = paste0(unique_id, ".", sub_polygon_id))


par(mar=c(0,0,0,0))

x %>%
  ggplot(aes(long, lat, group = group)) + 
  geom_polygon(size = 0.25,fill = "gray90", color = "black") + 
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
  theme_minimal() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(size = 15),
        legend.position = "bottom",
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12)) + 
  theme(plot.margin = unit(c(0,0,0,0), "mm"))