remove(list =ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/rviz/day_2_30_day_of_graphs/")
library(tidyverse)

x <- readxl::read_excel("crecimiento.xlsx") %>%
    gather(pais, crec, -ano)

x %>%
    ggplot(aes(x = ano,y = crec/100, color = str_to_title(pais))) + 
    geom_hline(yintercept = 0, size =0.5, linetype = 2, color = "yellow") +
    geom_line(size =0.5) + 
    geom_point(size =0.5,show.legend = FALSE) + 
    labs(x = "\nAño", y = "%", title = "Crecimiento del PIB (% anual) en Colombia y Venezuela",
         subtitle = "Datos obtenidos del Banco Mundial",
         caption = "#30diasdegraficos",
         color = "") + 
    scale_x_continuous(
        breaks = seq(1960,2020,by = 10)
    ) + 
    scale_y_continuous(labels = scales::percent) + 
    scale_color_manual(values = c("dodgerblue","firebrick1")) + 
    theme_minimal() + 
    theme(
        axis.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        plot.background = element_rect(fill = "gray80"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray20", size = 0.05),
        text =element_text(family = "Corbel"),
        axis.line = element_line(size = 0.5,color = "gray20"),
        legend.position = "bottom"
    ) + 
    guides(color = guide_legend(label.position = "bottom"))
    