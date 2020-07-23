remove(list =ls())
options(scipen = 999)
options(stringsAsFactors = FALSE)
library(tidyverse)


x <- readxl::read_excel("/Users/francisco06121988/Desktop/rviz/day_2_30_day_of_graphs/escanos.xlsx") %>%
    slice(1:10)

color_palette <- c("PSOE" = "red",
                   "PP" = "dodgerblue2",
                   "VOX" = "green",
                   "PODEMOS-IU" = "seagreen4",
                   "Cs" = "orange",
                   "ERC-SOBIRANISTES" = "darkorange1",
                   "JxCAT-JUNTS" = "deeppink2",
                   "PNV" = "seagreen3",
                   "EH Bildu" = "lightgreen",
                   "MÁS PAÍS" = "mediumturquoise")

x %>% 
    ggplot(aes(x = reorder(partido, escanos),
               y = escanos,
               color = partido,
               label = escanos,
               fill = partido)) + 
    geom_point(show.legend = FALSE, size = 3) + 
    geom_col(width = 0.05, show.legend = FALSE) + 
    coord_flip() + 
    scale_color_manual(values = color_palette) + 
    scale_fill_manual(values = color_palette) + 
    geom_text(nudge_y = 4,
              size = 3,
              show.legend = FALSE,
              color = "black",
              family = "CMU Sans Serif") + 
    theme_minimal() + 
    theme(
        text = element_text(family = "Arial Narrow"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.05,color = "black", linetype = 2),
        axis.line = element_line(color = "gray20", size = 0.1),
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text( size = 10),
        axis.title = element_text(face = "bold")
    ) + 
    labs(x = "Partido\n",
         y = "\nEscaños",
         title = "Resultado de escaños del Congreso de los Diputados",
         subtitle = "Elecciones celebradas en el 10 de noviembre de 2019",
         caption = "Otros seis partidos también obtuvieron escaños")



