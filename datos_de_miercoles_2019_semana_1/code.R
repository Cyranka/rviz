remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/rviz/datos_de_miercoles_2019_semana_1/")
library(tidyverse);library(lubridate)

x <- readr::read_delim("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-10/partidos.txt",delim = "\t")

winners <- c("Francia",
             "Brasil",
             "Uruguay",
             "Argentina",
             "Alemania","Alemania occidental",
             "Italia","España","Inglaterra")

# Goals -------------------------------------------------------------------
define_function <- function(country){
    p <- x %>%
        filter(equipo_1 == country) %>%
        select(anio, equipo_1, equipo_1_final) %>%
        magrittr::set_colnames(c("anio", "equipo","goles")) %>%
    bind_rows(x %>% filter(equipo_2 == country) %>%
                  select(anio, equipo_2, equipo_2_final) %>%
                  magrittr::set_colnames(c("anio", "equipo", "goles"))) %>%
        arrange(anio)
    
    return(p)
}

totals <- bind_rows(lapply(winners, define_function)) %>%
    mutate(equipo = ifelse(equipo == "Alemania occidental", "Alemania",equipo))

cum_goals <- totals %>%
    group_by(anio, equipo) %>%
    summarise(
        goles_ano = sum(goles)
    ) %>%
    arrange(equipo, anio) %>%
    group_by(equipo) %>%
    mutate(cum = cumsum(goles_ano)) %>%
    ungroup()

last_point <- cum_goals %>%
    group_by(equipo) %>% top_n(1, anio)

cum_goals %>%
    ggplot(aes(x = anio, y = cum,
               color = equipo,
               alpha = anio)) +
    geom_line(show.legend = TRUE) + 
    geom_point(aes(x = anio, y = cum,
                   color = equipo), data = last_point) + 
    ggrepel::geom_text_repel(aes(x = anio, y = cum,
                                 label = equipo), data = last_point,
                             nudge_x = 3) + 
    theme_minimal() + 
    theme(
        text = element_text(color = "gray80", family = "Roboto Condensed"),
        axis.text = element_text(color = "gray80", family = "Roboto Condensed"),
        axis.title = element_text(color= "gray80", family = "Roboto Condensed"),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 10,face = "bold"),
        legend.position = "none",
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(size =0.1, color = "gray80"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.1, color = "gray80"),
        plot.background = element_rect(fill = "black")
    ) + 
    scale_x_continuous(breaks = seq(1930, 2018, by = 4),
                       expand = c(0.1,2)) + 
    scale_y_continuous(breaks = seq(0,250,by = 50),
                       limits = c(0,260)) + 
    labs(x = "Mundial",
         y = "Goles (cumulativo)",
         title = "Brasil y Alemania son los máximos goleadores en mundiales",
         subtitle = "Los dos países son los únicos que han marcado más de doscientos goles",
         caption = "Datos de miércoles 2019, primera semana: mundiales de fútbol\nAnálisis restricto a los equipos campeones del mundo") + 
    guides(color = guide_legend(title = "Equipo",
                                title.position = "top"
                                ),
           alpha = NULL) + 
    scale_color_manual(values = c("gray97",
                                  "powderblue",
                                  "lightgreen",
                                  "firebrick1",
                                  "deepskyblue3",
                                  "white",
                                  "dodgerblue2",
                                  "yellow1")) + 
    scale_alpha_continuous(range = c(0.3,1))




