remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

library(tidyverse)

x <- read_csv("phd_by_field.csv") %>%
    filter(major_field == "Biological and biomedical sciences") %>%
    mutate(field = ifelse(field == "Neurosciences, neurobiologye",
                          "Neurosciences/neurobiology",field))

epi <- filter(x, field %in% c("Epidemiology","Neurosciences/neurobiology"))


epi %>%
    ggplot(aes(x = year, y = n_phds, label = field,
               color = field)) + 
    ggrepel::geom_text_repel(size = 2.5,
                             point.padding = 0.3,
                             color = "black",
                             segment.color = "white",
                             show.legend = FALSE, family = "Roboto") + 
    geom_point(size = 3, show.legend = FALSE) +
    geom_point(color = "black", size = 4, shape = 1, show.legend = FALSE) + 
    scale_x_reverse(breaks = seq(2008, 2017,1)) + 
    scale_y_continuous(breaks = seq(0, 1100,100)) + 
    labs(x = "Total",
         y = "Year",
         title = "Doctorates awarded in the biological and medical science field",
         subtitle = "Epidemiology and Neuroscience highlighted") + 
    coord_flip() + 
    geom_jitter(data = x %>% drop_na(),
               aes(x = year, y = n_phds),
               color = "gray70",width = 0.15,
               size = 4,alpha = 0.1) + 
    theme_minimal() + 
    theme(
        text = element_text(family = "Roboto"),
        plot.title = element_text(size = 15,face = "bold"),
        axis.line.x = element_line(size = 0.5,color = "gray20"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.2)
    )