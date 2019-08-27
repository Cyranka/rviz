remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harro.cyranka/Desktop/rviz/")
library(tidyverse);library(LaCroixColoR);library(ggforce)

nuclear_explosions <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-20/nuclear_explosions.csv") %>%
  filter(!is.na(country))


nuclear_explosions <- nuclear_explosions %>%
  mutate(country = factor(country, levels = c("USA", "USSR","UK",
                                              "FRANCE", "CHINA",
                                              "INDIA", "PAKIST"),
                          labels = c("United States", "Soviet Union",
                                     "United Kingdom", "France", "China",
                                     "India", "Pakistan")),
         atmospheric = ifelse(depth <0, "Atmospheric", "Underground"))

color_palette <- LaCroixColoR::lacroix_palette("PeachPear", type = "continuous",n = 7)

nuclear_explosions %>%
  group_by(country, year, atmospheric) %>%
  summarize(
    total = n()
  ) %>%
  arrange(year) %>%
  mutate(total = ifelse(atmospheric == "Underground", total*-1,total)) %>%
  ggplot(aes(x = year, y = total, fill = country)) + 
  geom_col() + 
  theme_minimal() + 
  theme(
    text = element_text("Roboto Condensed", color = "white"),
    plot.title = element_text(size = 18,family = "Roboto Condensed", face = "bold", color = "white"),
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "white", face = "bold"),
    plot.background = element_rect(fill = "black"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(size = 0.05,color = "gray70"),
    axis.line.x = element_line(color = "white")
  ) + 
  labs(x = "\nYear", y = "Undergound             Atmospheric\n",
       title = "Timeline of nuclear explosions",
       subtitle = "The United States and the Soviet Union were responsible for over 85% of total explosions") + 
  guides(fill = guide_legend(nrow = 1, title = "Country", title.position = "top", 
                             title.hjust = 0.5, label.position = "bottom",
                             keyheight = 0.3)) + 
  scale_y_continuous(limits = c(-180, 180), 
                     breaks = c(-150,-125,-100,-75,
                                -50,-25,0,25,50,75,100,125,150),
                     labels = c("150", "125", "100", "75","50","25",
                                "0","25","50","75", "100", "125","150")) + 
  scale_fill_manual(values = color_palette) + 
  scale_x_continuous(breaks = seq(1945,1995, by = 5)) + 
  annotate("segment", x = 1963, xend = 1963, y = 0, yend = 50, color = "gray70") + 
  annotate("text", y =70, x = 1967, label = "Partial Test Ban Treaty (1963)\nProhibits atmospheric nuclear tests", color = "white",
           size = 3, family = "Roboto Condensed", fontface = "bold") + 
  annotate("segment", x = 1996, xend = 1996, y = 0, yend = 50, color = "gray70") + 
  annotate("text", y =60, x = 1990, label = "Comprehensive Nuclear Test Ban Treaty (1996)", color = "white",
           size = 3, family = "Roboto Condensed", fontface = "bold")
  




  