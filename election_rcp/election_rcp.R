remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/election_rcp/")
library(tidyverse);library(broom);library(ggridges);library(lubridate)

x <- read_excel("polls_house.xlsx") %>%
  mutate(month = factor(lubridate::month(date_2, label = TRUE)))

x %>% select(month, Dem, GOP) %>%
  gather(party, total, -month) %>%
  ggplot(aes(x = total, y = fct_rev(month), fill = party)) +
  geom_density_ridges(scale = 1, alpha = 0.7, color = "white",
                      from = 30, to = 60,
                      quantile_lines = TRUE,
                      quantiles = 2) + 
  theme_minimal() + 
  labs(x = "\n% of total", y = "Month",title = "Distribution of generic congressional vote in major opinion polls",
       subtitle = "Sold line represents the median",
       caption = "Source: Real clear politics") + 
  theme(
    text = element_text(family = "Roboto"),
    legend.position = "bottom",
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_line(linetype = 2),
    panel.grid.major.y = element_line(linetype = 2),
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 13, color = "black"),
    plot.caption = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 13, face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.title.x = element_text(size = 13, face = "bold")
  ) + 
  scale_fill_manual(values = c("steelblue", "firebrick3")) + 
  scale_x_continuous(breaks = c(30,35,40,45,50,55,60),
                     labels = c("30","35","40","45", "50", "55","60")) + 
  guides(fill = guide_legend(title.position = "top",
                             keywidth = 4,
                             keyheight = 0.5,
                             title="Party",
                             title.hjust = 0.5,
                             label.position = "bottom",
                             label.hjust = 0.5,
                             title.vjust = 1))
  

