remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/rviz/tidy_tuesday_2019_week_15/")
library(tidyverse);library(lubridate)

pensions <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/pensions.csv")
brazil <- pensions %>% filter(country == "Brazil")
selected_labels <- pensions %>%
    filter(country %in% c("Italy","Japan","Mexico", "Turkey","South Korea",
                          "France"))

pensions %>% 
    ggplot(aes(x = pop_65_percent,
               y = gov_spend_percent_gdp
               )) + 
    geom_point(color= "gray80",
               alpha = 0.8) + 
    geom_point(aes(x = pop_65_percent,
                   y = gov_spend_percent_gdp),
               data = brazil,
               color = "red",
               size = 4) + 
    scale_x_continuous(limits = c(0,35)) + 
    scale_y_continuous(limits = c(0,20)) + 
    geom_text(aes(x = pop_65_percent,
                   y = gov_spend_percent_gdp,
                  label = country),
               data = brazil,
               color = "red",
               size = 2.5,
               nudge_y = -1,
               family = "Roboto Condensed") + 
    geom_hline(yintercept = 8.40,
               size = 0.2,
               linetype = 2,
               color= "white") + 
    annotate(geom = "text",
             y = 9.2,
             x = 30,
             label = "OECD average (8.41% of GDP)",
             size = 2,
             color = "white",
             family = "Roboto Condensed") + 
    geom_point(aes(x = pop_65_percent,
                   y = gov_spend_percent_gdp),
               data = selected_labels,
               color = "cyan",
               size = 2.5) + 
    ggrepel::geom_text_repel(aes(x = pop_65_percent,
                  y = gov_spend_percent_gdp,
                  label = country),
              data = selected_labels,
              size = 2.5,
              color = "white",
              nudge_y = -0.6,point.padding = 0.2,nudge_x = 1,
              segment.size = 0.1) + 
    hrbrthemes::theme_ipsum_rc() + 
    labs(x = "Population aged 65 years and over, % of total",
         y = "Government spending on pension benefits\n(% of GDP)",
         title = "Despite a relatively young population, Brazil spends considerably on pensions",
         subtitle = "Data refers to latest year available",
         caption = "Sources: OECD;World Bank;Previdência Social") + 
    theme(
        text = element_text(color = "white"),
        plot.background = element_rect(fill = "gray30"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size =0.1),
        axis.text = element_text(color = "white")
    )