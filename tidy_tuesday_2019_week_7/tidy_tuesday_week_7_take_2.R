
remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)


setwd("/Users/harro.cyranka/Desktop/")
library(tidyverse);library(tidytext)


x <-read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-12/fed_r_d_spending.csv")

correlations <- x %>%group_by(year) %>%
  summarise(
    total_budget = max(total_outlays)/1000000000,
    gdp = max(gdp)/1000000000
  ) %>% ungroup() %>%
  mutate(
    gdp_index = gdp/min(gdp)*100,
    rd_index = total_budget/min(total_budget)*100,
    period= case_when(
      between(year, 1)
    )
  ) %>%
  group_by(period) %>%
  mutate(correlation = cor(gdp_index, rd_index))

x %>%
  group_by(year) %>%
  summarise(
    total_budget = max(total_outlays)/1000000000,
    gdp = max(gdp)/1000000000
  ) %>%
  mutate(
    gdp_index = gdp/min(gdp)*100,
    rd_index = total_budget/min(total_budget)*100
  ) %>%
  select(-total_budget, -gdp) %>%
  gather(measure, value, -year) %>%
  ggplot(aes(x =year, y = value, color = measure)) + 
  geom_point(size = 2, show.legend = FALSE) + 
  geom_line(size = 1) + 
  theme_minimal() + 
  theme(
    text = element_text(family = "Roboto"),
    plot.title = element_text(size = 15, face ="bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )  + 
  labs(x = "\nYear",
       y = "Index", 
       title = "Growth in GDP and Total R&D Outlays",
       subtitle = "1976 has index 100 for both measures",
       caption = "Tidy Tuesday 2019 Week 7: Federal R&D Spending") + 
  scale_x_continuous(breaks = seq(1976,2016,by = 4)) + 
  scale_y_continuous(breaks = seq(100,1100,by = 100), labels = scales::comma) + 
  scale_color_manual(values = c("darkorange", "firebrick3"),
                     labels = c("GDP", "R&D\nOutlays")) + 
  guides(color = guide_legend(title = "Legend",
                              title.position = "top",
                              title.hjust = 0.5,
                              label.position = "bottom",
                              keywidth = 4,override.aes = list(size = 3))) + 
  annotate("rect", xmin = 1976, xmax = 1980, ymin = 0,ymax = 1100,fill = "darkblue", alpha = 0.2) + 
  annotate("rect", xmin = 1980, xmax = 1992, ymin = 0,ymax = 1100,fill = "red", alpha = 0.2) + 
  annotate("rect", xmin = 1992, xmax = 2000, ymin = 0,ymax = 1100,fill = "darkblue", alpha = 0.2) + 
  annotate("rect", xmin = 2000, xmax = 2008, ymin = 0,ymax = 1100,fill = "red", alpha = 0.2)  + 
  annotate("rect", xmin = 2008, xmax = 2016, ymin = 0,ymax = 1100,fill = "darkblue", alpha = 0.2) + 
  annotate("rect", xmin = 2016, xmax = 2018, ymin = 0,ymax = 1100,fill = "red", alpha = 0.2)  + 
  annotate("text", x = 1978, y = 1000,color = "black", label = "Carter (D)\n", family = "Roboto", fontface = "bold") + 
  annotate("text", x = 1986, y = 1000,color = "black", label = "Reagan/Bush Sr. (R)\n", family = "Roboto", fontface = "bold") + 
  annotate("text", x = 1996, y = 1000,color = "black", label = "Clinton (D)\n", family = "Roboto", fontface = "bold") + 
  annotate("text", x = 2004, y = 1000,color = "black", label = "Bush (R)\n", family = "Roboto", fontface = "bold") + 
  annotate("text", x = 2012, y = 1000,color = "black", label = "Obama (D)\n", family = "Roboto", fontface = "bold") 


