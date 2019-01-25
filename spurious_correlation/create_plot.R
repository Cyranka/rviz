remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)


library(tidyverse);library(readxl);library(hrbrthemes)

x <- read_excel("spurious.xlsx") %>%
  mutate(divorce_i = divorce_rate/5*100,
         margarine_i = margarine_ppc/8.2*100) %>%
  select(-c(divorce_rate:margarine_ppc)) %>%
  gather(variable, value, -year)
  

x %>%
  ggplot(aes(x = year, y = value, group = variable,color = variable)) + 
  geom_line(size = 2) + geom_point(size = 4, show.legend = FALSE) + 
  scale_x_continuous(breaks = seq(2000,2009,by = 1)) + 
  scale_y_continuous(limits = c(0,100)) + theme_ipsum_rc() + 
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = c(0.1,0.4),
    
  ) + 
  labs(x = "Year", 
       y = "Index",
       color = NULL,
       title = "Divorce rate in Maine correlates with per capita consumption of margarine",
       subtitle = "Correlation: 99.3% (r = 0.993)",
       caption = "Data sources: National Vital Statistics and\n U.S Department of Agriculture\nOriginal: tylervigen.com") +
  scale_color_manual(values = c("black", "firebrick4"),
    labels = c("Divorce rate in Maine","Margarine consumed"))

