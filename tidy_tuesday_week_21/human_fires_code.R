remove(list =ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/tidy_tuesday_week_21/")
library(tidyverse);library(readxl);library(lubridate)

x <- read_csv("california_fires_1.csv") %>% dplyr::rename(year = year_)
y <- read_excel("cal_fire_units.xlsx") 
z <- x %>% left_join(y, by = c("unit_id" = "unit_name"))

##
causes_by_year <- z %>% group_by(year,fire_cause) %>%
  summarise(totals = n()) %>%
  group_by(year) %>%
  mutate(percent = totals/sum(totals))

l <- (causes_by_year %>%
  ggplot(aes(x = year, y = percent*100)) + geom_line(size = 1.1, alpha = 0.5) + 
  geom_smooth(se = FALSE, size = 1.1) + theme_bw() + 
  labs(x = "Year", y ="% of total fires",
       title = "% of total fires by year",
       subtitle = "Marked increase in the proportion of human-related fires since the 1950s",
       caption = "Tidy Tuesday Week 21: California Fires") + facet_wrap(~fire_cause, nrow = 3) + 
  theme_minimal())


##Size by Decades
dec_labels <- c("1950-1960","1961-1970","1971-1980", "1981-1990", "1991-2000", "2001-2010","2010-2017")
decades <- z %>% mutate(decade = cut_width(year,10,boundary = 1950)) %>%
  arrange(year) %>% mutate(decade = factor(decade,labels = dec_labels))
  
decades %>%
  group_by(year, decade, fire_cause) %>%
  summarise(mean_log_gis_acres = mean(log(gis_acres,base = 2),na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = mean_log_gis_acres, color = decade)) +
  geom_line(size = 1.4) + theme_bw() + geom_smooth(size = 0.3,color = "black", se = FALSE) + 
  labs(x = "Year", color = "Decade", y = "Mean log acres of fire",
       title = "Mean log acres of fire of fire-related incidents",
       subtitle = "There has been a visible decrease in the acreage of fires \nSmooth line in black",
       caption = "Tidy Tuesday Week 21: California Fires\nY-axis is Base 2 Log") + facet_wrap(~fire_cause, nrow = 3)


x %>% mutate(month = month(ymd(alarm_date),label = TRUE)) %>% count(month, sort = TRUE)