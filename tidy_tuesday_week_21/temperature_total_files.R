remove(list =ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/tidy_tuesday_week_21/")
library(tidyverse);library(readxl);library(lubridate)

x <- read_csv("california_fires_1.csv") %>% dplyr::rename(year = year_) %>%
  mutate(month = month(ymd(alarm_date)))
y <- read_excel("avg_temperature.xlsx") %>%
  rename(mean_temperature = Value)
z <- x %>% left_join(y, by = c("year" = "Date", "month" = "Month")) 

k <- z %>% group_by(year, month) %>%
  summarise(total = n(),
            avg_temperature = mean(mean_temperature,na.rm = TRUE))  %>%
  filter(!is.na(month))

k %>%
  ggplot(aes(x = avg_temperature, y = total)) + geom_point() + theme_bw()


