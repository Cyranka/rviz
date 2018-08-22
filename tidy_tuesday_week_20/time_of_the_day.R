remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/tidy_tuesday_week_20/")
library(tidyverse);library(lubridate)

x <- read_csv("/Users/harrocyranka/Desktop/rviz/tidy_tuesday_week_20/sampled_tweets.csv")

##
k <- x %>% mutate(row = 1:nrow(x)) %>%
  select(publish_date, row) %>%
  separate(publish_date, into = c("date", "time"), sep = " ") %>%
  mutate(day_of_the_week = factor(wday(mdy(date),label = TRUE,week_start = 1)),
         hour = str_extract(time, "^[0-9]{1,2}"))

k2 <- k %>% group_by(day_of_the_week, hour) %>% tally() %>%
  mutate(hour = as.numeric(hour)) %>%
  arrange(day_of_the_week, hour) %>%
  mutate(day_order = as.numeric(day_of_the_week))


k2 %>% arrange(hour, day_of_the_week) %>%
  ggplot(aes(y = reorder(day_order,-day_order), x = hour, fill = n)) + geom_tile(color = "black") + theme_minimal() + 
  scale_fill_continuous(high = "firebrick", low = "white") + 
  scale_y_discrete(labels = c("1" = "Mon", "2" = "Tue","3" = "Wed","4" = "Thu", "5" = "Fri", "6" = "Sat", "7" = "Sun")) + 
  scale_x_continuous(breaks = c(0:23)) + labs(y = "Day of the week", x = "Hour", fill = "Total tweets",
                                              title = "Russian bots: total tweets by hour and day of the week")