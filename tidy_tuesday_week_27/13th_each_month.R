remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/tidy_tuesday_week_27/")
library(tidyverse);library(lubridate)

x <- read_csv("us_births_2000_2014.csv") %>%
  mutate(date = ymd(paste0(year, "-", month,"-",date_of_month)))

averages_days <- x %>% group_by(year, month) %>%
  mutate(percent = round(births/sum(births)*100,2)) %>%
  group_by(date_of_month) %>%
  summarise(avg = mean(percent))

for_points <- x %>% group_by(year, month) %>%
  mutate(percent = round(births/sum(births)*100,2)) %>%
  filter(date_of_month == 13 & day_of_week %in% c(5,6,7)) %>%
  mutate(date = ymd(paste0(year, "-", month,"-",date_of_month)))


x %>% group_by(year, month) %>%
  mutate(percent = round(births/sum(births)*100,2)) %>%
  filter(date_of_month == 13) %>%
  mutate(date = ymd(paste0(year, "-", month,"-",date_of_month))) %>% mutate(day_name = weekdays(date, abbreviate = TRUE)) %>%
  mutate(day_name = factor(day_name, levels = c("Mon", "Tue","Wed","Thu", "Fri", "Sat","Sun"))) %>%
  ggplot(aes(x = date, y = percent)) + geom_line(alpha = 1, size = 0.3, color= "gray60") + 
  scale_y_continuous(limits = c(0,6), breaks = seq(0,6,1)) + theme_minimal() + 
  scale_x_date(date_breaks = "1 year") +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, size = 10),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size =12, face = "bold"),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust =0.5),
    plot.caption = element_text(size =8)
  ) +
  labs(x = "Date",
       y = "% of total births",
       title = "Proportion of births on the 13th of each month",
       subtitle = "Data aggregated by month\nWeekends are consistently below average",
       caption = "Tidy Tuesday Week 27 - Births in the United States 2000-2014\nSource:538") + 
  geom_point(aes(x = date, y = percent, color = day_name)) + 
  geom_hline(yintercept = averages_days %>% filter(date_of_month == 13) %>% pull(avg),
             size = 0.5,linetype = 2, color = "red") + 
  annotate("label", x = ymd("2015-01-01"), y = 3.3, label = "Average: 3.2%",
           size =2.4, color = "firebrick3") + 
  scale_color_manual(values = c("firebrick1", "firebrick3",
                                "deepskyblue3","deepskyblue4",
                                "goldenrod2", "goldenrod3","goldenrod4")) + 
  guides(color = guide_legend(title = "Day of the week", title.position = "top", 
                             title.hjust = 0.5,nrow = 1,label.position = "bottom"))
  