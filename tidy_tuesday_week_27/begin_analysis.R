remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/rviz/tidy_tuesday_week_27/")
library(tidyverse);library(lubridate)

x <- read_csv("us_births_2000_2014.csv")




all_fridays <- x %>% group_by(year, month, date_of_month) %>%
    summarise(total = sum(births)) %>%
    mutate(proportion_day = total/sum(total)) %>%
    mutate(date = lubridate::ymd(paste0(year,"-",month,"-", date_of_month))) %>%
    filter(date_of_month == 13)
    
friday_13th <- x %>%
    filter(date_of_month == 13 & day_of_week %in% c(5,6,7)) %>%
    mutate(date = lubridate::ymd(paste0(year,"-",month,"-", date_of_month)))

for_points <- all_fridays %>% filter(date %in% friday_13th$date)


other_days_average <- x %>% group_by(year, month, date_of_month) %>%
    summarise(total = sum(births)) %>%
    mutate(proportion_day = total/sum(total)) %>%
    group_by(date_of_month) %>% summarise(avg_prop = mean(proportion_day)*100)

all_fridays %>% ggplot(aes(x = date, y = proportion_day*100)) + 
    geom_line(size = 0.1, linetype = 1) + theme_bw() +
    labs(x = "Date", y = "% of total births") + 
    theme(
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45),
        axis.ticks.x = element_blank(),
        panel.border = element_blank()
    ) + 
    scale_y_continuous(limits = c(1,5), breaks = seq(1,5,1)) + 
    geom_point(aes(x = date, y = proportion_day*100, col = weekdays(date)), data = for_points, size = 0.7) + 
    scale_x_date(date_breaks = "1 year",date_labels =  "%b %Y",limits = c(ymd("1999-06-01"),ymd("2016-06-01"))) + 
    geom_hline(yintercept = 3.22,size = 0.3, col = "blue", linetype = 1,alpha =0.5) + 
    geom_hline(yintercept = 3.34, size =0.3, col = "red", linetype = 1, alpha =0.5) + 
    annotate("text",x = lubridate::ymd("2015-12-01"),y = 3.37,label = "Mean for day 14: 3.34%" ,
             col = "firebrick", size =2, alpha =1) + 
    annotate("text",x = lubridate::ymd("2015-12-01"),y = 3.25,label = "Mean for day 13: 3.22%" ,
             col = "blue", size =2, alpha =1)
    

