remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/tidy_tuesday_week_31/")
library(tidyverse);library(viridis);library(lubridate);library(gridExtra)

x <- read_csv("r_downloads.csv")

##
month_levels <- c("Oct-2017","Nov-2017","Dec-2017",
                  "Jan-2018", "Feb-2018","Mar-2018",
                  "Apr-2018", "May-2018","Jun-2018",
                  "Jul-2018","Aug-2018","Sep-2018",
                  "Oct-2018")

##
x_2 <- x %>% group_by(date,os) %>% tally() %>%
  mutate(os = str_to_title(os)) %>%
  filter(!is.na(os)) %>%
  mutate(month_levels = factor(paste0(month(date,abbr = TRUE,label = TRUE),"-",year(date)),levels = month_levels))


medians <- x_2 %>% group_by(month_levels, os) %>% summarise(median = median(n)) %>% mutate(os = case_when(
  os == "Osx"~ "OSX",
  os == "Src"~ "SRC",
  TRUE ~ "Windows"
))

x_2 %>% mutate(os = case_when(
  os == "Osx"~ "OSX",
  os == "Src"~ "SRC",
  TRUE ~ "Windows"
)) %>%
  ggplot(aes(month_levels, y = n, color = os)) +
  geom_jitter(width = 0.3,alpha = 0.5, size = 3, show.legend = FALSE) + 
  geom_line(aes(x = month_levels, y = median, group = os),
            data = medians, inherit.aes = FALSE,
            linetype = 2) + 
  geom_boxplot(alpha = 0.5, width = 0.5,col = "black",outlier.alpha = 0) + 
  theme_minimal() + facet_wrap(~os, ncol = 1,scales = "free") + 
  labs(x = "\nMonth", y = "Total donwloads",
       title = "Distribution of downloads by month and operational system",
       subtitle = "Dotted line connects the median for each month",
       caption = "Tidy tuesday week 31\nR and R package downloads") + 
  theme(
    text = element_text(family = "Roboto"),
    axis.title.x = element_text(hjust = 0.5, family = "Roboto", face = "bold", size = 11),
    axis.title.y = element_text(hjust =0.5, family = "Roboto", face = "bold",size = 11),
    axis.text.y = element_text(size = 10),
    axis.line.y = element_blank(),
    axis.line.x = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "gray90"),
    plot.title = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(size = 10)
  )