remove(list =ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/tidy_tuesday_week_1/")
library(tidyverse);library(viridis);library(gridExtra)


x <- readxl::read_excel("us_avg_tuition.xlsx") %>%
  inner_join(readxl::read_excel("/Users/harrocyranka/Desktop/code/list_of_regions.xlsx", sheet = 3) %>%
               select(Region, State)) %>%
  select(State, Region, `2004-05`:`2015-16`) %>%
  mutate(Region = ifelse(Region == "Western Overseas", "West", Region))

x <- x %>% gather(year, price, -State,-Region)

##Points are not that good
x %>%
  ggplot(aes(x = year, y = price, color = Region)) + 
  geom_jitter(width = 0.11, size = 4, alpha = 0.7, shape = 19) + 
  theme_minimal() + 
  scale_color_viridis(option = "B",discrete = TRUE, begin = 0.6) + 
  theme(
    legend.position = "bottom"
  )

##Let's try geom_ridges
library(ggridges)

x %>% ggplot(aes(x = price, y = fct_rev(year), fill = Region)) + 
  geom_density_ridges_gradient(color = "black",
                               show.legend = FALSE,
                               alpha = 0.3,
                               panel_scaling = TRUE,
                               quantile_lines = TRUE, quantiles = 2) + 
  facet_wrap(~fct_reorder(Region,price,mean), scales = "free") + 
  scale_x_continuous(limits = c(1000,18000),
                     breaks = seq(0,18000,3000),
                     labels = c("0","3,000","6,000","9,000",
                                "12,000", "15,000", "18,000")) + 
  theme_minimal()  + 
  labs(x = "Average tuition",
       y = "Academic year",
       title = "Distribution of average tuition price by region",
       subtitle = "Solid line denotes the median",
       caption = "Tidy tuesday week 1: Tuition price by state") + 
  scale_fill_viridis(alpha = 0.9, option = "C", discrete = TRUE, begin = 0.3) + 
  theme(
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 12),
    strip.text = element_text(size = 10, face = "bold")
  )

##Trying gg highlights
track_by_region <-x %>% group_by(Region, year) %>%
  summarise(price = median(price)) %>%
  mutate(year_code = as.numeric(factor(year)))

x %>%
  mutate(year_code = as.numeric(factor(year))) %>%
  ggplot(aes(x = year_code, y= price, group = State)) +
  geom_line(show.legend = FALSE, color= "gray", size = 3,
            alpha = 0.5) + 
  geom_line(aes(x = year_code, y = price, color = Region),
            data = track_by_region, inherit.aes = FALSE, 
            size = 5, alpha = 0.5) + 
  theme_minimal() + 
  theme(
    legend.position = "bottom",
    panel.grid.minor.x = element_blank(),
    axis.title.x = element_text(size = 13, face = "bold"),
    axis.text.x = element_text(size = 11),
    axis.title.y = element_text(size = 13, face = "bold"),
    axis.text.y = element_text(size = 11),
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 13)
  ) + 
  scale_x_continuous(limits = c(1, 12),
                     breaks = seq(1,12,by = 1),
                     labels = unique(x$year)) + 
  scale_y_continuous(limits = c(2000,16000),
                     breaks = seq(2000,20000,4000),
                     labels = c("2,000","6,000","10,000","14,000", "18,000")) + 
  labs(x = "\nYear",
       y = "Median tuition price",
       title = "Change in median tuition price",
       subtitle = "Median regional changes highlighted") +
  guides(color = guide_legend(
    title.position = "top",
    title.hjust = 0.5,
    label.position = "top",
    keywidth = 4
  )) + 
  viridis::scale_color_viridis(alpha = 0.9, option = "C", discrete = TRUE)


##Change in standard deviation
sd_median <-x %>% group_by(Region, year) %>%
  summarise(sd = sd(price),
            mean = mean(price)) %>%
  mutate(rows = row_number())

##Do a different graph for median and for std_deviation
##Std deviation
stdev <- sd_median %>% gather(measure, value, -Region, -year, -rows) %>%
  filter(measure == "sd") %>%
  ggplot(aes(x = rows, y = value, color = Region))  + geom_line(size =2) + 
  geom_point(show.legend = FALSE, size = 4) + 
  scale_y_continuous(breaks = seq(750,2500,by = 250),
                     labels = prettyNum(as.character(seq(750,2500,by = 250)),big.mark = ",")) + 
  scale_x_continuous(breaks = seq(1, 12, by = 1),
                     labels = unique(x$year)) +
  theme_minimal() + 
  labs(x = "\nYear", y = "Standard deviation",
       title = "Change in standard deviation of average tuition price\n") + 
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "black", size = 0.1),
    axis.title.x = element_text(size = 15, face = "bold"),
    axis.title.y = element_text(size = 15, face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 20, face = "bold"),
    plot.background = element_rect(fill = "gray95"),
    legend.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 11)
  ) + 
  guides(color= guide_legend(
    title.position = "top",
    title.hjust = 0.5,keywidth = 0.5,label.position = "bottom"
  ))
  
##
mean <- sd_median %>% gather(measure, value, -Region, -year, -rows) %>%
  filter(measure == "mean") %>%
  ggplot(aes(x = rows, y = value, color = Region))  + geom_line(size =2) + 
  geom_point(show.legend = FALSE, size = 4) + 
 scale_y_continuous(breaks = seq(5000,13000,by = 1000),
                      labels = prettyNum(as.character(seq(5000,13000,by = 1000)),big.mark = ",")) + 
  scale_x_continuous(breaks = seq(1, 12, by = 1),
                     labels = unique(x$year)) +
  theme_minimal() + 
  labs(x = "\nYear", y = "Mean",
       title = "Change in average tuition price by region\n") + 
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "black", size = 0.1),
    axis.title.x = element_text(size = 15, face = "bold"),
    axis.title.y = element_text(size = 15, face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 20, face = "bold"),
    plot.background = element_rect(fill = "gray95"),
    legend.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 11)
  ) + 
  guides(color= guide_legend(
    title.position = "top",
    title.hjust = 0.5,keywidth = 0.5,label.position = "bottom"
  ))


gridExtra::grid.arrange(mean,stdev)