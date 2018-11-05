remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)


setwd("/Users/harrocyranka/Desktop/rviz/tidy_tuesday_week_32/")
library(tidyverse);library(viridis);library(lubridate)

x <- read_csv("us_wind.csv")
y <- readxl::read_excel("list_of_regions.xlsx", sheet = 3) %>%
  select(Abbreviation, Region) %>%
  magrittr::set_colnames(c("t_state", "region"))

x_2 <- x %>% inner_join(y) %>%
  filter(t_cap > -9999) %>%
  filter(region != "Western Overseas")
  

##Map
map_us <- urbnmapr::counties %>% filter(!state_abbv %in% c("AK","HI")) %>%
  ggplot(aes(long, lat, group = group)) + 
  geom_polygon() + 
  geom_polygon(size = .55,color = "gray35", fill = "gray20") +
  theme_minimal() + 
  labs(title = "Distribution of wind turbines in the United States",
       subtitle = "Alaska, Hawaii, and overseas territories not shown",
       caption = 'Tidy tuesday week 32: US wind turbine data\nTurbines with missing data on capacity not shown') + 
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 13),
        plot.subtitle = element_text(hjust = 0.5, size = 13),
        legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 11),
        legend.text =element_text(size = 10)) +
  geom_point(aes(xlong, ylat,size = t_cap,
                 color = region),alpha = 0.1,
             inherit.aes = FALSE, data =x_2) + 
  scale_size_continuous(range = c(1,4),
                        breaks = c(0,1000,2000, 3000,4000,5000,6000),
                        labels = c("0","1,000","2,000","3,000","4,000","5,000","6,000")) + 
  guides(color = guide_legend(title.position ="top",
                              title = "Region",
                              title.hjust = 0.5,override.aes = list(alpha = 0.9))) + 
  guides(size = guide_legend(title = "Turbine capacity",
                             title.position = "top",
                             title.hjust = 0.5, nrow = 1,
                             override.aes = list(alpha =0.9))) 

##Waffle plot
library(waffle)
p <- x_2 %>% group_by(region) %>%
  tally() %>% mutate(percent = n/sum(n)) %>%
  arrange(desc(n))

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

waffle_1 <- waffle(parts =c(`Midwest` = 34, `Southwest` = 33, `West` = 27, Northeast = 5, South = 1), rows = 5,
                   colors = c("#F8766D","#00B0F6","#E76BF3","#A3A500", "#00BF7D"), legend_pos = "right",
                   size = 0.2) +
  labs(title = "Proportion of wind turbines by region",
       subtitle = "The midwest contains 34.3% of turbines with known capacity in the United States",
       caption = "Tidy tuesday week 32: US wind turbine data",
       x = "Each square represents ~1% (Proportions were rounded to the nearest tenth)") + 
  theme(
    plot.title = element_text(size = 12,face = "bold"),
    plot.subtitle = element_text(size = 9),
    plot.caption = element_text(size = 9),
    axis.title.x = element_text(size = 9)
  )

waffle_1