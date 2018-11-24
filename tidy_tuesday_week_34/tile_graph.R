remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/tidy_tuesday_week_34/")
library(tidyverse);library(lubridate);library(gganimate);library(viridis)

##
regions <- readxl::read_excel("list_of_regions.xlsx", sheet = 3) %>%
  select(Region, State) %>% magrittr::set_colnames(c("region", "state"))

us <- readxl::read_excel("us_locations_fixed.xlsx") %>%
  separate(location, into = c("place", "state"), sep = ", ") %>%
  left_join(regions) %>%
  mutate(region = ifelse(region == "Western Overseas", "West", region)) %>%
  mutate(lat = ifelse(state == "Hawaii", 25.93719, lat),
         lon = ifelse(state == "Hawaii", -107.2428, lon)) %>%
  mutate(cause_of_death = factor(cause_of_death, levels = c("Shot", "Stabbed", "Beaten", "Other", "Not reported")))


us %>% group_by(region, year, cause_of_death) %>% tally()  %>%
  ggplot(aes(x = year, y = fct_rev(cause_of_death), fill = n,label = n)) + geom_tile(color = "black") + 
  facet_wrap(~factor(region,levels = c("South", "Midwest", "Northeast", "West", "Southwest")),
             scales = "free", ncol = 1) + geom_text(fontface = "bold", color = "white", size = 3) + 
  scale_fill_viridis(option = "A", alpha = 1, begin = 0.5, end= 0.8,
                     breaks = seq(1,12,by = 1)) + 
  theme_minimal() + 
    scale_x_continuous(breaks = seq(2007, 2018, by = 1)) + 
  labs(x = "\nYear", y = "Cause of death",
       title = "Murders of transgender individuals in the United States since 2007",
       subtitle = "Regions sorted by total number of cases",
       caption = "Tidy tuesday week 34: Transgender day of remembrance") + 
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill = "gray30"),
    axis.text = element_text(color = "white",size = 11),
    axis.title = element_text(color = "white",size = 13, face = "bold"),
    strip.text = element_text(color= "white", size = 12, face = "bold"),
    panel.grid.major = element_line(color = "white", size = 0.04),
    panel.grid.minor = element_line(color = "white", size = 0.04),
    axis.line.x = element_line(color = "gray90", size = 0.5),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color= "white", face = "bold"),
    plot.title = element_text(color = "white", size = 16, face = "bold"),
    plot.subtitle = element_text(color= "white", size = 14),
    plot.caption = element_text(color = "white", size = 12),
    text = element_text(family = "Calibri")
  ) + 
  guides(fill = guide_colorbar(title = "Total deaths",
                               title.position = "top",title.hjust = 0.5,barwidth = 15))


