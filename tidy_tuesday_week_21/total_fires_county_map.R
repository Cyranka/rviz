remove(list =ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/tidy_tuesday_week_21/")
library(tidyverse);library(urbnmapr)


read_and_filter_ca <- function(files){
  k <- read_csv(files) %>% filter(state %in% c("CA", "California"))
  return(k)
}

get_files <- grep("buzzfee",list.files(),value = TRUE)

##
j <- lapply(get_files, read_and_filter_ca)
ca_buzzfeed <- bind_rows(lapply(1:7, function(i) j[[i]] %>% mutate_all(as.character)))


##Data Frames to Use
with_county <- ca_buzzfeed %>% select(state,fips_name,fips_code,fire_year, discovery_date, fire_size,latitude,longitude) %>%
  filter(!is.na(fips_name)) %>% dplyr::rename(county = fips_name)

##Lat/Long
with_latlong <- ca_buzzfeed %>% select(state,fips_name,fips_code, fire_year, discovery_date, fire_size,latitude,longitude) %>%
   dplyr::rename(county = fips_name)


##County California Map
fires_by_county <- with_county %>% group_by(county) %>% 
  summarise(total_fires = n()) %>%
  mutate(county = paste0(county, " County")) %>%
  mutate(fire_group = cut_number(total_fires,5, boundary = 0, labels = FALSE))


##Labels for Map
get_labels <- fires_by_county %>% 
  group_by(fire_group) %>% 
  summarise(min = min(total_fires), max = max(total_fires)) %>%
  mutate(my_labels = paste0(min, "-",max))

#library(RColorBrewer)
colors <- brewer.pal(5,"YlOrRd")

##groups to remove: c("06037.3","06083.1","06111.3")

groups_to_filter <- c("06037.3","06083.5","06083.2","06083.3","06083.4","06111.3")

fires_by_county %>% inner_join(urbnmapr::counties, by = c("county" = "county_name")) %>%
  filter(state_abbv == "CA" & !group %in% groups_to_filter) %>%
  ggplot(aes(long, lat, group = county, fill = as.character(fire_group))) + 
  geom_polygon(color = "black", size = .25)+ theme_minimal() + 
  scale_fill_manual(values = colors,labels = get_labels$my_labels) + 
  labs(x = "", y = "", fill = "Total fires",
       title = "Total wildfires in California",
       subtitle = "County level map",
       caption = "Source: CalFire/Buzzfeed\nEach color represents groups of roughly equal size") + 
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.background = element_rect(fill = "gray20"),
        legend.text = element_text(color = "white", size = 12),
        legend.title = element_text(color = "white", size = 12,face = "bold"),
        plot.title = element_text(color = "white", size = 20,face = "bold"),
        plot.subtitle = element_text(color = "white", size = 15,face = "bold"),
        plot.caption =  element_text(color = "white", size = 9,face = "bold"),
        panel.grid.minor = element_blank())


##Break down between human and natural
fires_by_county <- with_county %>% group_by(county) %>% 
  summarise(total_fires = n()) %>%
  mutate(county = paste0(county, " County")) 

causes_tibble <- tribble(
  ~stat_cause_descr, ~cause,
  "Miscellaneous", "Other",
  "Equipment Use", "Human",
  "Lightning", "Nature",
  "Arson", "Human",
  "Debris Burning","Human",
  "Missing/Undefined", "Other",
  "Campfire", "Human",
  "Children", "Human",
  "Smoking", "Human",
  "Powerline", "Human",
  "Railroad", "Human",
  "Fireworks", "Human",
  "Structure", "Human"
)

added_causes <- ca_buzzfeed %>% inner_join(causes_tibble) %>%
  select(state,fips_name,fips_code,cause,fire_year, discovery_date, fire_size,latitude,longitude) %>%
  dplyr::rename(county = fips_name) %>%
  filter(!is.na(county))

county_cause_group <- added_causes %>%
  group_by(county, cause) %>% 
  summarise(total_fires = n()) %>% ungroup() %>%
  mutate(county = paste0(county, " County")) %>%
  spread(cause, total_fires, fill = 0) %>%
  gather(cause, total_fires, -county) %>%
  group_by(cause) %>%
  arrange(cause, total_fires) %>%
  mutate(row = row_number()) %>%
  mutate(color_group = cut_width(row,width = 10,boundary = 0))

##Do Humans first
library(RColorBrewer)
new_colors <- brewer.pal(6,"YlOrBr")

county_cause_group %>% 
  mutate(color_group = factor(color_group, labels = c("1-10",
                                                      "11-20",
                                                      "21-30",
                                                      "31-40",
                                                      "41-50",
                                                      "50-59"))) %>%
  inner_join(urbnmapr::counties, by = c("county" = "county_name")) %>%
  filter(state_abbv == "CA" & !group %in% groups_to_filter & cause!="Other") %>% 
  ggplot(aes(long, lat, group = county, fill = color_group)) + 
  geom_polygon(color = "black", size = .25) + theme_minimal() + 
  scale_fill_manual(values = c("blue4","blue1",
                               "lightblue3","honeydew3",
                               "indianred3", "firebrick3")) + 
  facet_wrap(~cause, scales = "free") + 
  labs(x = "", y ="",
       title = "County comparison between human and nature related wildfires",
       subtitle = "Counties ranked from least number of incidents to greatest number of incidents",
       fill = "County rank",
       caption = "Source CalFire/Buzzfeed\nCounty ranked #1 is the one with the lowest number of wildfires") + 
  theme(#panel.grid.major = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.text = element_text(color = "black", size = 12),
        legend.title = element_text(color = "black", size = 12,face = "bold"),
        plot.title = element_text(color = "black", size = 20,face = "bold"),
        plot.subtitle = element_text(color = "black", size = 13,face = "bold"),
        plot.caption =  element_text(color = "black", size = 9,face = "bold"),
        #panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "azure4"),
        strip.text = element_text(color = "white", face = "bold")
        )
  