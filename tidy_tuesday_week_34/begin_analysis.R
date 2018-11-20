remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/tidy_tuesday_week_34/")
library(tidyverse);library(lubridate);library(gganimate);library(viridis)

x <- tdor::tdor

##Clean description variable
us <- filter(x, Country == "USA") %>%
  mutate(Description = rtweet::plain_tweets(Description)) %>%
  mutate(Description = str_replace_all(Description, "> >|>", "")) %>%
  mutate(Description = tm::stripWhitespace(str_replace_all(Description, 'Transrespect versus Transphobia Worldwide.*$',"")))


#Add year
us <- us %>%
  mutate(Year = ifelse(Year == 2006, 2007, Year))

##
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

##
us <- us %>% select(Longitude, Latitude, `Cause of death`,Location,Year) %>%
  magrittr::set_colnames(c("lon", "lat", "cause_of_death","location","year")) %>%
  mutate(cause_of_death = firstup(cause_of_death)) %>%
  mutate(cause_of_death = ifelse(cause_of_death %in% c("Beaten", "Shot","Stabbed", "Not reported"),
                                 cause_of_death, "Other"))
##writexl::write_xlsx(us, "locations_to_fix.xlsx")


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


##Grab names

###
map_us <- urbnmapr::states  %>% filter(!state_abbv %in% "AK") %>%
  ggplot(aes(long, lat, group = group)) + 
  geom_polygon() + 
  geom_polygon(size = .45,color = "gray35", fill = "gray90") +
  theme_minimal() +
  geom_point(aes(lon, lat,
                 color = cause_of_death), inherit.aes = FALSE, data = us,size = 6, alpha = 0.7) + 
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(size = 15),
        legend.position = "bottom",
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12)) + 
  scale_color_viridis(option = "B",discrete = TRUE,begin = 0.3, end = 0.8) + 
  labs(title = "Murders of transgender individuals in the United States",
       subtitle = "Year: {closest_state}",
       caption = 'Tidy tuesday week 34: Transgender day of remembrance') + 
  guides(color = guide_legend(title.position ="top",
                              title = "Cause of death",
                              title.hjust = 0.5,override.aes = list(alpha = 0.9),nrow = 1,label.position = "bottom"))   + 
  transition_states(
    year,
    transition_length =0.001,
    state_length = 0.5
  )


animate(map_us, nframes = 120, height = 600, width =1000)

anim_save("trans_murders.gif")