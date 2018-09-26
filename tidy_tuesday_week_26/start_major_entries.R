remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/tidy_tuesday_week_26/")
library(readxl);library(tidyverse)

air <- read_csv("total_airports_by_country.csv") %>% select(-date_info)
ports <- read_csv("ports_of_the_world.csv") %>%
  rename(major_ports = n) 


ports_air <- air %>% left_join(ports, by = c("country" = "country"))

##
species_by_country <- read_csv("african_species.csv") %>% group_by(country) %>% tally() %>%
  mutate(country = ifelse(country == "Gambia (the)", "Gambia",
                          ifelse(country == "United Republic of Tanzania", "Tanzania", country)))


with_entries <- species_by_country %>% left_join(ports_air) %>% 
  mutate(major_ports = ifelse(is.na(major_ports), 0, major_ports)) %>%
  mutate(major_entries = total_airports +major_ports)

with_entries %>% ggplot(aes(x = major_entries,y = n)) + geom_point()