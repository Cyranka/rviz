remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

# setwd("/Users/harrocyranka/Desktop/rviz/tidy_tuesday_week_26/")
library(tidyverse);library(viridis)
x <- read_csv("african_species.csv")

species_by_country <- x %>% group_by(country,kingdom) %>% tally() %>%
  ungroup() %>%
  mutate(country = ifelse(country == "Gambia (the)", "Gambia",
                          ifelse(country == "United Republic of Tanzania", "Tanzania", country))) %>%
  spread(kingdom, n, fill = 0) %>%
  gather(kingdom,n, -country) %>%
  group_by(country) %>% mutate(percent = round(n/sum(n),3)*100) %>%
  arrange(country, percent)

total_species <-  x %>% group_by(country) %>% tally() %>%
  mutate(country = ifelse(country == "Gambia (the)", "Gambia",
                          ifelse(country == "United Republic of Tanzania", "Tanzania", country))) %>%
  rename(total_species = n)


species_by_country %>% inner_join(total_species) %>%
  mutate(kingdom = factor(kingdom, levels = c("Viruses", "Fungi","Chromista", "Bacteria", "Animalia", "Plantae"))) %>%
  ggplot(aes(reorder(country, total_species), percent, fill = kingdom)) + 
  geom_col(col = "gray50") + coord_flip() + theme_minimal() + scale_fill_viridis(option = "D",discrete = TRUE,direction = 1) + 
  labs(x = "Country", y = "% of invasive species by kingdom", fill = "Kingdom",
       title = "Distribution of invasive species by kingdom within African countries",
       subtitle = "Countries sorted by total invasive species\nRight vertical axis shows total invasive species by country",
       caption ="Tidy Tuesday week 26 - Invasive species in Africa") + 
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  guides(fill = guide_legend(nrow = 1, label.position = "bottom", keywidth = 0.5,keyheight = 0.5,
                             title.position = "top", title.hjust = 0.5)) + 
  geom_text(aes(y = constant, x = country, label = total_species), data = total_species %>% mutate(constant = 102.5), inherit.aes = FALSE,
             size = 3)