remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

library(tidyverse);library(socviz);library(ggrepel)


##
county_data <- socviz::county_data %>% as_tibble() %>%
  select(id, name, state, census_region, pop, black) %>%
  filter(!is.na(state))

##Get incarceration data and filter by most recent year
incarceration_data <- read_csv("prison_population.csv")
incarceration_data2 <- incarceration_data %>% group_by(state,region,county_name) %>%
  drop_na(prison_population) %>%
  filter(year == max(year)) %>%
  filter(pop_category == "Total")

incarceration_data2 <- incarceration_data2 %>%
  mutate(rate_per_100000 = prison_population/population*100000) %>%
  filter(population > 5000)

counties_to_color <- incarceration_data2 %>% arrange(desc(rate_per_100000)) %>% ungroup() %>%
  slice(1:500)

counties_to_color <- counties_to_color %>% inner_join(county_data %>% select(name, state, pop, black), by = c("county_name" = "name", "state" = "state"))

counties_to_color <- counties_to_color %>% 
  mutate(rate_level = cut_number(rate_per_100000,5,boundary = 950)) %>%
  mutate(rate_level_2 = case_when(
    between(rate_per_100000,956,1053) ~ "956 - 1,053",
    between(rate_per_100000, 1053, 1140) ~ "1,053 - 1,140",
    between(rate_per_100000, 1142,1297) ~ "1,142 - 1,297",
    between(rate_per_100000, 1298,1529) ~ "1,298 - 1,529",
    TRUE ~ "1,530 - 3,809"
  )) %>%
  mutate(rate_level_2 = factor(rate_level_2, levels = c("956 - 1,053",
                                                        "1,053 - 1,140",
                                                        "1,142 - 1,297",
                                                        "1,298 - 1,529",
                                                        "1,530 - 3,809")))

counties_to_color %>% group_by(rate_level) %>%
  summarise(min_rate = min(rate_per_100000),
         max_rate = max(rate_per_100000))
###
county_data %>% filter(!str_detect(name, "[0-9]")) %>%
  filter(pop >0) %>%
  ggplot(aes(x = pop, y = black/100)) + 
    geom_point(color = "gray75",
               size = 3,
               alpha = 0.2) + 
  geom_point(data = counties_to_color,
             aes(color = rate_level_2)) + 
  scale_x_log10(labels = scales::comma) + 
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) + 
  scale_color_brewer(type = "seq",palette = "YlOrRd") + 
  labs(x = "\nCounty population (log scale)",
       y = "% African-American",
       color = "\nIncarceration rate per 100,000",
       title = "Top 500 counties by incarceration rate",
       subtitle = "Labeled counties are those with at least 60% African-American population\nCounties with at least 5,000 population",
       caption = "Population data from @kjhealy socviz package\nIncarceration rates used refer to last available year") + 
  ggrepel::geom_text_repel(data = subset(counties_to_color, black > 60),
                           mapping = aes(label = paste0(str_replace(county_name," County",""),",",state)), 
                           size =3, color = "gray90",fontface = "bold") + 
  theme_minimal() + 
  theme(text = element_text(family = "Roboto", size = 12, color = "white"),
        plot.title= element_text(face = "bold", size = 15),
        plot.subtitle = element_text(face = "bold"),
        plot.background = element_rect(fill = "gray20"),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(color = "white"),
        legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(face = "bold", size = 11),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.1)) + 
  guides(color = guide_legend(title.position = "top",
                              title.hjust = 0.5,
                              nrow = 1,
                              label.position = "bottom",override.aes = aes(size = 4)))