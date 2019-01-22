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

rates <- incarceration_data %>% group_by(state,region,county_name) %>%
  drop_na(prison_population) %>%
  filter(year == max(year)) %>%
  filter(pop_category == "Total")%>%
  mutate(rate_per_100000 = prison_population/population*100000)


prop_aa <- incarceration_data %>%group_by(state,region,county_name) %>%
  drop_na(prison_population) %>%
  filter(year == max(year)) %>%
  filter(pop_category %in% c("Total","Black")) %>%
  select(-prison_population) %>%
  spread(pop_category, population, fill = 0) %>%
  mutate(percent_aa = Black/Total) %>%
  select(state, county_name, region, percent_aa)


##
l <- rates %>% inner_join(prop_aa)
l %>% 
  subset(rate_per_100000 <10000) %>%
  ggplot(aes(x = percent_aa, y = rate_per_100000)) + 
  geom_point(color = "gray50", size = 3,alpha =0.3) + 
  scale_x_continuous(labels = scales::percent) + 
  geom_smooth(aes(color = region),formula = y~x,method = "lm", se = FALSE,size = 2, 
              show.legend = FALSE) + 
  theme_minimal() + 
  facet_wrap(~region,scales ="free",ncol = 4) + 
  labs(x = "\n% African-American", y = "Incarceration rate per 100,000",
       title = "County incarceration rates by % African-American in the population",
       subtitle = "Relationship seems to be stronger in the midwest",
       caption = "Tidy Tuesday 2019, Week 4: Incarceration Rates\nData from the @verainstitute") + 
  scale_y_continuous(labels = scales::comma,
                     limits = c(0,4000)) + 
  theme(text = element_text(family = "Roboto", size = 12),
        plot.title= element_text(face = "bold", size = 15),
        #plot.subtitle = element_text(),
        #plot.background = element_rect(fill = "gray20"),
        axis.title = element_text(face = "bold"),
        #axis.text = element_text(color = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.1, linetype = 2, color = "black"),
        strip.background = element_rect(fill = "gray50",color = "gray50"),
        strip.text = element_text(color = "white", face = "bold"),
        axis.line.x = element_line(size = 0.5))


l <- rates %>% inner_join(prop_aa) %>%
  mutate(percent_aa = percent_aa*100)

lm(rate_per_100000 ~ percent_aa*region, data = l)