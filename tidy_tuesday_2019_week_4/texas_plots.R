remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)


library(tidyverse);library(urbnmapr);library(viridis)

x <- read_csv("incarceration_trends.csv")

hfac::describe(x) 
l <- x %>% select(year, fips, state, county_name,jail_from_ice) %>%
  filter(jail_from_ice >0 ) %>%
  arrange(state,year, county_name) %>%
  group_by(state, county_name) %>%
  filter(year == max(year)) %>%
  filter(year >=2010) %>%
  group_by(year, state,county_name) %>%
  summarise(total_ice = sum(jail_from_ice))

##

list_of_breaks <- log(c(1,2,4,8,16,32,64,128,256,512,867), base = 2)
map <- counties %>% left_join(l %>% mutate(total_ice = round(total_ice,0)),
                       by = c("county_name" = "county_name", "state_abbv" = "state")) %>%
  filter(state_abbv %in% c("TX")) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = log(round(total_ice,0),2))) +
  geom_polygon(color = "gray60", size = 0.15,
               show.legend = TRUE) + 
  hrbrthemes::theme_ipsum_rc() + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "bottom"
  ) +
  scale_fill_viridis(option = "B", na.value = "gray10", begin = 0.3,
                     end = 0.9,
                     breaks = list_of_breaks,
                     labels = as.character(c(1,2,4,8,16,32,64,128,256,512, 867))) + 
  guides(fill = guide_colorbar(title = "Total jail from ICE",
                               title.position = "top",barwidth = 15)) + 
  labs(title = "Incarceration by ICE in Texas",
       subtitle = "Numbers refer to the last year with available data in the current decade",
       caption = "Data from the @verainstitute")


# Basic barplot
cleveland <- l %>% filter(state == "TX") %>%
  mutate(county_name = str_replace_all(county_name," County","")) %>%
  mutate(total_ice = round(total_ice, 0)) %>%
  arrange(desc(total_ice)) %>% ungroup() %>%
  slice(1:15) %>%
  ggplot(aes(x = reorder(county_name, total_ice), y = total_ice,label = paste0(total_ice, " (", year,")"))) + 
  geom_point(size = 5, color = "mediumorchid4") + coord_flip() + 
  labs(title = "Incarceration by ICE in Texas: top 15 counties",
       subtitle = "Number in parentheses refers to the last year with available data",
       x = "County", y = "Total ICE", caption = "Data from the @verainstitute") + 
  hrbrthemes::theme_ipsum_rc() + 
  theme(
    text = element_text(family = "Roboto"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 13),
    panel.grid.minor.y = element_blank(),
    panel.grid.major = element_line(size =0.1)
  ) + scale_y_continuous(breaks = c(0,200,400,600,800), expand = c(0,100)) + 
  geom_text(nudge_y = 75, color = "black", size =4)


##
cowplot::plot_grid(cleveland, map,align = "v", ncol = 1)