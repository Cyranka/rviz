remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/ny_election/")
library(tidyverse);library(urbnmapr);library(viridis)

x <- read_csv("ny_election.csv")

##Cities for map
cities <- tribble(
  ~long, ~lat, ~city,
  -73.93,40.73, "New York City",
  -78.87,42.88, "Buffalo",
  -77.61,43.16, "Rochester",
  -76.15, 43.08,"Syracuse",
  -73.75, 42.65, "Albany",
  -73.89, 40.93, "Yonkers"
)


##Create map
x %>% select(-reported) %>%
  mutate(cuomo_pct = round(cuomo/(cuomo+ nixon),3),
         nixon_pct = round(nixon/(cuomo + nixon),3)) %>%
  select(county, cuomo_pct, nixon_pct) %>%
  mutate(county = paste0(county, " County")) %>% mutate(winner = ifelse(cuomo_pct >0.5, "Cuomo", "Nixon")) %>%
  inner_join(counties, by = c("county" = "county_name")) %>%
  filter(state_abbv == "NY") %>%
  ggplot(aes(x = long, y = lat,group = group, fill =factor(winner, levels = c("Nixon", "Cuomo")))) + 
  geom_polygon(color = "gray50", size = .25, alpha =0.5) + theme_minimal() + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_text(face = "bold", size = 15, hjust =0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom") + 
  labs(fill = "Winner",title = "New York state democratic primary: results by county",
       subtitle = "Cities with over 90,000 population displayed on the map", 
       caption ="Source: NY Times") + 
  scale_fill_manual(values = c("chocolate3","dodgerblue1")) + 
  ggrepel::geom_label_repel(data = cities, aes(x = long, y = lat, label = city), inherit.aes = FALSE, nudge_y =0.12,
             nudge_x = -0.12,size = 2.8, alpha = 0.7) + 
  geom_point(data = cities, aes(x = long, y = lat), inherit.aes = FALSE) + 
  guides(fill = guide_legend(keywidth = 4, keyheight = 0.5,label.position = "bottom", label.hjust = 0.5,title.vjust = 1))
  


##Create bar graph
for_breaks <- x %>% select(-reported) %>%
  mutate(cuomo_pct = round(cuomo/(cuomo+ nixon),3),
         nixon_pct = round(nixon/(cuomo + nixon),3)) %>%
  arrange(cuomo_pct) %>% pull(county)

x %>% select(-reported) %>%
  mutate(cuomo_pct = round(cuomo/(cuomo+ nixon),3),
         nixon_pct = round(nixon/(cuomo + nixon),3)) %>%
  select(county, cuomo_pct, nixon_pct) %>%
  gather(person, percent, - county) %>%
  ggplot(aes(x = county, y = percent*100, group = factor(person, levels = c("nixon_pct", "cuomo_pct")), fill = factor(person, levels = c("nixon_pct", "cuomo_pct"),
                                                                                                                      labels = c("Nixon", "Cuomo")))) + geom_col() + 
  coord_flip() + scale_x_discrete(limits = for_breaks) + 
  geom_hline(yintercept = 0.5*100, linetype = 2, size = 0.3) + 
  scale_fill_manual(values = c("chocolate3","dodgerblue1")) +
  labs(fill = "Candidate", x = "County", y = "",
       title = "New York state democratic primary: results by county",
       subtitle = "Data sorted by Cuomo proportion of the Nixon/Cuomo vote",
       caption = "Source: NY Times") + theme_minimal() + 
  theme(
    axis.text.y = element_text(size = 8),
    axis.title.y = element_text(size = 9),
    axis.text.x = element_text(size = 8),
    axis.title.x = element_text(size = 9),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    legend.position = "bottom"
  ) + guides(fill = guide_legend(keywidth = 4, keyheight = 0.5,label.position = "bottom", label.hjust = 0.5,title.vjust = 1)) + 
  scale_y_continuous(breaks = seq(0,100, by = 10))

##Scatterplot
y <- read_csv("voter_file_race.csv") %>%
  mutate(prop_white = white_voters/total_voters)

set.seed(4)
x %>% inner_join(y) %>%
  mutate(cuomo_pct = round(cuomo/(cuomo+ nixon),3),
         nixon_pct = round(nixon/(cuomo + nixon),3),
         total_votes = cuomo + nixon) %>%
  ggplot(aes(x = prop_white*100, y = cuomo_pct*100, size = total_votes)) + 
  geom_point(show.legend = FALSE) + geom_smooth(se = FALSE, method = "lm", show.legend = FALSE, size = 0.5, linetype = 1) + 
  theme_minimal() + labs(x = "% of non-hispanic whites among registered voters",
                         y = "Cuomo proportion of the Nixon/Cuomo vote",
                         title = "Scatterplot of Cuomo vote by % of non-hispanics white among registered voters",
                         subtitle = "Fitted line: OLS estimate",
                         caption = "Source: NY Times and L2 Political\nLabels selected at random") + 
  ggrepel::geom_text_repel(aes(y =  cuomo_pct*100, x = prop_white*100, label = county), data = x %>% inner_join(y) %>%
               mutate(cuomo_pct = round(cuomo/(cuomo+ nixon),3),
                      nixon_pct = round(nixon/(cuomo + nixon),3),
                      total_votes = cuomo + nixon) %>% sample_frac(0.5),nudge_y = -0.7,
            size = 3) + 
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    legend.position = "bottom"
  ) + 
  geom_hline(yintercept = 50, linetype =2, size = 0.25)

