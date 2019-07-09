remove(list =ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

library(tidyverse)
wwc_outcomes <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/wwc_outcomes.csv")

top_teams <- wwc_outcomes%>%
  select(year, team) %>% unique() %>%
  unique() %>%
  count(team, sort = TRUE) %>%
  filter(n ==8) %>%
  pull(team)


last_point <-wwc_outcomes %>%
  group_by(year, team) %>%
  summarise(total_goals = sum(score)) %>%
  ungroup() %>%
  filter(team %in% top_teams) %>%
  arrange(team,year) %>%
  mutate(team = case_when(
    team == "BRA" ~ "Brazil",
    team == "GER" ~ "Germany",
    team == "JPN" ~ "Japan",
    team == "NGA" ~ "Nigeria",
    team == "NOR" ~ "Norway",
    team == "SWE" ~ "Sweden",
    TRUE ~ "USA"
  )) %>%
  group_by(team)  %>% 
  mutate(cum_goals = cumsum(total_goals)) %>%
  filter(year == 2019)

wwc_outcomes %>%
  group_by(year, team) %>%
  summarise(total_goals = sum(score)) %>%
  ungroup() %>%
  filter(team %in% top_teams) %>%
  arrange(team,year) %>%
  mutate(team = case_when(
    team == "BRA" ~ "Brazil",
    team == "GER" ~ "Germany",
    team == "JPN" ~ "Japan",
    team == "NGA" ~ "Nigeria",
    team == "NOR" ~ "Norway",
    team == "SWE" ~ "Sweden",
    TRUE ~ "USA"
  )) %>%
  group_by(team)  %>% 
  mutate(cum_goals = cumsum(total_goals)) %>%
  ggplot(aes(x = year, y = cum_goals, color = team, alpha = year)) + 
  geom_point(aes(x = year, y = cum_goals), data = last_point, show.legend = FALSE) + 
  geom_line(size = 1.2) + 
  scale_color_manual(values = c("lawngreen","white","mediumturquoise", "limegreen","indianred3","khaki1","lightskyblue")) + 
  theme_minimal() + 
  theme(
    text = element_text(color = "gray80", family = "Roboto Condensed", size = 13),
    axis.text = element_text(color = "gray80", family = "Roboto Condensed"),
    axis.title = element_text(color= "gray80", family = "Roboto Condensed"),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 11,face = "bold"),
    legend.position = "none",
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_line(size =0.09, color = "gray80"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(size = 0.09, color = "gray80"),
    plot.background = element_rect(fill = "gray20"),
    axis.title.x = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  ) + 
  scale_x_continuous(breaks = seq(1991, 2019, by = 4),
                     expand = c(0.1,2)) + 
  scale_y_continuous(breaks = seq(0,150,by = 30),
                     limits = c(0,150)) + 
  labs(x = "\nWorld Cup",
       y = "Total goals (cumulative)\n",
       title = "United States and Germany are the top scorers in the Women's World Cup",
       subtitle = "They were the two teams that have scored at least 120 goals\nAnalysis restricted to countries that participated in all tournaments",
       caption = "Tidy Tuesday 2019: Women's World Cup Results") + 
  ggrepel::geom_text_repel(aes(x = year,
                               y = cum_goals,
                               label = team),
                               data = last_point,
                               nudge_x = 3,
                               show.legend = FALSE,
                               fontface = "bold",
                                family = "Roboto Condensed") + 
  scale_alpha_continuous(range = c(0.4,1))