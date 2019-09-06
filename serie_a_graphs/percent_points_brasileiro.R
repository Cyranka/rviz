remove(list =ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)


library(rvest);library(tidyverse)

url <- "https://pt.wikipedia.org/wiki/Resultados_do_primeiro_turno_do_Campeonato_Brasileiro_de_Futebol_de_2019_-_S%C3%A9rie_A"

p <- read_html(url) %>%
  html_table(fill = TRUE)



x <- bind_rows(lapply(p[4:173], function(i)i %>% slice(1))) %>%
  rename(date = 1,
         home = 2,
         result = 3,
         away = 4,
         stadium = 5) %>% as_tibble() %>%
  separate(result, into = c("home_goals", "away_goals"), sep = "–") %>%
  mutate_at(c("home_goals", "away_goals"), parse_number)

x <- x %>% 
  replace_na(list(home_goals = 0, away_goals = 0)) %>%
  mutate(home_points = case_when(
    home_goals > away_goals ~ 3,
    home_goals == away_goals ~ 1,
    TRUE ~ 0
  ),
  away_points  = case_when(
    home_points == 3 ~ 0,
    home_points == 1 ~ 1,
    TRUE ~ 3
  )) %>%
  mutate(game_n = row_number())


count_points <- function(team_name){
  home_points <- x %>% 
    filter(home == team_name) %>%
    select(home, home_points, game_n) %>%
    rename(team = 1, points = 2, game_n = 3)
  
  away_points <- x %>%
    filter(away == team_name) %>%
    select(away, away_points, game_n) %>%
    rename(team = 1, points = 2, game_n = 3)
  
  y <- bind_rows(home_points, away_points) %>% arrange(game_n) %>%
    mutate(cumulative_points = cumsum(points),
           round_n = row_number())
  return(y)
}

team_list <- unique(x$home)
team_points <- bind_rows(lapply(team_list, function(i)count_points(i))) %>%
  group_by(team) %>%
  mutate(result = case_when(
    points == 3 ~ "Win",
    points == 1 ~ "Tie",
    TRUE ~ "Loss"
  ))

points_list <-lapply(team_list, function(i)count_points(i) %>% 
                       mutate(result = case_when(
                         points == 3 ~ "Win",
                         points == 1 ~ "Tie",
                         TRUE ~ "Loss"
                       )) %>%
                       mutate(full_points = seq(3,51,by = 3),
                              percent_points = cumulative_points/full_points))
names(points_list) <- team_list
points_list[[1]]

colors <- readxl::read_excel("color_teams_serie_a.xlsx")

bind_rows(points_list) %>%
  select(team, round_n,percent_points, cumulative_points) %>%
  ggplot(aes(round_n, percent_points, fill = team)) + 
  geom_area(show.legend = FALSE, alpha = 0.6) + 
  facet_wrap(~fct_rev(fct_reorder(team, cumulative_points,max)),
             scales = "free",nrow = 5) + 
  geom_line(aes(color = team), size = 2, show.legend = FALSE) + 
  scale_color_manual(values = colors$color) + 
  scale_fill_manual(values = colors$color) + 
  scale_y_continuous(labels = scales::percent,
                     breaks = c(0,0.5,1), limits = c(0,1.1)) + 
  scale_x_continuous(breaks = seq(1,17,1), expand = c(0,0.1)) + 
  theme_gray() + 
  theme(
    text = element_text(color = "white", family = "IBM Plex Sans"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(size = 0.02, color = "white"),
    panel.background = element_rect(fill = "gray20"),
    plot.background = element_rect(fill = "black"),
    plot.title = element_text(face = "bold",size = 18),
    plot.subtitle = element_text(size = 14),
    strip.background = element_rect(fill = "gray20", color = "black"),
    strip.text = element_text(color = "white", family = "IBM Plex Sans"),
    axis.text = element_text(family = "IBM Plex Sans", color = "white"),
    axis.title = element_text(family = "IBM Plex Sans", color = "white")) + 
  labs(x = "\nRound",y = "% of total points\n", 
       title = "Changes in point performance in the Brazilian Série A until round 17",
       subtitle = "Delayed games marked as a tie\n")
  
  
  