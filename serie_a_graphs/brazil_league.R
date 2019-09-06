remove(list =ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harro.cyranka/Desktop/rviz/")
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
                         points == 3 ~ "Victory",
                         points == 1 ~ "Tie",
                         TRUE ~ "Loss"
                       )))
names(points_list) <- team_list




team_points %>%
  ggplot(aes(x = round_n,
             y = cumulative_points, group = team)) + 
  geom_step(size = 1,
            color = "gray85",
            alpha =0.5) + 
  geom_step(aes(x = round_n,
                 y = cumulative_points,
                 color = result),
             data = points_list[["Flamengo"]],
             size = 1,alpha =0.6,show.legend = FALSE) + 
  geom_point(aes(x = round_n,
                 y = cumulative_points,
                 color = result),
             data = points_list[["Flamengo"]],
             size = 6,shape = 18) +
  scale_color_manual(values = c("Loss" = "firebrick3", 
                                "Tie" = "lavenderblush4",
                                "Victory" = "darkgreen")) + 
  scale_y_continuous(breaks = seq(0,40,5)) + 
  scale_x_continuous(breaks = seq(1,17,1)) + 
  theme_classic() + 
  theme(
    text = element_text(family = "IBM Plex Sans"),
    legend.position = "bottom"
  ) + 
  labs(x = "\nRodada", y = "Total de pontos\n",
       title = "Flamengo: total de pontos ao final de cada rodada",
       subtitle = "Todos os resultados até a rodada 17") + 
  guides(color = guide_legend(title = "Resultado",
                              title.hjust = 0.5,title.position = "top",label.position = "bottom"))


plot_function <- function(team){
  z <- team_points %>%
    ggplot(aes(x = round_n,
               y = cumulative_points, group = team)) + 
    geom_step(size = 3,
              color = "gray85",
              alpha =0.3) + 
    geom_step(aes(x = round_n,
                  y = cumulative_points,
                  color = result),
              data = points_list[[team]],
              size = 1,alpha =0.6,show.legend = FALSE) + 
    geom_point(aes(x = round_n,
                   y = cumulative_points,
                   color = result),
               data = points_list[[team]],
               size = 6,shape = 18) +
    scale_color_manual(values = c("Loss" = "firebrick3", 
                                  "Tie" = "lavenderblush4",
                                  "Victory" = "darkgreen")) + 
    scale_y_continuous(breaks = seq(0,40,5)) + 
    scale_x_continuous(breaks = seq(1,17,1)) + 
    theme_classic() + 
    theme(
      text = element_text(family = "IBM Plex Sans"),
      legend.position = "bottom"
    ) + 
    labs(x = "\nRound", y = "Total points\n",
         title = paste0(team,": round by round trajectory"),
         subtitle = "All results until round 17",
         caption = "Delayed games marked as a tie") + 
    guides(color = guide_legend(title = "Result",
                                title.hjust = 0.5,title.position = "top",label.position = "bottom"))
  
  return(z)
  
}

a<- plot_function("Flamengo")
b <- plot_function("Botafogo")
c <- plot_function("Vasco da Gama")
d <- plot_function("Fluminense")

plot_row <- cowplot::plot_grid(a,b,c,d, nrow = 2)

title <- cowplot::ggdraw() + 
  cowplot::draw_label(
    "Rio's Big 4: Série A trajectory round by round",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    plot.margin = margin(0, 0, 0, 7),
    text = element_text(family = "IBM Plex Sans", face = "bold")
  )

cowplot::plot_grid(
  title, plot_row,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1),ncol = 1
)