remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

library(tidyverse)

x <- read_csv("launches.csv")


cumulative <- x %>% group_by(launch_year,state_code) %>% tally() %>%
  arrange(state_code, launch_year) %>%
  group_by(state_code) %>%
  mutate(cumulative =cumsum(n)) %>%
  filter(state_code %in% c("US", "SU") & between(launch_year, 1957, 1991))



cumulative %>% ungroup() %>%
  select(-cumulative) %>%
  spread(state_code, n) %>% mutate(winner = ifelse(SU > US, "SU", "US")) %>%
  filter(winner == "SU")
  
  
cumulative %>% ungroup() %>%
  mutate(state_code = ifelse(state_code == "SU", "USSR", "USA")) %>%
  ggplot(aes(x = launch_year, y = n, fill = state_code)) + 
  geom_area(alpha  = 0.5, position = position_dodge(width = 0.5)) + 
  geom_vline(xintercept = 1967, linetype = 2, size = 0.5, color = "gray40") + 
  geom_vline(xintercept = 1957.5, linetype = 2, size = 0.5, color = "gray40") + 
  geom_vline(xintercept = 1991, linetype = 2, size = 0.5, color = "gray40") + 
  scale_x_continuous(breaks = seq(1957,1991,1)) + 
  scale_y_continuous(breaks = seq(0,105,5)) + 
  scale_fill_manual(values = c("steelblue", "firebrick2")) + 
  labs(x = "\nYear", y = "Total launches",
       title = "The race for space during the Cold War",
       subtitle = "The USSR overtook the United States in 1967") + 
  theme_minimal() + 
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill = "gray85"),
    panel.grid.minor = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(size =0.3),
    axis.text.x = element_text(angle = 90)
  ) + 
  guides(fill = guide_legend(title = "Country", title.position = "top", title.hjust = 0.5,
                             label.position = "bottom",keyheight =0.5,
                             keywidth = 3)) + 
  annotate("text", x = 1966.5, y = 85, label = "1967: The USSR overtakes the US", angle = 90, size = 3) + 
  annotate("text", x = 1957, y = 85, label = "1957: Sputnik launched", angle = 90, size = 3) + 
  annotate("text", x = 1990.5, y = 85, label = "1991: End of the USSR", angle = 90, size = 3)
