remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/tidy_tuesday_week_28/")
library(tidytext);library(tidyverse)


x <- read_csv("voter_turnout.csv") %>%
  mutate(turnout = round(votes/eligible_voters*100,2)) %>%
  mutate(election = ifelse(year %in%seq(1980, 2012, 4),"Presidential","Midterm")) %>%
  select(year,election,icpsr_state_code:turnout)

input_data <- x %>% filter(icpsr_state_code!=0) %>%
  filter(election == "Presidential") %>%
  group_by(year) %>%
  mutate(turnout = ifelse(is.na(turnout), mean(turnout,na.rm = TRUE), turnout)) 


top_states <- input_data %>% group_by(state) %>%
  summarise(turnout_period = mean(turnout)) %>%
  arrange(desc(turnout_period)) %>%
  slice(1:3) %>%
  bind_rows(input_data %>% group_by(state) %>%
              summarise(turnout_period = mean(turnout)) %>%
              arrange(turnout_period) %>%
              slice(1:3))



dat_test <- tibble(state_factor = top_states$state,
                   turnout = top_states$turnout_period,
  text = paste0("Average turnout in the period: ",round(top_states$turnout_period,1), "%")) %>%
  mutate(state_factor = factor(state_factor)) %>%
  mutate(state_factor= fct_reorder(state_factor, turnout, mean, .desc = TRUE),
         x = 2000,  y= 35)


k <- input_data %>%
  mutate(state_factor = factor(state)) %>%
  mutate(state_factor = fct_reorder(state_factor, turnout, mean, .desc = TRUE))
  

k%>%
  ggplot(aes(x = year, y = turnout, color = state)) + geom_line(show.legend = FALSE, size = 3) + 
  gghighlight::gghighlight(state %in%top_states$state,
                           use_group_by = FALSE,use_direct_label = FALSE) + 
  theme_minimal() + facet_wrap(~state_factor, scales = "free", nrow = 2,strip.position = "top") + 
  labs(x = "Year",
       y ="Turnout in %",
       title = "Trends in turnout in high and low turnout states: Presidential elections",
       subtitle = "Top and bottom states selected on the basis of their average turnout between 1980 and 2014",
       caption = "Tidy tuesday week 28: turnout in the United States\nMissing numbers were replaced with the median for that year") + 
  scale_x_continuous(breaks = seq(1980, 2012, 4)) + 
  theme(
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    axis.line.x = element_line(size = 0.5, linetype = "solid"),
    strip.background = element_rect(fill = "gray91"),
    strip.text = element_text(face = "bold")
  ) + 
  scale_color_manual(values = c("red4","steelblue3","steelblue3","red4","steelblue3","red4")) + 
  geom_text(aes(x = x, y = y, color = state, label = text), data = dat_test, col = "black",
            size = 3)
