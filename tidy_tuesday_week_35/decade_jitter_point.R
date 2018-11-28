remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

library(tidyverse);library(viridis)
x <- read_csv("baltimore_bridges.csv")


x <- x %>% filter(yr_built >= 1910) %>%
  mutate(decade = (yr_built - yr_built %%10))



x %>% ggplot(aes(x = as.factor(decade),
                 y = avg_daily_traffic,
                 color = factor(bridge_condition,labels = c("Poor", "Fair", "Good")))) + 
  geom_jitter(width = 0.25, alpha =0.6) + 
  theme_minimal() + 
  theme(
    legend.position = "bottom"
  ) + 
  labs(color = "Bridge condition",
       x = "\nDecade",
       y = "Average daily traffic")