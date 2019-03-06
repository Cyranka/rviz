remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/rviz/")
library(tidyverse)

x <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/jobs_gender.csv")

x %>% filter(!is.na(wage_percent_of_male)) %>%
  pull(minor_category) %>%
  table()


x %>% filter(!is.na(wage_percent_of_male)) %>%
    mutate(major_category = factor(major_category),
           major_category = fct_rev(fct_reorder(major_category, wage_percent_of_male))) %>%
  ggplot(aes(x = percent_female,
             y = wage_percent_of_male, 
             color = major_category)) + 
  geom_point(show.legend = FALSE, size = 4,alpha =0.3) + 
  facet_wrap(~major_category, nrow = 8, scales = "free_x") + 
  geom_smooth(method = "lm", show.legend = FALSE,se = FALSE, color = "white",
              size = 0.5) + 
  scale_x_continuous(limits = c(0,100)) + 
  scale_y_continuous(limits = c(40,130)) + 
  theme_minimal() + 
  labs(x = "\n% Female", y = "Female earnings as % of male earnings\n", 
       title = "Does the proportion of females within an occupation influence the gender pay gap?",
       subtitle = "Linear relationship is frequently small and non-significant",
       caption = "White line represents regression line\nTidy Tuesday 2019 Week 10: Women in the workforce") + 
  theme(
    text = element_text(family = "Roboto", color = "white"),
    plot.title = element_text(family = "Roboto", size = 14, face = "bold"),
    axis.text = element_text(family = "Roboto", color = "white"),
    axis.title = element_text(family = "Roboto", color = "white", face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(size = 0.05,color = "gray90"),
    axis.line.x = element_line(color = "gray80",size = 0.5),
    plot.background = element_rect(fill = "gray20"),
    strip.text = element_text(family = "Roboto", color = "white", size = 9,face = "bold"), 
    strip.background = element_rect(fill = "gray30", color = "gray20")
  ) + 
  scale_color_viridis_d(option = "B", begin = 0.6,end = 0.85, direction = -1)


##
x %>% filter(!is.na(wage_percent_of_male)) %>%
  mutate(major_category = factor(major_category),
         major_category = fct_reorder(major_category, wage_percent_of_male)) %>%
  ggplot(aes(x = major_category,
             y = wage_percent_of_male, 
             color = major_category)) +
  geom_jitter(show.legend = FALSE, width = 0.14) +
  geom_boxplot(show.legend = FALSE,width = 0.3, alpha =0.5, color = "gray30") + 
  labs(x = "Major category\n", y = "\nFemale earnings as % of male earnings",
       title = "Distribution of female earnings as % of male earnings",
       subtitle = "Data grouped by major employment categories",
       caption = "White dotted line represents wage parity\nTidy Tuesday 2019 Week 10: Women in the workforce") + 
  coord_flip() + 
  geom_hline(yintercept = 100, color = "gray80", linetype = 2) + 
    theme_minimal() + 
    theme(
        text = element_text(family = "Roboto", color = "white"),
        plot.title = element_text(family = "Roboto", size = 14, face = "bold"),
        axis.text = element_text(family = "Roboto", color = "white"),
        axis.title = element_text(family = "Roboto", color = "white", face = "bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.05,color = "gray90"),
        axis.line.x = element_line(color = "gray80",size = 0.5),
        plot.background = element_rect(fill = "gray20"),
        strip.text = element_text(family = "Roboto", color = "white", size = 9,face = "bold"), 
        strip.background = element_rect(fill = "gray30", color = "gray20")
    ) + 
    scale_color_viridis_d(option = "B", begin = 0.6,end = 0.85) + 
    annotate("text", x = 3.5,y = 105, label = "Parity line", color = "white",
             family = "Roboto", fontface = "bold")