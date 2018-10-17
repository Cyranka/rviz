remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/tidy_tuesday_week_29/")
library(tidyverse);library(viridis)


# Load data ---------------------------------------------------------------

x <- read_csv("recent_grads.csv") %>%
  mutate(Major = str_to_title(Major))


# Graph creation ----------------------------------------------------------
max_salary <- x %>% mutate(Major_category = str_replace(Major_category, "&","/")) %>%
  mutate(Major_category = fct_reorder(Major_category, Median)) %>%
  group_by(Major_category) %>% 
  filter(n() >4) %>%
  top_n(1,wt = Median) %>%
  select(Major, Major_category, Median)


x %>% mutate(Major_category = str_replace(Major_category, "&","/")) %>%
  mutate(Major_category = fct_reorder(Major_category, Median)) %>%
  group_by(Major_category) %>% filter(n() >4) %>%
  ggplot(aes(x = Major_category, y = Median, fill = Major_category)) +
  geom_boxplot(show.legend = FALSE, size =0.4, alpha = 0.6, width = 0.5,outlier.shape = 18,outlier.fill = "red") +
  geom_point(show.legend = FALSE, shape = 18) + 
  coord_flip() + theme_minimal() + 
  labs(x = "", y = "\nMedian salary",
       title = "Distribution of median salary by major category",
       subtitle = "Categories sorted by median salary",
       caption = "Tidy tuesday Week 29: salaries by college major\nLabels indicate highest median salary") + 
  theme(
    text = element_text(family = "Roboto"),
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 13),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 12,face = "bold"),
    panel.grid.minor = element_blank()
  ) + 
  scale_fill_viridis(discrete = TRUE, option = "B", direction = -1) + 
  scale_y_continuous(
    limits = c(20000,120000),
    breaks = c(0,20000, 40000, 60000, 80000, 100000),
    labels = c("0","20,000","40,000","60,000","80,000", "100,000")) + 
  geom_hline(yintercept = 59000, linetype = 2, size =0.2) +
  ggrepel::geom_text_repel(aes(x = Major_category, y = Median, label = Major),
            data = max_salary,inherit.aes = FALSE, size = 2.25,
            nudge_y = 600,
            nudge_x = -0.1,
            fontface= "bold") +
  annotate("text", x = "Education",
           y = 60000,
           label = "US median household income: U$59,000",
           angle = 90, 
           family = "Roboto",
           size = 3,
           fontface = "bold"
           )
  

