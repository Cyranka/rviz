remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

library(tidyverse);library(viridis);library(tidytext)
setwd("/Users/harrocyranka/Desktop/rviz/tidy_tuesday_week_29/")

##
cut_point_labels <- c("0 to 25%", "25 to 50%", "50 to 75%", "75 to 100%")

##
x <- read_csv("recent_grads.csv") %>% filter(!is.na(ShareWomen)) %>%
  filter(ShareWomen >0) %>%
  mutate(women_percent = cut(ShareWomen*100,
                             seq(from = 0, to = 100, by = 25),
                             labels = cut_point_labels))


top_words <- x %>% select(women_percent, Major) %>%
  mutate(Major=str_to_title(Major)) %>%
  unnest_tokens(word, Major) %>% group_by(women_percent,word) %>%
  tally() %>% anti_join(stop_words) %>% filter(word != "miscellaneous")


##Create Graph
top_words %>% group_by(women_percent) %>% arrange(women_percent, desc(n)) %>% slice(1:10) %>%
  mutate(word = str_to_title(case_when(
    women_percent == "0 to 25%" ~ paste0(word, " "),
    women_percent == "25 to 50%" ~ paste0(" ",word),
    women_percent == "50 to 75%" ~ paste0(" ", word, " "),
    TRUE ~ word
  ))) %>% ungroup() %>%
  mutate(women_percent = paste0("Proportion of female graduates: ",women_percent)) %>%
  ggplot(aes(x = reorder(word,n), y = n, fill = women_percent, label = n)) + 
  geom_col(show.legend = FALSE) +  
  labs(y = "\nTotal frequency in major titles", x = "Word",
       title = "Most common words in major titles",
       subtitle = "Stop words were removed",
       caption = "Tidy tuesday week 29: Salary by major") + 
  facet_wrap(~women_percent, scales = "free") + 
  coord_flip() +
  theme_minimal() + 
  scale_y_continuous(limits = c(0,15),
                     breaks = seq(0,15,by = 3),
                     labels = c("0","3","6", "9","12","15")) + 
  theme(
    text = element_text(family = "Roboto"),
    panel.grid.minor.x = element_blank(),
    strip.text =element_text(size =11, color = "black"),
    axis.line.x = element_line(size = 0.5),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.text.x = element_text(size =11),
    axis.text.y = element_text(size = 11),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  ) + 
  scale_fill_manual(values = RColorBrewer::brewer.pal(6, "YlOrRd")[3:6])



