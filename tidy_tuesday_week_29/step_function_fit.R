remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

library(tidyverse);library(viridis)
setwd("/Users/harrocyranka/Desktop/rviz/tidy_tuesday_week_29/")

x <- read_csv("recent_grads.csv")

##Fit step function
fit_1 <- lm(Median~cut(ShareWomen,seq(from = 0, to = 1, by = 0.25)), data = x)
arm::display(fit_1, detail = TRUE)

pred_df <- tibble(ShareWomen = x$ShareWomen,
  predicted_salary = predict(fit_1, newdata = tibble(ShareWomen = x$ShareWomen))) %>%
  filter(!is.na(ShareWomen),
         !is.na(predicted_salary))

##
cut_point_labels <- c("0 to 25%", "25 to 50%", "50 to 75%", "75 to 100%")
##
x %>% filter(!is.na(ShareWomen)) %>%
  filter(ShareWomen >0) %>%
  ggplot(aes(x = ShareWomen*100, y = Median, color = cut(ShareWomen*100,
                                                         seq(from = 0, to = 100, by = 25),
                                                         labels = cut_point_labels))) + 
  geom_point(size = 3, alpha = 0.5) + 
  geom_line(aes(x = ShareWomen*100, y = predicted_salary), inherit.aes = FALSE, data = pred_df,linetype = 1, size =0.3) + 
  labs(x = "\n% of female graduates", y = "Major median salary", color = "Categories",
       title = "Step function fit of major median salary on % of female graduates",
       subtitle = "Bins selected at 25, 50, and 75%. Fitted line in black.",
       caption = "Tidy tuesday week 29: Salary by major") + 
  theme_minimal() + 
  theme(
    text = element_text(family = "Roboto"),
    legend.position = "bottom",
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(size = 14, face = "bold")
  ) + 
  guides(color = guide_legend(title.position = "top",
                              title.hjust = 0.5,
                              label.position = "bottom")) + 
  scale_y_continuous(limits = c(20000,130000),
                     breaks = seq(20000,120000,by = 20000),
                     labels = c("20,000","40,000", "60,000","80,000","100,000","120,000")) + 
  scale_x_continuous(limits = c(0,100),
                     breaks = seq(0,100,by =25)) + 
  geom_vline(xintercept = c(25,50,75),linetype = 2, size= 0.15) + 
  geom_segment(aes(x = 1, y = 120000, xend = 24,yend=120000), 
               size = 0.1, col = "gray45", lineend = "butt") + 
  geom_segment(aes(x = 26, y = 100000, xend = 49,yend=100000), 
               size = 0.1, col = "gray45", lineend = "butt") + 
  geom_segment(aes(x = 51, y = 80000, xend = 74,yend=80000), 
               size = 0.1, col = "gray45", lineend = "butt") + 
  geom_segment(aes(x = 76, y = 60000, xend = 99,yend=60000), 
               size = 0.1, col = "gray45", lineend = "butt") + 
  annotate("text", label = "Predicted salary: U$ 54,036", x = 12.5, y = 122000, color = "black",
           size = 3, fontface = "bold") + 
  annotate("text", label = "Predicted salary: U$ 43,604", x = 37.5, y = 102000, color = "black",
           size = 3, fontface = "bold") + 
  annotate("text", label = "Predicted salary: U$ 35,335", x = 62.5, y = 82000, color = "black",
           size = 3, fontface = "bold") + 
  annotate("text", label = "Predicted salary: U$ 33,074", x = 87.5, y = 62000, color = "black",
           size = 3, fontface = "bold") + 
  scale_color_viridis(option = "A", discrete = TRUE, begin = 0.5, end = 0.8)