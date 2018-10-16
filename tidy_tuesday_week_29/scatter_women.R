rm(list = ls())

x <- read_csv("recent_grads.csv")

x %>% ggplot(aes(x = ShareWomen, y = Median, color = Major_category))+
    geom_point() + 
    theme(
        legend.position = "bottom"
    )