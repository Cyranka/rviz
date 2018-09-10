remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/tidy_tuesday_week_24/")
library(tidyverse)

x <- read_csv("cats_vs_dogs.csv") %>%
  mutate(state = ifelse(state == "District of Columbia", "DC", state))

##Let's find outliers, first
percent_owners <- x %>% select(percent_dog_owners, percent_cat_owners) 
col_means <- apply(percent_owners,2, mean)
s_matrix <- cov(percent_owners)

distances <- apply(percent_owners, 1, function(x) t(x-col_means) %*% solve(s_matrix) %*% (x-col_means))
chi_qc <- qchisq((1:nrow(percent_owners) - 1/2)/nrow(percent_owners),df = 2)


##
all_distances <- tibble(state = x$state[order(distances)],distances = sort(distances),chi_quantiles = sort(chi_qc)) 
top_6 <- all_distances %>% top_n(6, distances)


all_distances%>%
  ggplot(aes(x = chi_quantiles, y= distances)) + geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  ggrepel::geom_text_repel(aes(x = chi_quantiles, y = distances, label = state), data = top_6,
                            size = 3) + 
  theme_minimal() + labs(x = expression(paste(chi[2]^2, " quantile")),
                         y = "Generalised distance")


##Killing DC and running the algorithm
distance <- x %>% select(state,percent_dog_owners, percent_cat_owners)%>%
  filter(state != "DC") %>%
  select(-state) %>%
  dist() %>% round(2)

hcFit <- hclust(distance, method = "average")
plot(hcFit,xlab = "Distance", cex = 0.8)

with_cluster <- x %>% select(state,percent_dog_owners, percent_cat_owners)%>%
  filter(state != "DC") %>%
  mutate(cluster = cutree(hcFit,k = 5))

with_cluster %>% 
  ggplot(aes(x = percent_dog_owners, y = percent_cat_owners,
             color = as.character(cluster),
             label = state)) + 
  geom_point(shape = 15, size = 2.5) + 
  theme_minimal() + 
  theme(legend.position = "bottom") + 
  labs( x = "% Percent dog owners",
        y = "% Percent cat owners",
        color = "Cluster assignment") + ggrepel::geom_label_repel(size = 2.5, color = "black")
  

##Put into a map
library(urbnmapr)

with_cluster %>%
  inner_join(urbnmapr::states,by = c("state" = "state_name")) %>%
  ggplot(aes(long, lat, group = group, fill = as.character(cluster))) + 
  geom_polygon(color = "black", size = .25) + theme_minimal() + 
  labs(fill = "Cluster") + 
  scale_fill_brewer(palette = "Reds") + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())



x %>% group_by(state) %>% summarise(ratio = dog_population/cat_population) %>% arrange(desc(ratio)) %>% 
  write_csv("/Users/harrocyranka/Desktop/dogs_to_cats.csv")
  