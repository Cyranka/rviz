remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/tidy_tuesday_week_24/")
library(tidyverse)
library(dendextend)

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
  ggplot(aes(x = chi_quantiles, y= distances)) + geom_point(size = 0.5) + 
  geom_abline(slope = 1, intercept = 0) + 
  ggrepel::geom_text_repel(aes(x = chi_quantiles, y = distances, label = state), data = top_6,
                            size = 3) + 
  theme_minimal() + labs(x = expression(paste(chi[2]^2, " quantile")),
                         y = "Generalised distance",
                         title = "Chi-square plot of generalised distances for pet ownership data",
                         subtitle = "Plot clearly shows DC, Maine,Vermont, and Massachusetts as potential outliers",
                         caption = "Analysis based on the following variables:  % of dog owners\n% of cat owners") + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 10),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))


##Killing DC and running the algorithm
distance <- x %>% select(state,percent_dog_owners, percent_cat_owners)%>%
  filter(state != "DC") %>%
  select(-state) %>%
  dist() %>% round(2)



hcFit <- hclust(distance, method = "average")

##Basic dendrogram
plot(hcFit,xlab = "Distance",hang = -1,cex = 0.8,main = "Hierarchical clustering dendrogram")

##More elaborated dendrogram
dend1 <- color_labels(hcFit, k = 5)
labels(dend1) <- x$state[-8][labels(dend1)]
plot(dend1 %>% color_branches(k = 5) %>% set("labels_cex", 0.75),ylab = "Height",xlab = "Distance",cex.lab = 0.75,
     main = "Hierarchical clustering dendrogram\nBranches and leaves colored by cluster assignment",
     sub = "Tidy tuesday week 24",cex.axis = 0.75, cex.main = 1, cex.sub = 0.65)


##colorspace::rainbow_hcl(n = 5,c = 90, l = 50)

##Basic plot 
with_cluster <- x %>% select(state,percent_dog_owners, percent_cat_owners)%>%
  filter(state != "DC") %>%
  mutate(cluster = cutree(hcFit,k = 5))

set.seed(1)
my_labels <- with_cluster %>% group_by(cluster) %>% sample_frac(0.99)

with_cluster %>% 
  ggplot(aes(x = percent_dog_owners, y = percent_cat_owners,
             color = as.character(cluster))) + 
  geom_point(size = 5, alpha = 0.5) + 
  theme_minimal() + 
  scale_color_manual(values =c("gold4","purple","forestgreen","firebrick","darkturquoise")) + 
  theme(legend.position = "bottom") + 
  labs( x = "% Percent dog owners",
        y = "% Percent cat owners",
        color = "Cluster assignment",
        title = "Scatterplot of % percent of cat owners against % of dog owners",
        subtitle = "Colors determined by cluster assignment",
        caption = "Tidy tuesday week 24") + 
  ggrepel::geom_label_repel(aes(x = percent_dog_owners,
                                       y = percent_cat_owners,
                                       label = state),size = 2.4,data = my_labels,
                                       show.legend = FALSE, color = "black",segment.size = 0.5) + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        plot.subtitle = element_text(size = 13))
  

##Put into a map
library(urbnmapr)

with_cluster %>%
  inner_join(urbnmapr::states,by = c("state" = "state_name")) %>%
  ggplot(aes(long, lat, group = group, fill = as.character(cluster))) + 
  geom_polygon(color = "gray25", size = .25) + theme_minimal() + 
  labs(fill = "Cluster",
       title = "Continental map of the United States colored by cluster assigment",
       subtitle = "States clustered on % of dog owners and % of cat owners (DC excluded)",
       caption = "Tidy tuesday week 24\nSource: AVA") + 
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
  scale_fill_manual(values =alpha(c("gold4","purple","forestgreen","firebrick","darkturquoise"),0.4)) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(face = "bold", size = 15, color = "black", hjust = 0.5),
        plot.subtitle = element_text(size = 13, color = "black", hjust = 0.5),
        legend.title = element_text(color = "black", face = "bold"),
        legend.text = element_text(color = "black"),
        text=element_text(family="Roboto"),
        plot.background = element_rect(fill = "white"),
        legend.position = "bottom") + 
  guides(fill = guide_legend(keywidth = 4, keyheight = 0.5, title="Cluster", label.position = "bottom", label.hjust = 0.5))



  