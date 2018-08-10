remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)


setwd("/Users/harrocyranka/Desktop/rviz/school_clustering/")
library(tidyverse);library(readxl)


x <- read_csv("dataset_with_clusters.csv") %>%
  select(pct_school_lunch, median_hh_income, pct_minority, enrollment)

standardize <- function(my_list){
  numerator <- (my_list - mean(my_list))
  denominator <- sd(my_list)
  last <- numerator/denominator
  last <- round(last, 3)
  return(last)
}


##Fit K-means and 
set.seed(1234)
k_fit <- kmeans(x %>% mutate_all(standardize),6, nstart = 25)

x <- x %>% mutate(cluster = k_fit$cluster)

##
cluster_statistics <- x %>% group_by(cluster) %>%
  dplyr::summarise(avg_school_lunch = 100*mean(pct_school_lunch),
                   avg_median_hh = mean(median_hh_income),
                   avg_non_white = 100*mean(pct_minority),
                   avg_enrollment = mean(enrollment),
                   school_number = n()) %>%
  magrittr::set_colnames(c("Cluster", "Mean % school lunch",
                           "Mean zip median household income",
                           "Mean % non-white",
                           "Mean enrollment",
                           "Total schools"))

##PCA
pca <- prcomp(x %>% select(-cluster),center = TRUE, scale = TRUE)
explained_variance <- tibble(component = 1:4, explained_variance = round(pca$sdev^2/sum(pca$sdev^2)*100,1))
explained_variance %>% ggplot(aes(x = component, y = round(explained_variance,2), label= explained_variance)) + geom_col() + 
  theme_minimal() + geom_label() + 
  labs(x = "Component", y= "Explained Variance in %",title = "Explained variance for each component")

pca_tibble <- tibble(pc1 = pca$x[,1], pc2 = pca$x[,2]) %>%
  mutate(cluster = x$cluster)

pca_tibble %>% ggplot(aes(x = pc1, y = pc2, color = as.character(cluster))) + 
  geom_point(size = 0.5) + theme_bw() + 
  labs(x = "First Principal Component", y = "Second Principal Component", color = "Cluster assignment",
       title = "Scores for the first two principal components")

round(pca$rotation,2)
cluster_statistics

##New York: Send this to visualize in tableau
ny_list <- read_csv("dataset_with_clusters.csv") %>%
  mutate(cluster = x$cluster) %>%
  filter(county %in% c("New York County", "Kings County", "Queens County", "Bronx County",
                       "Richmond County") & lstate == "NY") %>%
  dplyr::select(latcod, loncod, cluster, enrollment) %>%
  dplyr::rename(lat = latcod, lon = loncod)

write_csv(ny_list, "new_york_school_list.csv")

##Chicago tableau
chicago <- read_csv("dataset_with_clusters.csv") %>%
  mutate(cluster = x$cluster) %>%
  filter(county %in% c("Cook County") & lstate == "IL") %>%
  dplyr::select(latcod, loncod, cluster, enrollment) %>%
  dplyr::rename(lat = latcod, lon = loncod)

write_csv(chicago, "chicago_school_list.csv")


##
cluster_statistics %>%
  select(-`Total schools`) %>%
  mutate(`Mean zip median household income` = round(`Mean zip median household income`,0),
         `Mean enrollment` = round(`Mean enrollment`,0),
         `Mean % school lunch` = round(`Mean % school lunch`,1),
         `Mean % non-white` = round(`Mean % non-white`,1)) %>%
  gather(measure, total,-Cluster) %>%
  arrange(Cluster) %>%
  ggplot(aes(x = as.character(Cluster), y = total, fill = as.character(Cluster),label = round(total,2))) +
  geom_col(show.legend = FALSE) + geom_label(show.legend = FALSE, fill = "white", size = 3) + 
  facet_wrap(~measure, scales = "free", ncol = 2) + theme_bw() + 
  labs(x = "Cluster", title = "Cluster statistics", y = "Total",
       subtitle = "Data on 25,000 public schools")


####Create Cluster Map####

library(shiny)
library(plotly)

create_cluster_map <- function(cluster_vector, colors_vector){
    us_map <- read_csv("dataset_with_clusters.csv") %>%
      mutate(cluster = x$cluster) %>% 
      filter(cluster %in% cluster_vector) %>%
      dplyr::select(latcod, loncod, cluster, enrollment, school_name)
    
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showland = TRUE,
      landcolor = toRGB("white"),
      subunitwidth = 1,
      countrywidth = 1,
      subunitcolor = toRGB("black"),
      countrycolor = toRGB("black")
    )
    
    
    p <- plot_geo(us_map, locationmode = 'USA-states', sizes = c(2, 20)) %>% add_markers(
      x = ~loncod, y = ~latcod, size = ~enrollment,color = ~cluster,colors = colors_vector,hoverinfo = "text",
      mode = "markers",
      text = ~paste('School Name: ',school_name,
                    '<br> Cluster: ',cluster)
    )  %>% layout(title = "US Map: Clusters 2 and 3", geo = g,showlegend = FALSE, width = 2000,height = 1000) %>%
      hide_colorbar()
    
    return(p)
}

y <- create_cluster_map(c(2,3),c("goldenrod","forestgreen"))

y

