remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/k_means_gifs/")
library(tidyverse);library(gganimate)

###
x <- iris %>% as_tibble()


##Fit PCA
x_scale <- x %>% select(-Species) %>% scale()
pca_object <- prcomp(x_scale)


##Build tibble with components
pc_tibble <- tibble(pc1 = pca_object$x[,1],
       pc2 = pca_object$x[,2],
       species = x$Species)


y <- pc_tibble %>% select(-species)

##Vanilla K-means: To provide a benchmark
vanilla <- kmeans(y, centers = 3)

##
k_means_algorithm <- function(data_frame, k, runs){
  
  ##Define euclidean distance function
  euc_dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
  
  ##Calculate initial assignment
  x <- data_frame %>% mutate(assignment = sapply(1:nrow(data_frame), function(i)sample(1:k,1)))
  centroid <- x %>% group_by(assignment) %>% summarise_all(mean)
  
  ##List of assignment
  list_of_assignments <- vector(mode = "list", length = runs)
  list_of_assignments[[1]] <- x$assignment
  
  ##List of centroids
  list_of_centroids <- vector(mode = "list", length = runs)
  list_of_centroids[[1]] <- centroid %>% select(-assignment)
    
  
  ##Update centroid and assignment
  for(p in 2:runs){
    print(paste0("Running iteration: ", as.character(p)))
      dist_matrix <- matrix(rep(0,nrow(centroid)*nrow(x)), ncol = nrow(centroid), nrow = nrow(x))
      
      for(i in 1:nrow(y)){
        for(j in 1:nrow(centroid)){
          dist_matrix[i,j] <- euc_dist(x[i,] %>% select(-assignment),centroid[j,] %>% select(-assignment))
        }
      }
      
      new_assignments <- apply(dist_matrix,MARGIN = 1,which.min)
      x <- x %>% mutate(assignment = new_assignments)
      
      list_of_assignments[[p]] <- new_assignments
      
      centroid <- x %>% group_by(assignment) %>% summarise_all(mean)
      list_of_centroids[[p]] <- centroid %>% select(-assignment)
  }
  
  return_list <- list(list_of_assignments, list_of_centroids)
  names(return_list) <- c("assignments", "centroids")
  return(return_list)
}


# K = 3 -------------------------------------------------------------------
l <- k_means_algorithm(y, 3,runs = 15)

##
assignments <- bind_cols(l[[1]])
centroids <- bind_rows(l[[2]]) %>% mutate(cluster = rep(1:3,15),
                                          iteration = rep(1:15,each = 3)) 


##
tibble_with_assignments <- pc_tibble %>% bind_cols(assignments) %>%
  gather(iteration, assignment, -pc1,-pc2,-species) %>%
  mutate(iteration = as.numeric(str_replace(iteration, "V","")))


gif_1 <- tibble_with_assignments %>%
  filter(iteration <10) %>%
  ggplot(aes(x = pc1, y = pc2)) + 
  geom_point(size = 4) + 
  geom_point(aes(x = pc1, y = pc2, color = as.character(assignment)), size = 4, show.legend = TRUE) +
  geom_point(aes(x = pc1, y = pc2),data = centroids %>% filter(iteration <10), shape = 18, size = 5) + 
  labs(x = "First principal component",
       y = "Second principal component",
       color = "Cluster",
       title = "Animated display of k-means clustering",
       subtitle = "\nCoordinates generated by principal component analysis",
       caption = "Rhombi indicate centroids") +
  theme_minimal() + 
  theme(
    text = element_text(family = "Roboto"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 17, hjust = 0.5),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    axis.title.x = element_text(size = 15,face = "bold"),
    axis.title.y = element_text(size = 15,face = "bold"),
    plot.caption = element_text(size = 13),
    legend.text = element_text(size = 11)
    ) +
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5)) + 
  transition_states(
    iteration,
    transition_length =0.001,
    state_length = 0.5
  )

animate(gif_1, nframes = 30, height = 600, width =1000)
  
anim_save("k_equals_3.gif")

# K = 5 -------------------------------------------------------------------
l <- k_means_algorithm(y, 4,runs = 10)


##
assignments <- bind_cols(l[[1]])
centroids <- bind_rows(l[[2]]) %>% mutate(cluster = rep(1:4,10),
                                          iteration = rep(1:10,each = 4))


##
tibble_with_assignments <- pc_tibble %>% bind_cols(assignments) %>%
  gather(iteration, assignment, -pc1,-pc2,-species) %>%
  mutate(iteration = as.numeric(str_replace(iteration, "V","")))



##
gif_2 <- tibble_with_assignments %>%
  filter(iteration <10) %>%
  ggplot(aes(x = pc1, y = pc2)) + 
  geom_point(size = 4) + 
  geom_point(aes(x = pc1, y = pc2, color = as.character(assignment)), size = 4, show.legend = TRUE) +
  geom_point(aes(x = pc1, y = pc2),data = centroids %>% filter(iteration <10), shape = 18, size = 5) + 
  labs(x = "First principal component",
       y = "Second principal component",
       color = "Cluster",
       title = "Animated display of k-means clustering",
       subtitle = "\nCoordinates generated by principal component analysis",
       caption = "Rhombi indicate centroids") +
  theme_minimal() + 
  theme(
    text = element_text(family = "Roboto"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 17, hjust = 0.5),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    axis.title.x = element_text(size = 15,face = "bold"),
    axis.title.y = element_text(size = 15,face = "bold"),
    plot.caption = element_text(size = 13),
    legend.text = element_text(size = 11)
  ) +
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5)) + 
  transition_states(
    iteration,
    transition_length =0.001,
    state_length = 0.5
  )


animate(gif_2, nframes = 30, height = 600, width =1000)
anim_save("k_equals_4.gif")
