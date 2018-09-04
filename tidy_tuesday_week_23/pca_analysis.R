rm(list =ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)


# Begin analysis ----------------------------------------------------------
library(tidyverse)
x <- read_csv("fastfood_calories.csv")


# Input mean function
input_mean <- function(x){
    return(round(ifelse(is.na(x), mean(x, na.rm = TRUE), x),2))
}

for_correlation <- x %>% select_if(is_numeric) %>%
    mutate_all(input_mean)

R <- cor(for_correlation) %>% round(3)

##Range from -0.15 to 0.99
R %>% as_tibble() %>%
    mutate(var_1 = rownames(R)) %>%
    select(var_1, calories:calcium) %>%
    gather(var_2,cor, -var_1) %>% 
    filter(cor !=1) %>%
    arrange(var_1) %>%
    mutate(var_1 = str_to_title(str_replace(var_1, "_",". ")),
           var_2 = str_to_title(str_replace(var_2, "_"," "))) %>%
    ggplot(aes(x = var_1, y = var_2, fill = cor,
               label = round(cor, 2))) + 
    geom_tile(col = "black") + theme_bw() + 
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank()) + 
    scale_fill_gradient2(low = "steelblue", high = "orange",mid = "grey",
                        midpoint = 0.5,
                        breaks = seq(-0.15, 0.99, length.out = 5),
                        labels = c("-0.15","0.13", "0.42", "0.71","0.99")) + 
    geom_text(color = "black", size =3) + 
    labs(x = "First variable", y= "Second variable",
         fill = "Correlation") + 
    scale_y_discrete(limits = rev(c("Cal Fat", "Calcium","Calories", "Cholesterol",
                                "Fiber", "Protein", "Sat Fat", "Sodium", "Sugar",
                                "Total Carb", "Total Fat", "Trans Fat", "Vit A", "Vit C"))) + 
    scale_x_discrete(position = "top")

##Start PCA
pca_fit <- princomp(for_correlation, cor = TRUE)

summary(pca_fit, loadings = TRUE)
unclass(summary(pca_fit, loadings = TRUE)$loadings) %>% round(2) %>%
    as_tibble() %>%
    select(`Comp.1`, `Comp.2`) %>%
    mutate(variable = rownames(unclass(summary(pca_fit, loadings = TRUE)$loadings))) %>%
    select(variable, Comp.1, Comp.2) %>%
    mutate(Comp.1 = ifelse(abs(Comp.1) <0.1, "", Comp.1),
           Comp.2 = ifelse(abs(Comp.2) <0.1, "", Comp.2))

##
top_pc1 <- pca_fit$scores[,1:2] %>% as_tibble() %>%
    mutate(restaraunt = x$restaurant, item = x$item) %>%
    select(restaraunt, item, Comp.1, Comp.2) %>%
    arrange(desc(Comp.1)) %>%slice(1:4)


top_pc2 <- pca_fit$scores[,1:2] %>% as_tibble() %>%
    mutate(restaraunt = x$restaurant, item = x$item) %>%
    select(restaraunt, item, Comp.1, Comp.2) %>%
    arrange(desc(Comp.2)) %>% slice(1:4)


pca_fit$scores[,1:2] %>% as_tibble() %>%
    mutate(restaraunt = x$restaurant, item = x$item) %>%
    select(restaraunt, item, Comp.1, Comp.2) %>%
    ggplot(aes(x = Comp.1, y = Comp.2, color = restaraunt)) + 
    geom_point(size = 0.75, show.legend = FALSE) + theme_minimal() + 
    labs(x = "First principal component",
         y = "Second principal component",
         color = "Restaurant") + 
    geom_text(aes(x = Comp.1, y = Comp.2, label = item), data = top_pc1, 
              size = 2.8, col = "black", nudge_x = 1,nudge_y = -0.5) +
    geom_text(aes(x = Comp.1, y = Comp.2, label = item), data = top_pc2, 
              size = 2.8, col = "blue",check_overlap = TRUE) +  
    scale_x_continuous(limits = c(-5,25)) + 
    scale_y_continuous(limits = c(-10, 15))
    
