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
          panel.grid.major = element_blank(),
          plot.title = element_text(size = 15, face = "bold"),
          plot.subtitle = element_text(size = 13)) + 
    scale_fill_gradient2(low = "deepskyblue1", high = "darkorange1",mid = "grey",
                        midpoint = 0.5,
                        breaks = seq(-0.15, 0.95, length.out = 5),
                        labels = c("-0.15","0.13", "0.42", "0.71","0.95")) + 
    geom_text(color = "black", size =3) + 
    labs(x = "", y= "",
         fill = "Correlation",
         title = "Correlation table between variables in fast food dataset",
         subtitle = "Missing values replaced by the mean",
         caption = "Tidy tuesday week 23") + 
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
    arrange(desc(Comp.2)) %>% slice(1:4) %>%
  mutate(Comp.1 = ifelse(Comp.1 < 1.5, -0.5, Comp.1)) %>%
  mutate(Comp.2 = ifelse(Comp.2 < 5.3, 5.05, Comp.2))


pca_fit$scores[,1:2] %>% as_tibble() %>%
    mutate(restaraunt = x$restaurant, item = x$item) %>%
    select(restaraunt, item, Comp.1, Comp.2) %>%
    ggplot(aes(x = Comp.1, y = Comp.2, color = restaraunt)) + 
    geom_point() + theme_minimal() + 
    labs(x = "First principal component",
         y = "Second principal component",
         color = "Restaurant",
         title = "Scatterplot of the first two principal components",
         subtitle = "Highest scoring meals in the first component labeled in black\nHighest scoring meals in the second component labeled in blue") + 
    geom_text(aes(x = Comp.1, y = Comp.2, label = item), data = top_pc1, 
              size = 2.8, col = "black", nudge_x = 1,nudge_y = -0.5) +
    geom_text(aes(x = Comp.1, y = Comp.2, label = item), data = top_pc2, 
              size = 2.8, col = "blue",check_overlap = TRUE, nudge_y = 0.5) +  
    scale_x_continuous(limits = c(-5,25)) + 
    scale_y_continuous(limits = c(-10, 15)) + 
    theme(plot.title = element_text(size = 15, face = "bold"),
          plot.subtitle = element_text(size = 10))
    

##
unclass(summary(pca_fit, loadings = TRUE)$loadings) %>% round(2) %>%
  as_tibble() %>%
  select(`Comp.1`, `Comp.2`) %>%
  mutate(variable = rownames(unclass(summary(pca_fit, loadings = TRUE)$loadings))) %>%
  select(variable, Comp.1, Comp.2) %>%
  gather(component, value, -variable) %>%
  mutate(variable = ifelse(component == "Comp.1", paste0(" ",variable),variable)) %>%
  mutate(variable = str_to_title(str_replace(variable, "_",". ")),
         component = str_replace_all(component, "Comp.", "Principal component ")) %>%
  ggplot(aes(x = reorder(variable, value), y = value, fill = value >0,
             label = value)) + geom_col(show.legend = FALSE, color = "black") + coord_flip() +
  facet_wrap(~component, scales = "free") + theme_bw() + geom_label(fill = "white", size = 3) + 
  labs(x = "Variable", y = "Factor loadings",
       title = "Factor loadings for the first two principal components",
       subtitle = "First principal component mostly concerned with high-calory, high-fat meals\nSecond principal component mostly concerned with high-fiber, high-carb meals",
       caption = "Tidy tuesday week 23") +  
  theme(plot.title = element_text(size = 15, face = "bold"),
        plot.subtitle = element_text(size = 10))
  
