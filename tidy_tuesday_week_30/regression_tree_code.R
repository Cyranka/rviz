remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/tidy_tuesday_week_30/")
library(tidyverse);library(lubridate);library(ggridges);library(tree)

x <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-10-23/movie_profit.csv") %>%
  mutate(release_date = mdy(release_date)) %>%
  mutate(decade = factor(year(floor_date(release_date, years(10))))) %>%
  filter(!worldwide_gross ==0) %>% select(-X1)


##
x_2 <- x %>% group_by(distributor) %>%
  filter(n() > 11) %>% ungroup() %>%
  filter(!is.na(mpaa_rating)) %>%
  filter(!is.na(distributor)) %>%
  mutate(bomb = factor(ifelse(worldwide_gross - production_budget >0,"No", "Yes"))) %>%
  mutate(month = month(release_date, abbr = TRUE, label = TRUE)) %>%
  mutate(production_millions = production_budget/1000000) %>%
  mutate(log_gross = log(worldwide_gross/1000000))

x_2 <- x_2 %>% mutate_if(is.character, factor)

##Define train rows
set.seed(1)
train_rows <- sample(1:nrow(x_2)/2, nrow(x_2)/2)


#Fit the model: Character columns MUST be converted to factors
tree_movies <- tree(log_gross ~ production_millions + mpaa_rating + genre + month + decade + distributor,
                    data = x_2, subset = train_rows)

summary(tree_movies)

##Plot the tree
plot(tree_movies)
text(tree_movies, pretty = 0,cex = 0.5)

##Tree error
predictions_1 <- predict(tree_movies, newdata = x_2[-train_rows,])
sqrt(mean((x_2[-train_rows,]$log_gross - predictions_1)**2)) ##RMSE in the log scale

##Tree pruning
cv_tree <- cv.tree(tree_movies)
cv_tree

tibble(size = cv_tree$size,
       errors = cv_tree$dev,
       penalty = cv_tree$k)  %>%
  filter(errors == min(errors)) ##No penalty is the best model


##
prediction_tibble <- tibble(test_predictions = predictions_1,
       test_log_gross = x_2[-train_rows,]$log_gross,
       movie = x_2[-train_rows,]$movie)

prediction_values <- sort(unique(predictions_1),decreasing = FALSE)

retrieve_outliers <- function(index){
  k <- prediction_tibble %>% filter(test_predictions ==  prediction_values[index]) %>%
    pull(test_log_gross) %>% boxplot()
  outliers <- k$out
  dev.off()
  return(prediction_tibble %>% filter(test_log_gross %in% outliers))
}

##Plot predictions vs actual values
prediction_tibble%>%
  ggplot(aes(x = as.character(round(test_predictions,1)), y = test_log_gross)) + geom_boxplot() + 
  geom_point(alpha = 0.1) + 
  labs(x = "Regression tree prediction",
       y = "Test log worldwide gross")


retrieve_outliers(6)



