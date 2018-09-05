rm(list =ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

# Begin analysis ----------------------------------------------------------
library(tidyverse)
x <- read_csv("fastfood_calories.csv") %>%
  mutate(item = rtweet::plain_tweets(item))

# Input mean function
input_mean <- function(x){
  return(round(ifelse(is.na(x), mean(x, na.rm = TRUE), x),2))
}

y <- suppressWarnings(x %>% select_if(is_numeric) %>%
                        mutate_all(input_mean) %>%
                        mutate(item = x$item, 
                               restaurant = x$restaurant) %>%
                        select(restaurant, item, calories:protein))


##modeling
library(caret)
full_data <- y %>% select(-item)%>%
  mutate(restaurant = factor(restaurant))


##Split Train/Test Set
set.seed(15)
rowsToTrain <- createDataPartition(full_data$restaurant, p = 0.8, list = F)
trainSet <- full_data[rowsToTrain,] %>%
  mutate(restaurant = factor(str_to_lower(make.names(restaurant))))
testSet <- full_data[-rowsToTrain,]%>%
  mutate(restaurant = factor(str_to_lower(make.names(restaurant))))


##Training control
fitControl <- trainControl(## 10-fold CV
  method = "cv",
  number = 10)

##
grid <-expand.grid(.mtry=seq(1,10,by = 1))

##Fitting a random forest

set.seed(10)
rfFit <- train(restaurant~., data = trainSet,
                  method = "rf",
                  metric = "Kappa",
                  tuneGrid = grid,
                  trControl = fitControl)

rfFit$results ##
confusionMatrix(rfFit)

testSet <- testSet %>% mutate(predictions = predict(rfFit, testSet))
results_graph <- testSet %>% group_by(restaurant, predictions) %>% tally() %>%ungroup() %>%
  spread(predictions,n,fill = 0) %>%
  gather(predictions, n,-restaurant) %>%  arrange(restaurant) %>%
  group_by(restaurant) %>%
  mutate(percent = round(n/sum(n),3)*100) %>% ungroup() %>%
  mutate(restaurant = str_to_title(str_replace_all(restaurant,"\\."," ")),
         predictions = str_to_title(str_replace_all(predictions,"\\."," ")))

results_graph %>%
  ggplot(aes(x = reorder(predictions, percent), y = percent + 0.5, label = paste0(percent, "%"), fill = restaurant)) + 
  facet_wrap(~restaurant, scales = "free_y") + geom_col(show.legend = FALSE, color = "black") + 
  theme_minimal() + coord_flip() + 
  labs(x = "Predictions", y = "Percent",title = "Ensemble method predictions on test set",
       subtitle = "Model: Random forest (Number of trees = 500, variables at each split =3)",
       caption = "Metric maximized: Kappa\nTest set accuracy: 66.3%") + geom_label(size = 3, fill = "white",nudge_y = 4) + 
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "grey", color = "gray20"),
        strip.text = element_text(size = 10, face = "bold"),
        plot.title = element_text(size = 15, face = "bold")) + 
  scale_y_continuous(limits = c(0,105)) + 
  scale_fill_brewer(palette = "Oranges")

varImp(rfFit, scale = FALSE)

###Fitting rpart
set.seed(10)
rpartFit <- train(restaurant~., data = trainSet,
               method = "rpart",
               metric = "Kappa",
               tuneLength = 15,
               trControl = fitControl)

rpartFit$results ##
confusionMatrix(rpartFit)
testSet2 <- testSet %>% mutate(predictions = predict(rpartFit, testSet))

results_rpart <- testSet2 %>% group_by(restaurant, predictions) %>% tally() %>%ungroup() %>%
  spread(predictions,n,fill = 0) %>%
  gather(predictions, n,-restaurant) %>%  arrange(restaurant) %>%
  group_by(restaurant) %>%
  mutate(percent = round(n/sum(n),3)*100) %>% ungroup() %>%
  mutate(restaurant = str_to_title(str_replace_all(restaurant,"\\."," ")),
         predictions = str_to_title(str_replace_all(predictions,"\\."," ")))


results_rpart %>%
  ggplot(aes(x = reorder(predictions, percent), y = percent + 0.5, label = paste0(percent, "%"), fill = restaurant)) + 
  facet_wrap(~restaurant, scales = "free_y") + geom_col(show.legend = FALSE, color = "black") + 
  theme_minimal() + coord_flip() + 
  labs(x = "Predictions", y = "Percent",title = "Classification trees predictions on test set",
       subtitle = "Model: Classification tree (Complexity parameter = 0.007)",
       caption = "Metric maximized: Kappa\nTest set accuracy: 44.6%") + geom_label(size = 3, fill = "white",nudge_y = 4) + 
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "grey", color = "gray20"),
        strip.text = element_text(size = 10, face = "bold"),
        plot.title = element_text(size = 15, face = "bold")) + 
  scale_y_continuous(limits = c(0,105)) + 
  scale_fill_brewer(palette = "Blues")

##
rpart.plot::rpart.plot(rpartFit$finalModel, extra = 2, main = "Decision tree model (Complexity parameter = 0.007)", fallen.leaves = FALSE,
                       cex = .6, clip.facs = TRUE,trace = -1)