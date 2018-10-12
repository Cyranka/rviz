remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/tidy_tuesday_week_20/")
library(tidyverse);library(tidytext)


##Bring and sample data
x <- read_csv("sampled_tweets.csv") %>%
  filter(account_type %in% c("left", "Right") & language == "English")
x <- x %>% mutate(account_type = str_to_title(account_type))

set.seed(1)
x_2 <- x %>% sample_n(10000) %>% dplyr::rename(text = content) %>%
  mutate(text = rtweet::plain_tweets(text)) %>%
  mutate(document = 1:10000)

###
tidy_tweets <- x_2 %>% select(document, account_type,text) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  group_by(word) %>% filter(n() > 10) %>%
  ungroup()

##Create a sparse matrix
sparse_tweets <- tidy_tweets %>% dplyr::count(document, word, sort = TRUE) %>%
  cast_sparse(document, word, n)

tweets_joined  <- data_frame(document = (as.integer(rownames(sparse_tweets)))) %>%
  left_join(x_2) %>% select(document,account_type)


##Fit the model
library(glmnet)
response_model <- tweets_joined$account_type == "Right"

lasso_model <- cv.glmnet(sparse_tweets,
                         y = response_model,
                         family = "binomial",
                         alpha = 0,
                         nfolds = 10)

##Tidying the model
library(broom)

coefficients <- lasso_model$glmnet.fit %>% tidy() %>%
  filter(lambda == lasso_model$lambda.min)

##Predict probabilities
classifications <- tidy_tweets %>% inner_join(coefficients, by = c("word" = "term")) %>%
  group_by(document) %>%
  summarize(Score = sum(estimate)) %>%
  mutate(probability = arm::invlogit(0.366 + Score)) %>%
  mutate(predictions = ifelse(probability > 0.55, "Right", "Left"))%>% ##Increasing threshold
  inner_join(x_2 %>% select(document, account_type))

##Create confusion matrix: wrong because this is training data
classifications %>% group_by(predictions, account_type) %>% tally() %>%
  spread(account_type, n)

##
coefficients %>% filter(term!="(Intercept)") %>%
  top_n(n = 12, estimate) %>% arrange(desc(estimate)) %>%
  bind_rows(coefficients %>% filter(term!="(Intercept)") %>%
              top_n(n = -12, estimate) %>% arrange(estimate)) %>% 
  ggplot(aes(x = reorder(term, estimate), y = estimate, fill = estimate >0)) + geom_col(show.legend = FALSE) + coord_flip() + 
  theme_minimal() + 
  labs(x = "Term", y = "Coefficient", title = "Largest and smallest coefficients: Lasso fit") + 
  theme(
    text = element_text(family = "Roboto")
  )


##Total zero coefficients
lasso_model$nzero

##Get ROC curve: Test set
account_classes <- classifications %>% mutate(Correct = (predictions == account_type))
original_classifications <- as.numeric(factor(account_classes$account_type)) - 1
predictions <- as.numeric(factor(account_classes$predictions)) - 1
pROC::roc(original_classifications, predictions)

##Predicted right, but actually left
mistakes <- classifications %>% filter(account_type == "Left" & probability > 0.7)


##Do a word cloud of the mistakes 
x_2 %>% filter(document %in% mistakes$document) %>% 
  select(text) %>% unnest_tokens(word, text, token = "tweets") %>%
  anti_join(stop_words) %>% group_by(word) %>% tally() %>% arrange(desc(n)) %>%
  slice(1:200) %>%
  wordcloud2::wordcloud2(color = "firebrick", fontFamily = "Roboto")

#Pretty good job of identifying right wing accounts
classifications %>%
  ggplot(aes(x = probability*100, y = ..density.., fill = account_type)) + 
  geom_density(adjust = 3, show.legend = TRUE,alpha = 0.5) + 
  theme_minimal() + 
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold")) + 
  labs(x = "Predicted probability of being a right-wing account", y= "Density") + 
  scale_fill_manual(values = c("navyblue", "firebrick3")) + 
  guides(fill = guide_legend(title = "Account type", title.position = "top", 
                             title.hjust = 0.5, barwidth = 20,
                             barheight = 0.5,keywidth = 5,keyheight = 0.5,nrow = 1,label.position = "bottom"))


classifications %>% filter(account_type == "Left") %>%
  pull(probability) %>% quantile(probs = seq(0,1,by = 0.1)) %>% round(3)*100

