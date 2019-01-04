remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/sou_files/")
library(tidyverse);library(tidytext);library(topicmodels)

files_to_read <- grep("\\.txt", list.files(), value = TRUE)
speech_names <- str_replace(str_to_title(str_replace(files_to_read,pattern = "_", " ")),"\\.Txt","")
x <- lapply(files_to_read, function(i)read_lines(i) %>% as_tibble() %>% filter(value != "") %>%
              mutate(sentence = row_number()))

x <- lapply(x, function(i) i %>% mutate(value = str_replace(value, "\\[Laughter\\]|\\[laughter\\]"," ")))


##
match_to_sentiment <- function(list_frame){
  z <- list_frame %>% unnest_tokens(word, value) %>% inner_join(get_sentiments("bing")) %>%
    filter(!word %in% c("work", "like", "right")) %>%
    group_by(sentence, sentiment) %>% tally() %>%
    ungroup() %>% spread(sentiment, n, fill = 0) %>%
    mutate(total = positive - negative)
  
  return(z)
}

x_1 <- lapply(x, match_to_sentiment)
x_1 <- lapply(1:length(x), function(i)x_1[[i]] %>% mutate(pres_year = speech_names[i]))

#
my_df <- bind_rows(x_1)

##
my_df %>%
  ggplot(aes(x = sentence, y =total, fill = total >0)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~pres_year, scales = "free_x", nrow = 3) + 
  labs(x = "Section",
       y = "Sentiment",
       title = "Sentiment analysis of State of the Union speeches using the BING dictionary",
       subtitle = "Some words were removed from dictionary") + 
  theme_minimal() + 
  theme(
    text = element_text(family = "Roboto"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(size = 0.15,color = "gray90"),
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 16),
    strip.background = element_rect(fill = "gray80",color = "gray80"),
    strip.text = element_text(face = "bold")
  ) + 
  scale_fill_manual(values = c("firebrick3", "darkblue"))

##Most common negative and positive words
sentiment_tally <- function(list_frame){
  z <- list_frame %>% unnest_tokens(word, value) %>% inner_join(get_sentiments("bing")) %>%
    group_by(word, sentiment) %>% tally() %>%
    filter(!word %in% c("work", "like","right"))
  return(z)
}


my_tally <- bind_rows(lapply(x, sentiment_tally))%>% ungroup()
my_tally %>% group_by(word, sentiment) %>%
  summarise(total = sum(n)) %>%
  arrange(sentiment, desc(total)) %>%
  group_by(sentiment) %>%
  top_n(20,total) %>%
  mutate(total = ifelse(sentiment == "negative", total*-1,total)) %>%
  ggplot(aes(x = reorder(word, total), y = total, fill = str_to_title(sentiment))) + 
  geom_col() + coord_flip() + 
  theme_minimal() + 
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(size = 0.1,linetype = 1,color = "gray80"),
    plot.title = element_text(size = 18, face = "bold"),
    text = element_text(family = "Roboto")
  ) + 
  scale_fill_manual(values = c("firebrick3", "darkblue")) + 
  labs(x ="Word", y = "Contribution to sentiment", title  = "Most common positive/negative words in SOU speeches since 2001") + 
  guides(fill = guide_legend(title = "Sentiment",
                             title.position = "top",
                             title.hjust = 0.5,
                             label.position = "bottom",
                             keywidth = 3, keyheight = 0.5))


##LDA
transform_into_unnest_tokens <- function(x){
  y <-x %>% select(value) %>% unnest_tokens(word, value) %>% anti_join(stop_words)
  return(y)
}

df_list <- lapply(x,function(y)transform_into_unnest_tokens(y))

for(i in 1:length(df_list)){
  df_list[[i]]$year <- speech_names[i]
  print(i)
}
remove(i)

##Get TF-IDF
tf_idf_frame <- df_list %>% bind_rows() %>%
  count(word, year, sort = TRUE)%>% bind_tf_idf(word,year, n) %>%
  filter(tf_idf >0)

##
dtm_1 <- tf_idf_frame %>%
  select(word, year,n) %>%
  cast_dtm(year, word, n)

##Words in each topic
sou_lda <- LDA(dtm_1, k =10, control = list(seed = 1234))
sou_topics <- tidy(sou_lda, matrix = "beta")
top_terms <- sou_topics %>% 
  filter(!str_detect(term, "[0-9]")) %>%
  group_by(topic) %>%
  arrange(topic,desc(beta)) %>%
  slice(1:20) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>% 
  group_by(term)%>% top_n(1,wt = beta) %>%
  ggplot(aes(x = reorder(term, beta), y = beta, fill = topic)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~topic, scales = "free", ncol = 2) + coord_flip() + 
  theme_minimal() + 
  theme(
    text = element_text(family = "Roboto", size = 13),
    plot.title = element_text(family = "Roboto", size = 17, face = "bold"),
    plot.subtitle = element_text(family = "Roboto", size = 17, face = "bold"),
    panel.grid = element_line(size =0.05, linetype = 1, color = "black"),
    strip.background = element_rect(fill = "gray95", color = "white")
  ) + 
  labs(x = "Beta", y = "Term", title = "Most common terms in each topic in State of the Union speeches since 2001", 
       subtitle = "LDA model with 10 topics", 
       caption = "Duplicate words removed") + 
  viridis::scale_fill_viridis(option = "D",begin =.2,end = 0.9)

##topics in each document
sou_gamma <- tidy(sou_lda, matrix = "gamma")
sou_gamma %>% mutate(document = reorder(document, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ document, scales = "free_x")


##Avg sentiment
x_1 %>% bind_rows() %>%
  group_by(pres_year) %>% summarise(avg_sentiment = mean(total)) %>%
  arrange(desc(avg_sentiment)) %>%
  ggplot(aes(x = reorder(pres_year, avg_sentiment), y = avg_sentiment)) + 
  geom_col() + coord_flip() +
  labs(x = "Speech", y = "Average sentiment")