remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/sou_files/")
library(tidyverse);library(tidytext);library(topicmodels)

files_to_read <- grep("\\.txt", list.files(), value = TRUE)
speech_names <- str_replace(str_to_title(str_replace(files_to_read,pattern = "_", " ")),"\\.Txt","")
x <- lapply(files_to_read, function(i)read_lines(i) %>% as_tibble() %>% filter(value != "") %>%
              mutate(sentence = row_number()))

x <- lapply(x, function(i) i %>% mutate(value = str_replace(value, "\\[Laughter\\]|\\[laughter\\]"," "),
                                        value = str_replace(value, "--"," - ")))


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


##
x_1 %>% bind_rows() %>%
  group_by(pres_year) %>% summarise(avg_sentiment = mean(total)) %>%
  arrange(desc(avg_sentiment)) %>%
  ggplot(aes(x = reorder(pres_year, avg_sentiment), y = avg_sentiment)) + 
  geom_col() + coord_flip() +
  labs(x = "Speech", y = "Average sentiment") + 
  theme_minimal()



# Open NLP Section --------------------------------------------------------
library(NLP)
library(openNLP)
library(openNLPmodels.en)

gc()

###Load NLP Functions
persons <- Maxent_Entity_Annotator(kind = "person",language = "en")
locations <- Maxent_Entity_Annotator(kind = "location",language = "en")
organizations <- Maxent_Entity_Annotator(kind = "organization",language = "en")
sent.token.annotator <- Maxent_Sent_Token_Annotator(language = "en")
word.token.annotator <- Maxent_Word_Token_Annotator(language = "en")
pos.tag.annotator <- Maxent_POS_Tag_Annotator(language = "en")


##Apply to one speech
annotations <- annotate(paste0(x[[9]]$value, collapse = " "),
                        list(sent.token.annotator, word.token.annotator,
                             pos.tag.annotator,persons, locations, organizations))


obama_09 <- as_data_frame(annotations)[,2:5]
obama_09$features <- unlist(as.character(obama_09$features))


my_chars <- NULL
for(i in 1:nrow(obama_09)){
  my_chars[i] <- 
    substr(paste0(x[[9]]$value, collapse = " "), obama_09[i,2], obama_09[i,3])
  print(i)
}
remove(i)

obama_09$words <- my_chars

