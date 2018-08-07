remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("")
library(tidyverse);library(tidytext)

x <- read_csv("hadley_api.csv") %>% dplyr::rename(text = description)

z <- readxl::read_excel("most_common_words_english_for_twitter.xlsx")
replace_reg <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"


##Create tidy bios and get top unigrams
tidy_bios <- x %>% select(screenName,text) %>% mutate(text = str_replace_all(text, replace_reg, "")) %>%
  unnest_tokens(word, text, token = "regex",pattern = unnest_reg) %>% filter(!word %in% stop_words$word,
                                                                             str_detect(word,"[a-z]")) %>%
  filter(!word %in% z$word)

top_unigrams <- tidy_bios %>% dplyr::count(word, sort = TRUE) ##Top Unigrams
top_unigrams

###Create Unigram Plot
png(filename="one_word_twitter.png",width=800, height=500)
plot(tidy_bios %>% group_by(word) %>% dplyr::count(word, sort = TRUE) %>% ungroup() %>% slice(1:20) %>% mutate(word = str_to_title(word)) %>%ggplot(aes(reorder(word, n), n)) + geom_col(show.legend = FALSE, fill = "#018270") + 
       coord_flip() + labs(y = "Count", x = "Word",
                           title = "Most common unigrams in @hadleywickham's twitter followers bios") + theme_bw() + theme(text = element_text(size=14)))

dev.off()

##Create tidy bigrams and get plot
##Two Words
tidy_bigrams <- x %>% select(screenName, text) %>% mutate(text = str_replace_all(text, replace_reg, "")) %>%
  unnest_tokens(word, text, token = "ngrams",n = 2)

count_bigrams <- tidy_bigrams %>% group_by(word) %>% tally() %>% arrange(desc(n)) %>% ungroup() %>% separate(word, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word) %>%
  filter(!word1 %in% z$word, !word2 %in% z$word) %>%
  filter(!is.na(word1),!is.na(word2))

png(filename="two_words_twitter.png",width=800, height=500)  
plot(count_bigrams %>% unite("word", c("word1", "word2"), sep = " ") %>% slice(1:20) %>% mutate(word  = str_to_title(word)) %>%
       ggplot(aes(reorder(word,n),n)) +  geom_col(show.legend = FALSE, fill = "#018270") + theme_bw() + coord_flip() + labs(y = "Count", x = "Words",
                                                                                                                           title = "Most common bigrams in @hadleywickham's twitter followers bios") + theme(text = element_text(size=14)))
dev.off()

##
library(widyr)
bio_word_pairs <- tidy_bios %>% pairwise_count(word, screenName, sort = TRUE)
bio_word_pairs <- bio_word_pairs %>% mutate(to_filter = as.numeric(row.names(bio_word_pairs)) %%2) %>% filter(to_filter == 0) %>% mutate(to_filter =NULL)

png(filename="word_pairs_twitter.png",width=800, height=500)  
plot(bio_word_pairs %>% unite("word", c("item1", "item2"), sep = "/") %>% slice(1:20) %>% mutate(word  = str_to_title(word)) %>%
       ggplot(aes(reorder(word,n),n)) +  geom_col(show.legend = FALSE, fill = "#018270") + theme_bw() + coord_flip() + labs(y = "Count", x = "Word Pairs",title = "Most common word pairs in @hadleywickham's twitter followers bios") + theme(text = element_text(size=14)))
dev.off()

saveRDS(bio_word_pairs, "bio_word_pairs.RDS")


##Sample 'good' bios for clustering
nice_bios <- x %>% filter(lang == "en" & text != "")
set.seed(144)
nice_bios <-nice_bios[sample(1:nrow(nice_bios),1500),]

readr::write_csv(nice_bios, "bios_for_clustering.csv")
