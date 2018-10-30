remove(list = ls())
#setwd("")
##Set Options
options(stringsAsFactors = FALSE)
options(scipen = 999)
Sys.setlocale('LC_ALL','C')

##add Libraries
library(skmeans)
library(tm)
library(clue)
library(cluster)
library(fpc)
library(clue)
library(wordcloud)
library(dplyr)
library(ggplot2)
library(viridis)
##
my_words <- readxl::read_excel("most_common_words_english_for_twitter.xlsx")
##Define Clean Corpus Function

clean.corpus <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords('en'),'lol',
                                          'tbh', 'smh', 'dm','np',"gmail","life","live","hotmail", "email", "e-mail","snap", "snapchat",
                                          "tweets", "twitter", "tweet","instagram","got","official", "com","world", "like", "youtube","without","high","born", "try",
                                          "better","help","former","since","every","views","mine","stuff",my_words$word))
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}


x <- readr::read_csv("bios_for_clustering.csv")
x_source <- VCorpus(VectorSource(x$text))
x_corpus <- clean.corpus(x_source) 

x_dtm <- DocumentTermMatrix(x_corpus,
                             control = list(weighting = weightTfIdf))

x_dtm_new <- removeSparseTerms(x_dtm, 0.995)


##Remove Empty Columns
rowTotals <- apply(x_dtm_new , 1, sum)
x_dtm_new <- x_dtm_new[rowTotals >0,]


set.seed(10)
change_number <- 10
soft.part <- skmeans(x_dtm_new, change_number, m = 1, control = 
                       list(nruns = 5, verbose = TRUE))

barplot(table(soft.part$cluster), main = 'Spherical K-means')
s.clus.proto <- t(cl_prototypes(soft.part))

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

##

comparison.cloud(s.clus.proto, max.words = 200, title.size = 0.01,random.order = FALSE, colors = gg_color_hue(change_number))

##

word_list <- list()

for(i in 1:change_number){
  data_frame <- as.data.frame(sort(s.clus.proto[,i], decreasing = T)[1:10]) 
  colnames(data_frame) <- c("score")
  word_list[[i]] <- tibble(word = row.names(data_frame), score = data_frame$score) %>% filter(!word == "things")
  colnames(word_list[[i]]) <- c("word", "weighted_score")
  word_list[[i]]$cluster <- as.character(i)
}
remove(i)



cluster_words <- bind_rows(word_list)
writexl::write_xlsx(cluster_words, "cluster_words.xlsx")
#repetition_words <- cluster_words %>% group_by(word) %>% tally()

#cluster_words <- cluster_words %>% inner_join(repetition_words) %>% filter(n <2) %>% select(-n)
cluster_words <- readxl::read_excel("cluster_words.xlsx")
cluster_words %>%
  group_by(word) %>% top_n(1, wt = weighted_score) %>%
  arrange(desc(weighted_score)) %>%
  ungroup() %>%
  ggplot(aes(
    x = reorder(stringr::str_to_title(word), weighted_score),
    y = weighted_score,
    fill = cluster
  )) + geom_col(show.legend = FALSE) +
  facet_wrap( ~ factor(cluster, levels = c(
    "1", "2", "3", "4", "5",
    "6", "7", "8", "9", "10"
  )), scales = "free",nrow = 2) +
  coord_flip() +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    plot.subtitle = element_text(size =13, hjust = 0.5),
    axis.line.x = element_line(size = 0.5, color = "gray30"),
    axis.title.x = element_text(size = 13, face = "bold"),
    axis.text.x = element_blank(),
    axis.title.y = element_text(size = 13),
    plot.caption = element_text(size = 13),
    strip.text = element_text(size = 11),
    strip.background = element_rect(fill = "gray90", color = "gray90")
  ) + 
  labs(x = "Most important words by cluser",
       y = "\nCluster",
       title = "R4DS twitter followers: bio clustering",
       subtitle = "Clustering determined by spherical K-means algorithm", 
       caption = "Based on 1,500 randomly sampled bios in English") + 
  scale_fill_viridis(option = "A",discrete = TRUE, end = 0.9)
  


table(soft.part$cluster)
round(prop.table(table(soft.part$cluster))*100,1)

1500 - sum(table(soft.part$cluster))  ##Empty Bios


