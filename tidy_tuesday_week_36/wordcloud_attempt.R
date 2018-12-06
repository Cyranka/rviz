remove(list =ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

library(tidyverse);library(tidytext);library(tm);library(slam)

##From wide to long and filter out empty titles
x <- read_csv("medium_datasci.csv") %>% filter(!is.na(title)) %>%
  group_by(title) %>% filter(nchar(title) >120) %>%
  unique() %>% ungroup()


##Get only reliably english
lang_detect <- cldr::detectLanguage(x$title)

x_filter <- x %>% mutate(lang = lang_detect$detectedLanguage,
             reliable = lang_detect$isReliable) %>%
  filter(lang == "ENGLISH" & reliable == TRUE)


##
set.seed(3)
x1 <- x_filter %>% select(-(tag_ai:tag_machine_learning)) %>%
  sample_n(50)%>% rename(doc_id = x1) %>%
  mutate(title = rtweet::plain_tweets(title))


##
tokens_unnested <- x1 %>% select(doc_id,title) %>% 
  mutate(title = removeNumbers(removePunctuation(title))) %>%
  unnest_tokens(word, title)

#Cast as a DTM
tdm <- tokens_unnested %>%
  count(doc_id, word) %>%
  cast_tdm(document = doc_id,term = word,value = n,weighting = tm::weightTfIdf)


#Calculate cosine distance
##Cosine of zero, means they point in the same direction, cosine of 1, means that they point in 'different directions'
library(slam);library(dendextend)

cosine_dist_mat <- 1 - crossprod_simple_triplet_matrix(tdm)/(sqrt(col_sums(tdm^2) %*% t(col_sums(tdm^2))))

##Vanilla dendrogram
hc <- hclust(dist(cosine_dist_mat), method = "average")
plot(hc, cex = 0.8, hang = -1)


##
my_palette <- c("black",viridis::viridis(n=4,begin = 0.6,end = 0.9,option = "B",direction = 1))
dend1 <- color_labels(hc, k = 5,col =my_palette)

plot(dend1 %>% color_branches(k = 5,col =my_palette) %>% set("labels_cex", 0.75),
     ylab = "Labels represent document id",xlab = "Distance",cex.lab = 0.75,
     main = "Hierarchical clustering dendrogram of a sample of english titles with over 120 characters\nBranches and leaves colored by cluster assignment",
     sub = "Tidy tuesday week 36",cex.axis = 0.75, cex.main = 1, cex.sub = 0.65,axes = FALSE)


##

x_cluster <- x1 %>% inner_join(tibble(doc_id = as.integer(names(cutree(hc,k = 5))), cluster = cutree(hc, k = 5))) %>%
  filter(cluster != 5)


for_ggword <- x_cluster %>% select(cluster, title) %>%
  mutate(title = removeNumbers(removePunctuation(title))) %>%
  unnest_tokens(word, title) %>% anti_join(stop_words) %>%
  group_by(cluster, word) %>% tally() %>% ungroup() %>%
  arrange(cluster, desc(n)) %>% group_by(cluster) %>%
  slice(1:20)


for_ggword %>%
  ggplot(aes(label = word, size = n,
             color = as.character(cluster),
             x = as.character(cluster))) + 
  ggwordcloud::geom_text_wordcloud_area(shape = "diamond") + 
  theme_minimal() + 
  scale_x_discrete(breaks = NULL) + 
  scale_size_area(max_size = 25) + 
  theme(
    plot.background = element_rect(fill = "black"),
    text = element_text(family = "Roboto"),
    plot.title = element_text(color = "white", size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(color = "white", size = 12, hjust = 0.5, face = "bold"),
    plot.caption = element_text(color = "white", size = 10, face = "bold"))+ 
  viridis::scale_color_viridis(begin = 0.6, discrete = TRUE, option = "B") + 
  labs(title = "Word cloud of top words by cluster",
       subtitle = "Clusters defined by hierarchical clustering based on a cosine distance matrix",
       caption = "Tidy tuesday week 36\nMedium data science articles metadata\nCluster 5 deleted due to size")

