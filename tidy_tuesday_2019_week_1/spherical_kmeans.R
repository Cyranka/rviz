remove(list = ls())
options(scipen = 999)
library(tidyverse);library(lubridate);library(tidytext);library(viridis)

x <- readRDS("rstats_tweets.rds")


url_regex <- '(http|https)[^([:blank:]|\\"|<|&|#\n\r)]+'

x <- x %>% 
    mutate(created_at = ymd(str_extract(x$created_at,
                                        "[0-9]{4}-[0-9]{2}-[0-9]{2}")),
           text = rtweet::plain_tweets(str_replace(text, url_regex, ""))) %>%
    filter(year(created_at) >= 2009)

##
set.seed(10)
df_sample <- x %>% group_by(year(created_at)) %>%
    sample_n(2500) %>% ungroup() %>%
    select(-`year(created_at)`)

tidy_tokens <- df_sample %>% select(status_id, text) %>%
    unnest_tokens(word, text) %>% anti_join(stop_words) %>%
    filter(nchar(word) >3) %>%
    filter(!str_detect(word, pattern = "\\.|[0-9]|rstats|times")) %>%
    mutate(word = case_when(
        word == "lines" ~"line",
        word == "updates" ~"update",
        word == "languages" ~ "language",
        word == "users" ~"user",
        word == "models" ~"model",
        word == "packages" ~"package",
        TRUE ~ word
    ))

##Cast as DTM
dtm <- tidy_tokens %>% group_by(status_id, word) %>% count() %>%
    cast_dtm(status_id,word,n, weighting = tm::weightTfIdf)


#Remove sparse terms and empty documents
dtm_2 <- tm::removeSparseTerms(dtm,sparse = 0.997)
rowTotals <- apply(dtm_2 , 1, sum)
dtm_2 <- dtm_2[rowTotals >0,]

#
set.seed(10)
n_cluster <- 10
sk_means <- skmeans::skmeans(dtm_2, n_cluster, m = 1, control = 
                         list(nruns = 5, verbose = TRUE))


##Table of cluster assignments
assignments <- tidy(sk_means$cluster) %>% count(x) %>%
    mutate(proportion = n/sum(n)) %>%
    magrittr::set_colnames(c("Cluster", "Total", "Proportion"))

assignments %>% 
    ggplot(aes(x = factor(Cluster), y = Proportion)) + geom_col()

#
gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
}

word_df <-t(clue::cl_prototypes(sk_means))


word_list <- list()

for(i in 1:n_cluster){
    data_frame <- dplyr::as_data_frame(sort(word_df[,i], decreasing = T)[1:15]) 
    colnames(data_frame) <- c("score")
    word_list[[i]] <- tibble(word = row.names(data_frame), score = data_frame$score) %>% filter(!word == "things")
    colnames(word_list[[i]]) <- c("word", "weighted_score")
    word_list[[i]]$cluster <- as.character(i)
}
remove(i)


##
cluster_words <- bind_rows(word_list)


#Now create the word cloud
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
        plot.background = element_rect(fill = "black"),
        plot.title = element_text(color = "white",face = "bold", size = 15, hjust = 0.5),
        plot.subtitle = element_text(color = "white",size =13, hjust = 0.5),
        axis.line.x = element_line(size = 0.5, color = "gray30"),
        axis.title.x = element_text(color = "white",size = 13, face = "bold"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(color ="white"),
        axis.title.y = element_text(color = "white", size = 13),
        plot.caption = element_text(color = "white",size = 11),
        strip.text = element_text(color = "white",size = 11),
        strip.background = element_rect(fill = "gray30", color = "gray30")
    ) + 
    labs(x = "Most important words by cluser",
         y = "\nCluster",
         title = "Clustering of tweets with the hashtag #rstats",
         subtitle = "Clustering determined by spherical K-means", 
         caption = "Based on a sample of 21,763 tweets\nStop words were removed") 

cluster_words %>% write_csv("cluster_words.csv")