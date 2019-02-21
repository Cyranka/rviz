remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/rviz/w2vec/")
library(text2vec);library(tidyverse);library(tm)

x <- read_rds("rstats_tweets.rds") %>%
    mutate(text = rtweet::plain_tweets(text)) %>%
    filter(account_lang == "en")

###
cleaning_function <- function(x){
    x <- removeWords(x,c(tidytext::stop_words$word,"#rstats"))
    x <- removeNumbers(x)
    x <- removePunctuation(x)
    x <- stripWhitespace(x)
    x <- str_to_lower(x)
    return(x)
}

x <- x %>% mutate(text = cleaning_function(text))

##Tokens
tokens <- str_split(x$text, pattern = " ")
vocab <- create_vocabulary(itoken(tokens), ngram = c(1,1))
vocab <- prune_vocabulary(vocab, term_count_min = 50)

iter <- itoken(tokens)
vectorizer <- vocab_vectorizer(vocab)
tcm <- create_tcm(iter, vectorizer, skip_grams_window = 3)

##fit Glove
fit_glove <- GloVe$new(word_vectors_size = 100,
                       vocabulary = vocab,
                       x_max = 10,
                       learning_rate = 0.1)

word_vectors_main = fit_glove$fit_transform(tcm, n_iter = 15)

word_vectors_context <-  fit_glove$components
word_vectors   <- word_vectors_main + t(word_vectors_context)

row.names(word_vectors) <- rownames(tcm)


# vectors -----------------------------------------------------------------
vector_check <- word_vectors['plot', ,drop = FALSE]

cos_dist <- dist2(vector_check,word_vectors,'cosine',norm = 'l2')
head(sort(1 - cos_dist[1,], decreasing = T), 20)


##
to_kmeans <- as_tibble(word_vectors)
cluster_1 <- kmeans(to_kmeans, centers = 100,iter.max = 25)

cluster_assignment <- tibble(word = row.names(word_vectors),
    cluster = cluster_1$cluster)
 
cluster_centers <- cluster_1$centers
