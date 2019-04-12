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
vector_check_function <- function(word){
    x <- word_vectors[word, ,drop = FALSE] 
    cos_dist <- dist2(x,word_vectors,'cosine',norm = 'l2')
    
    k <- head(sort(1 - cos_dist[1,], decreasing = T), 16)[2:16]
    
    df <- tibble(
        words = names(k),
        cosine = k,
        original_word = word
    )
    return(df)
    
}

words_to_search <- c("clustering","tidyverse",
                     "regression", "trees",
                     "bayesian", "text")

list_1 <- lapply(words_to_search, vector_check_function)
names(list_1) <- words_to_search

# Use cowplot -------------------------------------------------------------
##Function to color
gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
}

my_colors <- gg_color_hue(6)

create_graphs <- function(position){
    j <- list_1[[position]] %>%
        ggplot(aes(x = reorder(words, cosine), y = cosine)) + 
        geom_col(fill = my_colors[position]) + 
        hrbrthemes::theme_ipsum_rc(axis_text_size = 9,
                                   axis_title_size = 9) + 
        coord_flip() + 
        labs(x = "Cosine similarity", y = "Word", 
             title = paste0("Words most similar to: ",names(list_1)[position]),
             subtitle = "Measured by cosine similarity") + 
        theme(
            plot.title = element_text(size = 10),
            plot.subtitle = element_text(size = 8)
        )

    return(j)
} 

graph_list <- lapply(1:6, function(i)create_graphs(i))

cowplot::plot_grid(plotlist = graph_list)

##T-SNE
set.seed(3)
to_kmeans <- as_tibble(word_vectors)
cluster_1 <- kmeans(to_kmeans, centers = 100,iter.max = 25)

cluster_assignment <- tibble(word = row.names(word_vectors),
                             cluster = cluster_1$cluster)

cluster_centers <- cluster_1$centers

##Interesting words
target_words <- c("clustering","tidyverse",
                  "regression", "trees",
                  "bayesian", "text")


word_clusters <- filter(cluster_assignment, word %in% target_words)

cluster_targets <- filter(cluster_assignment, cluster %in% c(word_clusters$cluster))

# Rtsne attempt -----------------------------------------------------------
set.seed(4)
tsne_model1 <- Rtsne::Rtsne(as.matrix(word_vectors[cluster_targets$word,]),check_duplicates = FALSE, perplexity = 50,theta = 0.5,dims = 2)

df_tsne = as_tibble(tsne_model1$Y)  

df_tsne %>% 
    mutate(cluster = cluster_targets$cluster,
           word = cluster_targets$word) %>%
    ggplot(aes(V1, V2, text = paste0("Cluster: ",cluster,"\n",
                                     "Word: ",word))) + 
    ggrepel::geom_text_repel(aes(color = as.factor(cluster),label = word),
                             size = 3,
                             segment.alpha = 0,
                             fontface = "bold",
                             family = "Roboto Condensed") + 
    scale_x_continuous(limits = c(-20,20)) + 
    scale_y_continuous(limits = c(-20,20)) + 
    theme_void() + 
    theme(
        legend.position = "bottom"
    ) + 
    guides(color = guide_legend(title = "Word/cluster",title.position = "top",
                                title.hjust = 0.5, 
                                label.position = "bottom",
                                nrow = 1)) + 
    scale_color_manual(values = my_colors,
                      labels = c("tidyverse",
                                 "clustering",
                                 "text", 
                                 "regression",
                                 "bayesian",
                                 "trees"))
