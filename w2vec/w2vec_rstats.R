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