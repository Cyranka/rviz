remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/rviz/tidy_tuesday_2019_week_17/")
library(tidyverse);library(sentimentr)

x <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-23/tidy_anime.csv")

p <- x %>%
    select(name,animeID, synopsis) %>%
    unique() %>%
    filter(!is.na(synopsis))

k <- lapply(1:nrow(p), function(i)get_sentences(p$synopsis[i]))
k2 <- lapply(1:length(k), function(i)k[[i]][[1]] %>% as_tibble() %>%
                 mutate(anime = p$name[i]))

k3 <- bind_rows(k2) %>%
    filter(!str_detect(value, pattern = "\\[Written|\\(Source")) %>%
    filter(value != "")

# Calculate polarity ------------------------------------------------------
polarity <- sentiment(k3$value, n.before = 3, n.after = 2,
                      neutral.nonverb.like = TRUE,
                      adversative.weight = 0.6)

# Add polarity ------------------------------------------------------------
k3$polarity <- polarity$sentiment



avg_polarity <- k3 %>%
    group_by(anime) %>%
    summarise(polarity = mean(polarity)) %>%
    mutate(polarity = general_rescale(polarity)) %>%
    mutate(sentiment_cat = case_when(
        between(polarity,-1,-0.3) ~ "Negative",
        between(polarity,-0.299999,-0.1) ~ "Leans negative",
        between(polarity,-0.0999999,0.0999999) ~ "Mostly neutral",
        between(polarity,0.1,0.299999) ~ "Leans positive",
        TRUE ~ "Positive"
    ),
    sentiment_cat = factor(sentiment_cat, levels = c("Negative",
                                                     "Leans negative",
                                                     "Mostly neutral",
                                                     "Leans positive",
                                                     "Positive")))

j <- x %>%
    select(name,animeID, type) %>%
    unique()


plot_df <- avg_polarity %>% inner_join(j, by = c("anime" = "name")) %>%
    filter(type != "Unknown")

set.seed(10)
labels <- plot_df %>%
    group_by(type) %>%
    top_n(1,polarity)%>%
    bind_rows(
        plot_df %>%
            group_by(type) %>%
            top_n(-1,polarity) 
    ) %>%
    bind_rows(plot_df %>% sample_n(6))
    
plot_df %>%
    ggplot(aes(x = polarity,
               y = fct_reorder(type, polarity,mean),
               color = sentiment_cat)) + 
    geom_jitter(alpha = 0.2, size = 2, height = 0.01) + 
    geom_point(size = 3, data = labels, shape = 1, show.legend = FALSE) + 
    scale_color_manual(values = c("red4","red",
                                  "gray",
                                  "royalblue1", "royalblue4")) + 
    hrbrthemes::theme_modern_rc() + 
    ggrepel::geom_text_repel(aes(label =anime),data = labels,
                             size = 2.5,
                             segment.size = 0.2,
                             nudge_x = 0.1,
                             nudge_y = 0.2,show.legend = FALSE,
                             color = "white") + 
    theme(
        legend.position = "bottom"
    ) + 
    guides(color = guide_legend(title = "Classification",
                                title.position = "top",
                                title.hjust = 0.5,
                                override.aes = list(alpha =1))) + 
    labs(x = "Synopsis polarity",
         y = "Type",
         title = "Feeling sad? You might want to try some music anime",
         subtitle = "Music animes have the highest average polarity",
         caption = "Tidy tuesday 2019, week 17: Animes\nMost points selected randomly")

