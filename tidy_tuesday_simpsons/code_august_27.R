remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harro.cyranka/Desktop/rviz/")
library(tidyverse);library(WikipediR);library(tidytext);library(rvest)


simpsons <- readr::read_delim("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-27/simpsons-guests.csv", delim = "|", quote = "")

guests <- simpsons %>%
  count(guest_star, sort = TRUE)



# Function to retrieve wikipedia ------------------------------------------
retrieve_wiki_page <- function(guest){

    x <- page_content(language = "en","wikipedia", page_name = guest,as_wikitext = FALSE)
    
    text_1 <- read_html(x$parse$text$`*`) %>%
      html_nodes("p") %>%
      html_text() %>%
      rtweet::plain_tweets() %>%
      as_tibble() %>%
      filter(value != "") %>%
      filter(str_detect(value,
                        "\\([A-Z]{1}[a-z]{2,30} [0-9]{2}, [0-9]{4}|\\([A-Z]{1}[a-z]{2,30} [0-9]{2} [0-9]{4}|\\(born [A-Z]{1}[a-z]{2,30} [0-9]{2} [0-9]{4}|[A-Z]{1}[a-z]{2,30} [0-9]{2}, [0-9]{4}\\)|\\(born [A-Z]{1}[a-z]{2,30} [0-9]{2}, [0-9]{4}")) %>%
      mutate(guest = guest)
    
    return(text_1)

}
  
# Lapply list -------------------------------------------------------------
y <- pbapply::pblapply(1:nrow(guests), function(i){
  print(i)
  try(retrieve_wiki_page(guests$guest_star[i]))
})

return_tibble <- bind_rows(y[-which(sapply(y, class) == 'try-error')])
return_tibble <- return_tibble %>% group_by(guest) %>%
  mutate(nrow = row_number()) %>%
  filter(nrow == 1)

# Do some tidy text mining ------------------------------------------------
library(ggwordcloud)

return_tibble %>%
  select(-nrow) %>%
  unnest_tokens(word, value) %>%
  anti_join(stop_words) %>%
  filter(!str_detect(word,"[0-9]|american|born")) %>%
  ungroup() %>% count(word, sort = TRUE) %>%
  slice(1:200) %>%
  ggplot(aes(size = n, label = word, color = log(n))) + 
  ggwordcloud::geom_text_wordcloud_area(family = "IBM Plex Sans",
                                   fontface = "bold", 
                                   shape = "square") + 
  theme_void() + 
  labs(title = "Word cloud generated from the biographies of 458 Simpsons guest stars",
       subtitle = "Information retrieved from Wikipedia") + 
  theme(
    plot.title = element_text(color = "black", family = "IBM Plex Sans", size = 15, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(color = "black", family = "IBM Plex Sans", size = 12, face = "bold", hjust = 0.5),
    plot.background = element_rect(fill = "gray95")
  ) + 
  scale_size_area(max_size = 14) + 
  scale_color_viridis_c(option = "E", end = 0.9, begin = 0.3)

# Continous data ----------------------------------------------------------
library(widyr);library(LaCroixColoR)

my_palette <- lacroix_palette("PeachPear", n = 24, type = "continuous")[4:12]



top_pairs <- return_tibble %>%
  select(-nrow) %>%
  unnest_tokens(word, value) %>%
  anti_join(stop_words) %>%
  filter(!str_detect(word,"[0-9]|american|born")) %>%
  ungroup() %>% mutate(guest_n = as.numeric(factor(guest))) %>%
  widyr::pairwise_cor(word, guest_n, sort = TRUE) %>%
  filter(item1 %in% c("actor", "producer","singer",
                      "comedian","songwriter",
                      "player","director", "actress")) %>%
  arrange(item1, desc(correlation)) %>%
  group_by(item1) %>% slice(1:15)

top_pairs %>%
  mutate(item2 = reorder_within(item2,
                                correlation,
                                item1)) %>%
  ggplot(aes(x = reorder(item2,correlation), y = correlation, fill = item1)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~item1, scales = "free", nrow = 2) +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(expand = c(0,0),limits = c(0, 0.6),
                     breaks = c(0,0.2,0.4,0.6)) + 
  theme_minimal() + 
  theme(
    text = element_text(family = "IBM Plex Sans"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(size = 0.03, color = "gray50"),
    strip.background = element_rect(fill = "gray95", color = "gray95")
  ) + 
  labs(y = "Correlation", x = "Word", 
       title = "Top correlations for selected biographical words", 
       subtitle = "Biographical information extracted from Wikipedia") + 
  scale_fill_manual(values = my_palette)
