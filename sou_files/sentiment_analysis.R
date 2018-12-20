remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/sou_files/")
library(tidyverse);library(tidytext)

files_to_read <- grep("\\.txt", list.files(), value = TRUE)
speech_names <- str_replace(str_to_title(str_replace(files_to_read,pattern = "_", " ")),"\\.Txt","")
x <- lapply(files_to_read, function(i)read_lines(i) %>% as_tibble() %>% filter(value != "") %>%
              mutate(sentence = row_number()))

x <- lapply(x, function(i) i %>% mutate(value = str_replace(value, "\\[Laughter\\]|\\[laughter\\]"," ")))


##
match_to_sentiment <- function(list_frame){
  z <- list_frame %>% unnest_tokens(word, value) %>% inner_join(get_sentiments("bing")) %>%
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
  ggplot(aes(x = sentence, y =total)) + geom_line() + 
  facet_wrap(~pres_year, scales = "free_x", nrow = 3) + 
  geom_smooth(se = FALSE)

