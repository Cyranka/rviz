remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

library(tidyverse);library(rvest)

url <- "https://www.cia.gov/library/publications/the-world-factbook/rankorder/2053rank.html"

x <- url %>% read_html()
k <- x %>% html_table() %>% bind_rows()

k %>% select(-Rank) %>%
  magrittr::set_colnames(c("country", "total_airports", "date_info")) %>%
  mutate(total_airports = parse_number(str_replace(total_airports, ",",""))) %>%
  as_tibble() %>%
  write_csv("total_airports_by_country.csv")