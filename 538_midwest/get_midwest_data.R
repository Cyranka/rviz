remove(list = ls())
options(scipen = 999)
options(stringsAsFactors = FALSE)

setwd("/Users/francisco06121988/Desktop/rviz/538_midwest/")
library(tidyverse)

x <- read_csv("https://github.com/fivethirtyeight/data/raw/master/region-survey/MIDWEST.csv") %>%
    janitor::clean_names()

x2 <- x %>% filter(how_much_if_at_all_do_you_personally_identify_as_a_midwesterner %in% c("A lot")|x4 %in% c("Some")) %>%
    slice(-1)


z <- x2 %>%
    select(which_of_the_following_states_do_you_consider_part_of_the_midwest_please_select_all_that_apply:x26)


##A lot or some midwestern
percentage_vector <- 1 - unname(sapply(z, function(i)sum(is.na(i))))/1357
name_vector <- sapply(1:20, function(i)coalesce(z[,i]) %>% rename(my_name = 1) %>%
           drop_na() %>%
           pull(my_name) %>% unique())

tibble(
    state = name_vector,
    percentage_in_midwest = percentage_vector
) %>%
    write_rds("percentage_midwest.rds")



