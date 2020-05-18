remove(list = ls())
options(scipen = 999)
options(stringsAsFactors = FALSE)

setwd("/Users/francisco06121988/Desktop/rviz/538_midwest/")
library(tidyverse)

x <- read_csv("https://github.com/fivethirtyeight/data/raw/master/region-survey/SOUTH.csv") %>%
    janitor::clean_names()


x2 <- x %>% filter(how_much_if_at_all_do_you_personally_identify_as_a_southerner %in% c("A lot")|x4 %in% c("Some")) %>%
    slice(-1)


z <- x2 %>%
    select(which_of_the_following_states_do_you_consider_part_of_the_south_please_select_all_that_apply:x31)

##A lot or some southern
percentage_vector <- 1 - unname(sapply(z, function(i)sum(is.na(i))))/1135
name_vector <- sapply(1:25, function(i)coalesce(z[,i]) %>% rename(my_name = 1) %>%
                          drop_na() %>%
                          pull(my_name) %>% unique())


tibble(
    state = name_vector,
    percentage_in_south = percentage_vector
)%>%
    write_rds("percentage_south.rds")





