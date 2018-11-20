remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/views_on_homosexuality/")
library(tidyverse)

x <- readxl::read_excel("views_gay.xlsx")

x <- x %>% gather(year,proportion, -X__1) %>%
  mutate(proportion = ifelse(proportion <1,round(proportion*100,1), proportion)) %>%
  rename(answer = X__1) 


write_csv(x, "views_on_homosexuality_gss.csv")