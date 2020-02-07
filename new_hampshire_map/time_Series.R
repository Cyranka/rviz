remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)


library(tidyverse);library(readxl);library(lubridate);library(fpp2)

x <- read_csv("tnf_ma.csv") %>%
  janitor::clean_names() %>%
  mutate(month = ymd(paste0(month, "-01")))

extract_ts <- x %>% pull(tnf_ma) %>%
  ts(frequency = 12, start = c(2004,1), end = c(2020,2))

basic_ts_model <- tslm(formula = extract_ts ~ trend + season)

x %>% 
  modelr::add_predictions(basic_ts_model,var = "tslm")  %>%
  gather(series, value, -month) %>%
  filter(!series%in%c("avg_temperature","temperature_model")) %>%
  ggplot(aes(x =month, y = value, color = factor(series, labels = c("Search volume","Fitted values")))) + 
  geom_line() + 
  labs(x = "Time", y = "Volume", color = "Series",
       title = "Search volume for The North Face in Massachusetts",
       subtitle = "Data obtained from Google Trends") + 
  hrbrthemes::theme_ipsum_rc(base_size = 10) + 
  theme(
    plot.title = element_text(size = 14),
    plot.subtitle = element_text(size = 10),
    axis.text.x = element_text(size = 8)
  ) + 
  scale_color_manual(
    values = c("red", "steelblue")
  )