remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/tidytuesday_week_19/")
library(tidyverse)
x <- read_csv("tidy_tuesday_week_19.csv")
y <- readxl::read_excel("airline_by_continent.xlsx")
z <- readxl::read_excel("per_capita_income.xls") %>%
  select(`Country Name`,`1985`:`2014`) %>%
  filter(`Country Name` %in% y$country)

aggregate_pci <- z %>% gather(year, per_capita_income, -`Country Name`) %>% rename(country = `Country Name`) %>%
  mutate(period = ifelse(year <2000, "1985-99", "2000-14")) %>%
  group_by(country,period) %>% summarise(avg_pci = mean(per_capita_income, na.rm = TRUE)/1000)


pci <- aggregate_pci %>% spread(period, avg_pci)

##
p <- x %>% inner_join(y) %>% 
  filter(type_of_event == "incidents") %>%
  dplyr::select(airline, year_range, n_events, country) %>%
  spread(year_range, n_events) %>%
  left_join(pci) %>%
  magrittr::set_colnames(c("airline","country","total_incidents_00_14", "total_incidents_85_99",
                           "avg_pci_85_99","avg_pci_00_14"))


##
p2 <- p %>% dplyr::select(airline, country, avg_pci_85_99, total_incidents_85_99) %>%
  magrittr::set_colnames(c("airline", "country", "avg_pci", "total_incidents")) %>%
  mutate(period = "1985-99") %>%
  bind_rows(p %>% dplyr::select(airline, country, avg_pci_00_14, total_incidents_00_14) %>%
              magrittr::set_colnames(c("airline", "country", "avg_pci", "total_incidents")) %>%
              mutate(period = "2000-14"))


p2 %>% ggplot(aes(x = avg_pci, y = total_incidents)) + geom_point() + 
  facet_wrap(~period, scale = "free") + theme_bw() + 
  labs(x = "Average per capita income in the period", y = "Total incidents")


##Only center income
x <- x %>% inner_join(y) %>% 
  filter(type_of_event == "incidents") %>%
  dplyr::select(airline, year_range, n_events, country) %>%
  spread(year_range, n_events) %>%
  left_join(pci) %>% select(-`1985-99`) %>%
  magrittr::set_colnames(c("airline","country","incidents_00_14","incidents_85_99","avg_pci_0014"))


##Need to center this, so it makes sense

fit_1 <- glm(incidents_00_14 ~ incidents_85_99 + avg_pci_0014, family = "poisson", data = x)
arm::display(fit_1, detail = TRUE)
plot(x$avg_pci_0014, x$incidents_00_14, pch = 19, cex = 0.7,
     xlab = "Average per capita income between 2000-2014", ylab = "Total incidents 2000-2014")


#No US
x_1 <- x %>% filter(incidents_00_14 <12)
fit_2 <- glm(incidents_00_14 ~ incidents_85_99 + avg_pci_0014, family = "poisson", data = x_1)
arm::display(fit_2, detail = TRUE)
