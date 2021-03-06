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
  labs(x = "Average per capita income in the period", y = "Total incidents") + 
  geom_smooth(se = FALSE)


##Only center income
x <- x %>% inner_join(y) %>% 
  filter(type_of_event == "incidents") %>%
  dplyr::select(airline, year_range, n_events, country) %>%
  spread(year_range, n_events) %>%
  left_join(pci) %>% select(-`1985-99`) %>%
  magrittr::set_colnames(c("airline","country","incidents_00_14","incidents_85_99","avg_pci_0014")) %>%
  mutate(centered_income = avg_pci_0014 - mean(avg_pci_0014))


##Need to center this, so it makes sense
fit_1 <- glm(incidents_00_14 ~ incidents_85_99 + centered_income, family = "poisson", data = x)
arm::display(fit_1, detail = TRUE)
plot(x$incidents_85_99, x$incidents_00_14, pch = 19, cex = 0.7,
     xlab = "Total incidents 1985-1999", ylab = "Total incidents 2000-2014")
curve(exp(cbind(1,x,0) %*% coef(fit_1)), add = TRUE,
      col = 'red', lwd = 2, lty = 2)


predictions_at_mean_income <- exp(cbind(1,x$incidents_85_99,0) %*% coef(fit_1))[,1]
predictions_at_1st <- exp(cbind(1,x$incidents_85_99,-18.9993) %*% coef(fit_1))[,1]
predictions_at_3rd <- exp(cbind(1,x$incidents_85_99,17.7214) %*% coef(fit_1))[,1]




##
x %>% mutate(predictions_at_mean_income = predictions_at_mean_income,
             predictions_at_1st = predictions_at_1st) %>%
  ggplot(aes(x = incidents_85_99, y= incidents_00_14)) + 
  geom_point() + theme_bw() + geom_jitter() + 
  geom_smooth(data = x, aes(x = incidents_85_99, y = predictions_at_mean_income)) + 
  geom_smooth(data = x, aes(x = incidents_85_99, y = predictions_at_1st), col = "red") + 
  geom_smooth(data = x, aes(x = incidents_85_99, y = predictions_at_3rd), col = "forestgreen") + 
  labs(x = "Incidents 1985-99",y = "Incidents 2000-14",
       title = "Poisson regression curves at three different income levels",
       subtitle = "Model: (Total incidents during 2000-14) = (Total incidents during 1985-99) + (Mean Per Capita Income between 2000-14 (Centered))",
       caption = "Blue line: poisson regression curve when mean per capita income ≈ 23.6 (Average)\nGreen line: poisson regression curve when mean per capita income ≈ 41.3 (3rd Quartile)\nRed line: poisson regression curve when mean per capita income ≈ 4.5 (1st Quartile)")

##
predictions_no_accidents <- exp(cbind(1,0,x$centered_income) %*% coef(fit_1))[,1]
predictions_4_accidents <- exp(cbind(1,4,x$centered_income) %*% coef(fit_1))[,1]
predictions_8_accidents <- exp(cbind(1,8,x$centered_income) %*% coef(fit_1))[,1]

x %>%
  ggplot(aes(x = centered_income, y= incidents_00_14)) + 
  geom_point() + theme_bw() + 
  geom_smooth(data = x, aes(x = centered_income, y = predictions_no_accidents), col = "blue") + 
  geom_smooth(data = x, aes(x = centered_income, y = predictions_4_accidents), col = "red") + 
  geom_smooth(data = x, aes(x = centered_income, y = predictions_8_accidents), col = "forestgreen")+ 
  labs(x = "Centered Mean Per Capita Income in 2000-14",y = "Incidents 2000-14",
       title = "Poisson regression curves at three different # of total incidents in 1985-99",
       subtitle = "Model: (Total incidents during 2000-14) = (Total incidents during 1985-99) + (Mean Per Capita Income between 2000-14 (Centered))",
       caption = "Blue line: poisson regression curve when # accidents = 0\nGreen line: poisson regression curve when # of accidents = 8 (3rd Quartile)\nRed line: poisson regression curve when # of accidents = 4 (Median)")

###Removing Aeroflot and refitting the model
fit_2 <- glm(incidents_00_14 ~ incidents_85_99 + centered_income, family = "poisson", data = x %>% filter(airline!="Aeroflot*"))
arm::display(fit_2, detail = TRUE)


predictions_at_mean_income <- exp(cbind(1,x$incidents_85_99[-2],0) %*% coef(fit_2))[,1]
predictions_at_1st <- exp(cbind(1,x$incidents_85_99[-2],-18.9993) %*% coef(fit_2))[,1]
predictions_at_3rd <- exp(cbind(1,x$incidents_85_99[-2],17.7214) %*% coef(fit_2))[,1]


##
x %>%  filter(airline!="Aeroflot*") %>%
  ggplot(aes(x = incidents_85_99, y= incidents_00_14)) + 
  geom_point() + theme_bw() + geom_jitter() + 
  geom_smooth(data = x %>%  filter(airline!="Aeroflot*"), aes(x = incidents_85_99, y = predictions_at_mean_income)) + 
  geom_smooth(data = x %>%  filter(airline!="Aeroflot*"), aes(x = incidents_85_99, y = predictions_at_1st), col = "red") + 
  geom_smooth(data = x %>%  filter(airline!="Aeroflot*"), aes(x = incidents_85_99, y = predictions_at_3rd), col = "forestgreen") + 
  labs(x = "Incidents 1985-99",y = "Incidents 2000-14",
       title = "Poisson regression curves at three different income levels (Aeroflot Removed)",
       subtitle = "Model: (Total incidents during 2000-14) = (Total incidents during 1985-99) + (Mean Per Capita Income between 2000-14 (Centered))",
       caption = "Blue line: poisson regression curve when mean per capita income ≈ 23.6 (Average)\nGreen line: poisson regression curve when mean per capita income ≈ 41.3 (3rd Quartile)\nRed line: poisson regression curve when mean per capita income ≈ 4.5 (1st Quartile)")


summary(x$incidents_85_99[-2])

###Removing Aeroflot and refitting the model: Income side
predictions_no_accidents <- exp(cbind(1,0,x$centered_income[-2]) %*% coef(fit_2))[,1]
predictions_4_accidents <- exp(cbind(1,4,x$centered_income[-2]) %*% coef(fit_2))[,1]
predictions_8_accidents <- exp(cbind(1,8,x$centered_income[-2]) %*% coef(fit_2))[,1]

x %>%filter(airline!="Aeroflot*") %>%
  ggplot(aes(x = centered_income, y= incidents_00_14)) + 
  geom_point() + theme_bw() + 
  geom_smooth(data = x %>%filter(airline!="Aeroflot*"), aes(x = centered_income, y = predictions_no_accidents), col = "blue") + 
  geom_smooth(data = x %>%filter(airline!="Aeroflot*"), aes(x = centered_income, y = predictions_4_accidents), col = "red") + 
  geom_smooth(data = x %>%filter(airline!="Aeroflot*"), aes(x = centered_income, y = predictions_8_accidents), col = "forestgreen")+ 
  labs(x = "Centered Mean Per Capita Income in 2000-14",y = "Incidents 2000-14",
       title = "Poisson regression curves at three different # of total incidents in 1985-99 (Aeroflot Removed)",
       subtitle = "Model: (Total incidents during 2000-14) = (Total incidents during 1985-99) + (Mean Per Capita Income between 2000-14 (Centered))",
       caption = "Blue line: poisson regression curve when # accidents = 0\nGreen line: poisson regression curve when # of accidents = 8 (3rd Quartile)\nRed line: poisson regression curve when # of accidents = 4 (Median)")
