remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)


setwd("/Users/harro.cyranka/Desktop/rviz/")
library(tidyverse);library(lubridate);library(forecast)


seattle_pets <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-26/seattle_pets.csv")


# Clean variables and filter dates after 2017 -----------------------------


seattle_pets <- seattle_pets %>%
  separate(license_issue_date, into = c("month", "day", "year")) %>%
  mutate(month = case_when(
    month == "January" ~ 1,
    month == "February" ~2,
    month == "March" ~3,
    month == "April"~4,
    month == "May"~5,
    month == "June" ~6,
    month == "July" ~7,
    month == "August" ~8,
    month == "September" ~9,
    month == "October" ~10,
    month == "November" ~11,
    month == "December" ~12
  )) %>%
  mutate(date = ymd(paste0(year,"-",month,"-",day)),
         zip_code = case_when(
           nchar(zip_code) == 4 ~ paste0("0", zip_code),
           nchar(zip_code) == 3 ~ paste0("00", zip_code),
           TRUE ~ zip_code
         ),
         zip_code = str_replace_all(zip_code, "-[0-9]{3,5}","")) %>%
  select(date, license_number:zip_code) %>%
  arrange(date) %>%
  filter(date >=ymd("2017-01-01"))



x <- seattle_pets %>%
  filter(!is.na(animals_name),
         species %in% c("Cat", "Dog"))

y <- x %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>% 
  tally() %>% ungroup()

stationary_series <- ts(diff(log(y$n)))
Box.test(stationary_series,lag = log(23))
# ACF and PACF ------------------------------------------------------------
##ACF slowly decays and PACF has one: AR(1)
par(mfrow = c(2,1))
acf(stationary_series,lag.max = 40)
pacf(stationary_series, lag.max = 40)

p <- arima(log(y$n),order = c(2,1,0))
Box.test(residuals(p),lag = log(length(residuals(p)))) ##No residual autocorrelation

forecasts <- forecast::forecast(p)
exp(forecasts$mean) ##These are the actual forecasts

y