remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/rviz/tidy_tuesday_2019_week_14/")
library(tidyverse);library(lubridate)

bike_traffic <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-02/bike_traffic.csv")

# Basic cleaning ----------------------------------------------------------
x <- bike_traffic %>%
    select(date,bike_count) %>%
    mutate(date = str_extract(date,"[0-9]{2}/[0-9]{2}/[0-9]{4}"),
           date = mdy(date),
           month = floor_date(date, unit = "month")) %>%
    select(date, month, bike_count)

# Monthly totals ----------------------------------------------------------
monthly <- x %>% 
    group_by(month) %>%
    summarise(total_bike = sum(bike_count,na.rm = TRUE)) %>%
    mutate(log_series = log(total_bike)) %>%
    slice(2:61)

# Create a time-series object ---------------------------------------------
time_series_log <- ts(monthly$log_series,
                      start = c(2014,1),
                      end = c(2018,12),
                      frequency = 12)
plot(time_series_log)

# Build a model with auto.arima -------------------------------------------
library(forecast)
model_1 <- auto.arima(time_series_log,d = 1,D = 1)

fitted_values <- exp(model_1$fitted) %>%
    broom::tidy()

point_predictions <- predict(model_1,n.ahead = 24)$pred
se_predictions <- predict(model_1,n.ahead = 24)$se

# Everything together -----------------------------------------------------
k <- point_predictions %>% broom::tidy()%>%
    mutate(date = seq.Date(ymd("2019-01-01"),
                           to = ymd("2020-12-01"),
                           by = "month")) %>%
    rename(point_predictions = x) %>%
    bind_cols(se_predictions %>% broom::tidy()) %>%
    rename(se = x)

# See fitted and actual together ------------------------------------------
monthly %>%
    mutate(fitted_arima = exp(model_1$fitted)) %>%
    select(month, total_bike, fitted_arima) %>%
    ggplot(aes(x = month, y = total_bike)) + 
    geom_point(color = "gray90",
               size = 0.5) + 
    geom_line(color = "gray90",
              size =0.5) + 
    geom_line(aes(x = month, y = fitted_arima),
              col = "cyan",
              alpha = 0.5,
              size = 2) + 
    geom_line(data = k,
              aes(x = date, y = exp(point_predictions)),
              size = 2,
              alpha = 0.5,
              col = "red") + 
    hrbrthemes::theme_modern_rc(axis_title_size = 14,
                                plot_title_size = 20) +
    theme(
        plot.subtitle = element_text(size = 14, color = "gray70"),
        plot.title = element_text(face = "bold"),
        axis.line.x = element_line(size =0.5, color = "gray90")
    ) + 
    labs(y = "Total bikes\n",
         x = "Month",
         title = "ARIMA forecasts for total bikes reported by bike counters in Seattle",
         subtitle = "Red line represents forecasts until December 2020",
         caption = "Tidy tuesday 2019, week 14: Seattle bike counters\nFitted model is ARIMA (1,1,0)(1,1,0)[12]") + 
    scale_y_continuous(limits = c(20000,200000),
                       breaks = seq(25000,200000, by = 25000),
                       labels = scales::comma) + 
    scale_x_date(date_labels = "%b %Y",
                 date_breaks = "1 year") + 
    annotate(geom = "text",
             x = ymd("2014-12-01"),
             y = 30000,
             label = "Cyan line is the fitted model",
             color = "cyan",
             fontface = "bold",
             family = "Roboto Condensed",
             size = 3) + 
    annotate(geom = "text",
             x = ymd("2014-12-01"),
             y = 22500,
             label = "Gray line is the actual series",
             color = "gray90",
             fontface = "bold",
             family = "Roboto Condensed",
             size = 3)
    
    
    
    

