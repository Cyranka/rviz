remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/rviz/tasa_de_paro_eeuu")
library(tidyverse);library(fpp2)

x <- readxl::read_excel("SeriesReport-20200524174916_763ce4.xlsx", sheet = 2) %>%
    janitor::clean_names() %>%
    gather(month, paro, -year) %>%
    drop_na() %>%
    arrange(year) 
 
ts_object <- x %>% slice(1:123) %>%
    pull(paro) %>%ts(start = c(2010,1),
                                    end = c(2020,3),
                                    frequency = 12)

arima_forecast <- auto.arima(ts_object)
forecast_april <- as.numeric(forecast(arima_forecast, h = 1)$mean)
series_and_forecast <- c(fitted(arima_forecast), forecast_april)

##Begin plotting
x %>%
    mutate(arima_fit_and_fcast = series_and_forecast,
           good_date = seq.Date(from = lubridate::ymd("2010-01-01"),
                                to =   lubridate::ymd("2020-04-01"),
                                by = "month")) %>%
    select(good_date, paro, arima_fit_and_fcast) %>%
    gather(series,value,-good_date) %>%
    ggplot(aes(good_date, y = value/100, color = factor(series,
                                                        labels = c("Predicciones de un modelo ARIMA",
                                                                   "Tasa real de paro")))) + 
    geom_line(size= 0.7) + 
    hrbrthemes::theme_modern_rc() + 
    theme(
        legend.position = "bottom",
        plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 12),
        axis.text.x = element_text(size = 8, angle = 90, hjust = 0.5),
        axis.text.y = element_text(size = 8),
        axis.line.x = element_line(color = "white")
     ) + 
    scale_x_date(date_breaks = "1 year",
                 date_labels = "%d/%m/%Y") + 
    scale_y_continuous(labels = scales::percent,limits = c(0,0.15)) + 
    labs(x = "\nFecha",
         y = "Tasa de paro\n (En % de la población activa)\n",
         title = "Evolución de la tasa mensual de paro en EEUU desde 2010",
         subtitle = "Datos del Bureau of Labor Statistics",
         color = "Serie") + 
    guides(color = guide_legend(title.position = "top",
                                nrow = 2)) + 
    scale_color_manual(values = c("red",
                                  "gray90"))

    



