remove(list =ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/coursera_files/plotly_shiny_book/")
library(plotly);library(tidyverse);library(Hmisc)


# Chapter 2 ---------------------------------------------------------------
##Plotly tries to automatically find the best representation for the data
plot_ly(diamonds, x = ~cut)
plot_ly(diamonds, x = ~cut, y=~clarity)
plot_ly(diamonds, x = ~cut, color = ~clarity, colors = "Accent")

##Customizing with "as is" -> I(")
plot_ly(diamonds, x = ~cut,
        color =I("red"),
        stroke = I("black"), ##outline
        span = I(2)) ##Outline size

##Using pipe operator to chain modifications
diamonds %>%
    plot_ly(x = ~cut) %>%
    layout(title = "Histogram")

##Adding a layer explicitly
diamonds %>%
    plot_ly() %>%
    add_histogram(x = ~cut)

##
diamonds %>%
    plot_ly(x = ~cut) %>% ##global assignment of x
    add_histogram() %>% ##Adding a layer
    group_by(cut) %>%  ##USing dplyr to change underlying data
    summarise(n = n()) %>% ##USing dplyr to change underlying data
    add_text(
        text = ~scales::comma(n), y = ~n,
        textposition = "top middle",
        cliponaxis = FALSE,
        showlegend = FALSE
    )

# Introduction to ggplotly ------------------------------------------------
p <- ggplot(diamonds, aes(x = log(carat), y = log(price))) + 
    geom_hex(bins = 100)
ggplotly(p)


p <- ggplot(diamonds, aes(x = log(price), color = clarity)) + 
    geom_freqpoly()
ggplotly(p)

#Using facet wrap
p <- ggplot(diamonds, aes(x = log(price), color = clarity)) + 
    geom_freqpoly(stat = "density") + ##Disable absolut frequency
    facet_wrap(~cut)
ggplotly(p)

##
p <- ggplot(diamonds,
            aes(x = clarity,
                y = log(price),
                color = clarity)) + 
    ggforce::geom_sina(alpha = 0.1)+ 
    stat_summary(fun.data = "mean_cl_boot", color = "black") + 
    facet_wrap(~cut)
toWebGL(ggplotly(p))

# GGally ------------------------------------------------------------------
library(GGally)

m <- lm(log(price) ~ log(carat) + cut, data = diamonds)
gg <- ggcoef(m)

ggplotly(gg, dynamicTicks = TRUE)

