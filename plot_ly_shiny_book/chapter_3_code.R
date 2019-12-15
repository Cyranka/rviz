remove(list =ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/coursera_files/plotly_shiny_book/")
library(plotly);library(tidyverse)

data("economics",package = "ggplot2")

p <- economics %>%
    arrange(psavert) %>%
    plot_ly(x =~date, y=~psavert)

##Example with add_paths
add_paths(p)

##Example with add lines
add_lines(p)

##Using dplyr to change
econ <- economics %>%
    mutate(yr = lubridate::year(date),mnth = lubridate::month(date))

econ %>%
    group_by(yr) %>% ##Grouping the data is similar to the group argument in ggplot2
    plot_ly(x = ~mnth, y = ~uempmed) %>%
    add_lines(text = ~yr)

##Using color to highlight groups
##Ordered determines the palette direction (i guess)
plot_ly(econ,x = ~mnth,y = ~uempmed) %>%
    add_lines(color = ~ordered(yr),
              text = ~yr)

# Figure 3.3 --------------------------------------------------------------
set.seed(99)

plot_ly() %>%
    add_trace(
        type = "scatter",
        mode = "markers+lines+text",
        x = 4:6,
        y = 4:6,
        text = replicate(3, praise::praise("You are ${adjective}!")),
        textposition = "right",
        hoverinfo = "text",
        hoverlabel = list(family = "Roboto Condensed",
                          size = 10,color = I("red"),bgcolor = toRGB("black")),
        opacity = 1,
        textfont = list(family = "Roboto Condensed", size = 16)
    ) %>%
    layout(xaxis = list(range = c(3,8)))

schema() ##Use this function to open plot.ly js information

# 3.1 Markers -------------------------------------------------------------
plotly::subplot( ##Use subplot to put two plots together
    plot_ly(mpg,x = ~cty, y = ~hwy, name = "default"),
    plot_ly(mpg, x = ~cty, y = ~hwy) %>%
        add_markers(alpha = 0.2, name = "alpha")
)


##3.1.2 Mapping numeric color to marker
p <- plot_ly(mpg, x = ~cty,y =~hwy, alpha = 0.5)
plotly::subplot(
    add_markers(p, color = ~cyl, showlegend = FALSE) %>%
        colorbar(title = "Viridis",len = 0.5),##len controls the size of the colorbar
    add_markers(p, color = ~factor(cyl))
)

##Using different ways to change default color palettes passed to gradient palettes
col1 <- c("#132B43","#56B1F7")
col2 <- viridisLite::inferno(10)
col3 <- colorRamp(c("red", "white","blue"))

plotly::subplot(
    p %>%add_markers(color = ~cyl, colors = col1) %>%
        colorbar(title = "ggplot2 default", len = 0.3),
    p %>%add_markers(color = ~cyl, colors = col2) %>%
        colorbar(title = "Inferno", len = 0.3),
    p %>%add_markers(color = ~cyl, colors = col3) %>%
        colorbar(title = "ColorRamp")
) %>% hide_legend()

##Using discrete color mapping
col1 <- "Accent"
col2 <- colorRamp(c("red","blue"))
col3 <- c(`4` = "red",`5` = "black", `6` = "blue",`8` = "green")

plotly::subplot(shareX = TRUE,shareY = TRUE, ##USe share to make them doubly interactive
    p %>%add_markers(color = ~factor(cyl), colors = col1),
    p %>%add_markers(color = ~factor(cyl), colors = col2),
    p %>%add_markers(color = ~factor(cyl), colors = col3)
) %>% hide_legend()


##Stroke controls the outline of a geometry
p %>%add_markers(color = ~I("red"), stroke = I("black"))

#3.1.3 Symbols
p <- plot_ly(mpg, x = ~cty, y = ~hwy,alpha = 0.3)
plotly::subplot(
    add_markers(p, symbol = ~cyl, name = "A single trace"),
    add_markers(p,symbol = ~factor(cyl), color = I("black"))
)

#Mapping symbol vectors to plots: either numbers or descriptions
plotly::subplot(
    add_markers(p, symbol = ~cyl, symbols = c(17,18,19)),
    add_markers(p, symbol = ~factor(cyl),
                symbols = c("triangle-up","diamond","circle"))
)

#3.1.4: Stroke and span: span is the 'width' of the stroke
plot_ly(mpg, x = ~cty, y = ~hwy, alpha = 0.5) %>%
    add_markers(symbol = I(18),stroke = I("black"), span = I(1))

#3.1.5: Sizes: variable to size parameter has to be numeric
p <- plot_ly(mpg, x = ~cty, y = ~hwy,alpha = 0.3)
subplot(
    add_markers(p, size = ~cyl, name = "default"),
    add_markers(p, size = ~cyl, sizes = c(1,500), name = "custom") ##Custom size
)

##Fixing the size using I()
plot_ly(mpg, x = ~cty, y = ~hwy, alpha = 0.3, size = I(100)) %>%
    add_markers(hoverinfo = "text",
                text = ~paste0("This is text"))


#3.16: Dotplots and error bars
#fit linear model
m <- lm(
    Sepal.Length ~ Sepal.Width*Petal.Length*Petal.Width,
    data = iris
)

broom::tidy(m) %>%
    mutate(term = forcats::fct_reorder(term, estimate)) %>%
    plot_ly(x = ~estimate, y = ~term) %>%
    add_markers(
        error_x = ~list(value = std.error),
        color = I("black"),
        hoverinfo = "x"
    )

# 3.2 Lines ---------------------------------------------------------------
top5 <- txhousing %>%
    group_by(city) %>%
    summarize(m = mean(sales, na.rm = TRUE)) %>%
    arrange(desc(m)) %>%
    top_n(5)

tx5 <- txhousing %>% semi_join(top5, by = "city")

tx5 %>% 
    plot_ly(x = ~date, y = ~median) %>%
    add_lines(linetype = ~city) ##Linetype refers to the basic element of drawing

##Create linetype arguments
ltys <- c(
    Austin = "dashdot",
    `Collin County` = "longdash",
    Dallas = "dash",
    Houston = "solid",
    `San Antonio` = "dot"
) ##Notice that it is a vector with names

plot_ly(tx5,
        x = ~date,
        y = ~median) %>%
    add_lines(linetype = ~city,
              linetypes = ltys,
              color = ~city,
              hoverinfo = "text",
              text = ~paste0("City: ", city,"<br>",
                             "Date: ", round(date,0),"<br>",
                             "Median: ", scales::comma(median)),
              hoverlabel = list(size = 10,
                                font = list(family = "Roboto Condensed",
                                            size = 12,
                                            color = "black"),
                                bgcolor = toRGB("white"))) ##Use linetypes arguments

#3.2.2 Segments