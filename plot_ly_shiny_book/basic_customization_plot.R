set.seed(4)
mpg %>%
    filter(year == 1999) %>%
    select(manufacturer, model,hwy) %>%
    group_by(manufacturer, model) %>%
    sample_n(1) %>% ungroup() %>%
    mutate(model = factor(model),
           model = fct_reorder(model,hwy, max)) %>%
    arrange(model) %>%
    plot_ly(x = ~hwy, y = ~model) %>%
    add_bars() %>%
    layout(
        xaxis = list(title = "<b>Miles per gallon (highway)<b>",
                     showline = TRUE,
                     linewidth = 2,
                     tickfont = list(family = "Roboto Condensed", size = 9)),
        yaxis = list(title = "<b>Model<b>",
                     showline = TRUE,
                     linewidth = 2,
                     tickfont = list(family = "Roboto Condensed", size = 9))
    )