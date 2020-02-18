remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/rviz/logistic_curve/")
library(tidyverse);library(readxl);library(nls2)

x <- read_excel("s007.xls", sheet = 3) %>%
    gather(state, population, -year) %>%
    mutate(state = str_to_title(str_replace_all(state,"_"," ")))

x %>%
    ggplot(aes(x = year, y = population, color = state)) + 
    geom_line() + 
    labs(x = "\nYear",
         y = "Total population\n",
         color = "State",
         title = "Population of states in the tri-state area since 1790",
         subtitle = "Data from the english Wikipedia") + 
    scale_y_continuous(labels = scales::comma) + 
    hrbrthemes::theme_ipsum_rc(base_size = 12) + 
    theme(
        axis.text.x  = element_text(size = 8),
        axis.text.y  = element_text(size = 8),
        axis.title = element_text(size = 10, face = "bold")
    )


# Group data and set function ---------------------------------------------
by_state <- x %>%
    group_by(state) %>%
    nest()

fit_logistic <- function(df){
    nls(population~SSlogis(year,Asym,xmid,scal), data = df)
}

##add models
by_state <- by_state %>%
    mutate(
        model =map(data,fit_logistic)
    )

##Add predictions
by_state <- by_state %>%
    mutate(
        predictions = map2(data, model, modelr::add_predictions)
    )

predictions <- unnest(by_state, predictions)
predictions %>%
    ggplot(aes(x = year, y = population, group = state)) + 
    geom_line(aes(x = year, y = pred, group = state),
              size = 2, color = "lawngreen") + 
    geom_line() + 
    facet_wrap(~state,nrow = 1) + 
    labs(x = "Year", y = "Population",
         title = "Population of states in the tri-state area since 1790",
         subtitle = "Green line represents the fitted values of a logistic growth population model") + 
    hrbrthemes::theme_ipsum_rc() + 
    theme(
        strip.text = element_text(size = 9),
        axis.text.x  = element_text(size = 8),
        axis.text.y  = element_text(size = 8),
        axis.title = element_text(size = 10, face = "bold"),
        legend.position = "bottom"
    ) + 
    scale_y_continuous(labels = scales::comma) + 
    guides(color = guide_legend(title = "State",
                                title.position = "top",nrow = 1,
                                title.hjust = 0.5,label.position = "bottom"))
