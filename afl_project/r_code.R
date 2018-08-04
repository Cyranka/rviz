remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("")
library(tidyverse);library(readxl);library(lubridate)


##House
house <- read_excel("raw_data_afl.xlsx") %>%
  magrittr::set_colnames(c("state","district","name", "party","score_2015","score_2016","score_2017","lifetime"))


house <- house %>%
  mutate(division = 3 - rowSums(is.na(house),dims = 1)) %>%
  mutate(year_sum = rowSums(house %>% dplyr::select(score_2015:score_2017),na.rm = TRUE)) %>%
  mutate(avg_score =year_sum/division)


house %>% mutate(Party = ifelse(party == "D", "Democrat", "Republican")) %>%
  ggplot(aes(x = avg_score, y = ..density.., fill = Party)) + geom_histogram(bins = 20,alpha = 0.5, color = 'black') + theme_bw() +
  scale_fill_manual(values = c("steelblue", "red")) + 
  labs(x = "AFL-CIO Lifetime Score", y = "Density", title = "Histogram of AFL-CIO Lifetime Scores by Party")
  


##
by_state <- house %>% group_by(state) %>% summarise(mean_score = mean(lifetime))

##Plotly Map
library(plotly)


#Define Colors
library(RColorBrewer)

colfunc <- colorRampPalette(c("red","steelblue"))
myColors <- colfunc(50)

# 
l <- list(color = toRGB("white"), width = 2)

# 
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

p <- plot_geo(by_state, locationmode = 'USA-states') %>%
  add_trace(
    z = ~mean_score, text = ~state, locations = ~state,
    color = ~mean_score, colors = myColors
  ) %>%
  colorbar(title = "Average AFL-CIO Lifetime Score") %>%
  layout(
    title = '',
    geo = g
  )%>%
  layout(autosize = F, width = 2290, height = 1116)

p

###
house %>% ggplot(aes(x = score_2016, y = score_2017, color = party)) + geom_point() +  
  geom_smooth(method = "lm", se = FALSE) + theme_bw() + 
  scale_color_manual(values = c("steelblue", "red"))



##
no_nas <- house %>% filter(!is.na(score_2016))

fit.1 <- lm(score_2017 ~ score_2016 +party + score_2016:party, data = no_nas)
arm::display(fit.1, detail = TRUE)

fit.2 <- lm(score_2017 ~ score_2016 + score_2016:party, data = house)
arm::display(fit.2, detail = TRUE)

colors <-ifelse(no_nas$party == "R", "red", "blue")

plot(no_nas$score_2016, no_nas$score_2017, col = colors, pch = 19, cex = 0.7,
     xlab = "2016 Scores", ylab = "2017 Scores", main = "Regression of 2017 Scores on 2016 Scores",
     sub ="\n Model: score_2017 = 1.54 + 0.94*score_2016 + -0.36*score_2016*party_gop")

curve(cbind(1,x,x*1) %*% coef(fit.2), add = TRUE,
      col = 'red', lwd = 1, lty = 2) ##Curve for GOP (Fit 2)

curve(cbind(1,x,x*0) %*% coef(fit.2), add = TRUE,
      col = 'blue', lwd = 1, lty = 2) ##Curve for Dem (Fit 2)

##Get most liberal republicans and most conservative democrats
lib_gop <- house %>% filter(party == "R") %>% group_by(party) %>%
  top_n(10, wt = lifetime) %>%
  mutate(group = "Highest Scoring Republicans")
  
con_dem <- house %>% filter(party == "D") %>% group_by(party) %>%
  top_n(-10, wt = lifetime) %>%
  mutate(group = "Lowest Scoring Democrats")

z <- bind_rows(lib_gop, con_dem) %>% mutate(name_state = paste(name, state, sep = "-"))

z %>% ggplot(aes(x = reorder(name_state,lifetime), y = lifetime, fill = party,
                 label = lifetime)) + 
  geom_col(show.legend = FALSE) + facet_wrap(~group, scale = "free_y") + 
  coord_flip() + theme_minimal() + 
  scale_fill_manual(values = c("blue", "red")) + geom_label(fill = "white", size = 3) + 
  labs(y = "Lifetime Scores", x = "Representative",
       title = "Most Liberal Republicans and Most Conservative Democrats\nAccording to the AFL-CIO Legislative Scorecard (Lifetime scores)")


 
by_state %>% top_n(10, wt = mean_score) %>%
  mutate(group = "Highest Scoring States") %>%
  bind_rows(by_state %>%top_n(-10,wt=mean_score) %>%mutate(group = "Lowest Scoring States")) %>%
  arrange(desc(mean_score)) %>% mutate(mean_score = round(mean_score,1)) %>%
  ggplot(aes(x = reorder(state,mean_score), y = mean_score, fill = group,
                                           label = mean_score)) + 
  geom_col(show.legend = FALSE) + 
  coord_flip() + theme_minimal() + 
  scale_fill_manual(values = c("blue", "red")) + geom_label(fill = "white", size = 3) + 
  labs(y = "Average Lifetime Score", x = "State",
       title = "Most Liberal and Most Conservative Congressional Delegations\nAccording to the AFL-CIO Legislative Scorecard (Average Lifetime scores)")

  


