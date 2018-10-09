remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

#setwd("/Users/francisco06121988/Desktop/rviz/tidy_tuesday_week_28/")
library(tidyverse)
extrafont::loadfonts()

x <- read_csv("voter_turnout.csv") %>%
    mutate(turnout = round(votes/eligible_voters*100,2)) %>%
    mutate(election = ifelse(year %in%seq(1980, 2012, 4),"Presidential","Midterm")) %>%
    select(year,election,icpsr_state_code:turnout)


no_us <- x %>% filter(icpsr_state_code!=0) %>%
    group_by(election, year) %>%
    summarise(avg_turnout = mean(turnout, na.rm = TRUE))


for_labels <- x %>% filter(icpsr_state_code!=0) %>%
    filter(election == "Presidential") %>% ungroup() %>%
    group_by(year) %>% top_n(2,turnout) %>%
    bind_rows(x %>% filter(icpsr_state_code!=0) %>%
                  filter(election == "Presidential") %>% ungroup() %>%
                  group_by(year) %>% top_n(-2,turnout)) %>%
    arrange(desc(year))

no_us %>% filter(election == "Presidential") %>%
    ggplot(aes(x =year, y = avg_turnout)) + geom_line(col = "black",size =1,
                                                      alpha =0.85) + 
    labs(x = "Election year", y = "Turnout in %",
         title = "Distribution of state turnout in presidential elections",
         subtitle = "Solid line connects turnout averages",
         caption = "Tidy tuesday week 29\nTurnout in the US") + 
    scale_y_continuous(limits = c(30,85)) +
    scale_x_continuous(limits = c(1979, 2013),
                       breaks = seq(1980, 2012, by = 4),
                       labels = seq(1980, 2012, by = 4)) + 
    geom_point(aes(x = year, y = turnout),data = x %>% filter(icpsr_state_code!=0) %>%
                   filter(election == "Presidential"),inherit.aes = FALSE,size =3, alpha  =0.35, color = "steelblue") + 
    theme_minimal() +
    theme(
        text = element_text(family = "Roboto"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x =element_blank(),
        panel.grid.major.y = element_blank(),
        axis.line.x = element_line(color = "black", size =0.5),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 11),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 11),
        plot.title = element_text(hjust = 0.5, size = 20,face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 15)
    ) + geom_segment(aes(x = 1980, y = 80, xend = 1991.9,yend=80), 
                     size = 1, col = "red", lineend = "butt") + 
    geom_segment(aes(x = 1991.9, y = 80, xend = 1980,yend=80),
                 size = 1, col = "red", lineend = "butt") +
    geom_segment(aes(x = 1992.1, y = 80, xend = 1999.9,yend=80), 
                 size = 1, col = "navyblue", lineend = "butt") + 
    geom_segment(aes(x = 1999.9, y = 80, xend = 1992.1,yend=80),
                 size = 1, col = "navyblue", lineend = "butt") + 
    geom_segment(aes(x = 2000.1, y = 80, xend = 2007.9,yend=80), 
                 size = 1, col = "red", lineend = "butt") + 
    geom_segment(aes(x = 2007.9, y = 80, xend =2000.1 ,yend=80),
                 size = 1, col = "red", lineend = "butt") + 
    geom_segment(aes(x = 2008.1, y = 80, xend = 2012,yend=80), 
                 size = 1, col = "navyblue", lineend = "butt") + 
    geom_segment(aes(x = 2011.9, y = 80, xend =2008.1 ,yend=80),
                 size = 1, col = "navyblue", lineend = "butt") + 
    annotate("text", label = "Ronald Reagan/George H.W Bush", x = 1986, y = 81.5, color = "red",
             size = 3, fontface = "bold") + 
    annotate("text", label = "Bill Clinton", x = 1996, y = 81.5, color = "navyblue",
             size = 3, fontface = "bold") + 
    annotate("text", label = "George W. Bush", x = 2004, y = 81.5, color = "red",
             size = 3, fontface = "bold") + 
    annotate("text", label = "Barack Obama", x = 2010, y = 81.5, color = "navyblue",
             size = 3, fontface = "bold") + 
    annotate("text", label = "Average for the period: 59.6%", x = 1979, y = 67.5, color = "black",
             size = 2, angle = 90, fontface = "bold") + 
    ggrepel::geom_text_repel(aes(x = year, y = turnout, label = state),
              data= for_labels,
              size = 2.5,
              nudge_x =0.75,
              segment.alpha = 0.2,
              fontface = "bold") + 
    geom_hline(yintercept = 59.6, linetype = 3, size =0.5)