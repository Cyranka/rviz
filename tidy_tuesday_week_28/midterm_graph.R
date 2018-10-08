remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/rviz/tidy_tuesday_week_28/")
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
    filter(election == "Midterm") %>% ungroup() %>%
    group_by(year) %>% top_n(2,turnout) %>%
    bind_rows(x %>% filter(icpsr_state_code!=0) %>%
                  filter(election == "Midterm") %>% ungroup() %>%
                  group_by(year) %>% top_n(-2,turnout)) %>%
    arrange(desc(year))

##
no_us %>% filter(election == "Midterm") %>%
    ggplot(aes(x =year, y = avg_turnout)) + geom_line(col = "black",size =1,
                                                      alpha =0.85) + 
    labs(x = "Election year", y = "Turnout in %",
         title = "Distribution of state turnout in midterm elections",
         subtitle = "Solid line connects turnout averages",
         caption = "Tidy tuesday week 29\nTurnout in the US") + 
    scale_y_continuous(limits = c(25,75)) +
    scale_x_continuous(limits = c(1981, 2015),
                       breaks = seq(1982, 2014, by = 4),
                       labels = seq(1982, 2014, by = 4)) + 
    geom_point(aes(x = year, y = turnout),data = x %>% filter(icpsr_state_code!=0) %>%
                   filter(election == "Midterm"),inherit.aes = FALSE,size =3, alpha  =0.35, color = "steelblue") + 
    theme_minimal() +
    theme(
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x =element_blank(),
        panel.grid.major.y = element_blank(),
        axis.line.x.bottom = element_line(color = "black", size =0.5),
        axis.line.x.top = element_line(color = "black", size =0.5),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 11),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 11),
        plot.title = element_text(hjust = 0.5, size = 20,face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 15)
    ) + geom_segment(aes(x = 1982, y = 70, xend = 1993.9,yend=70), 
                     size = 1, col = "navyblue", lineend = "butt") + 
    geom_segment(aes(x = 1993.9, y = 70, xend = 1982,yend=70),
                 size = 1, col = "navyblue", lineend = "butt") +
    geom_segment(aes(x = 1994, y = 70, xend = 2005.9,yend=70), 
                 size = 1, col = "red", lineend = "butt") + 
    geom_segment(aes(x = 1994, y = 70, xend = 2005.9,yend=70),
                 size = 1, col = "red", lineend = "butt")+ 
    geom_segment(aes(x = 2006, y = 70, xend = 2009.9,yend=70), 
                 size = 1, col = "navyblue", lineend = "butt") + 
    geom_segment(aes(x = 2006, y = 70, xend =2009.9 ,yend=70),
                 size = 1, col = "navyblue", lineend = "butt") + 
    geom_segment(aes(x = 2010, y = 70, xend = 2014,yend=70), 
                 size = 1, col = "red", lineend = "butt")+ 
    annotate("text", label = "Democratic majority", x = 1988, y = 71.5, color = "navyblue",
             size = 3, fontface = "bold") + 
    annotate("text", label = "Republican Majority", x = 2000, y = 71.5, color = "red",
             size = 3, fontface = "bold") + 
    annotate("text", label = "Democratic Majority", x = 2008, y = 71.5, color = "navyblue",
             size = 3, fontface = "bold") + 
    annotate("text", label = "Republican Majority", x = 2012, y = 71.5, color = "red",
             size = 3, fontface = "bold") + 
    annotate("text", label = "Average for the period: 44.2%", x = 1981, y = 50.5, color = "black",
             size = 2, angle = 90, fontface = "bold") + 
    ggrepel::geom_text_repel(aes(x = year, y = turnout, label = state),
                             data= for_labels,
                             size = 2.5,
                             nudge_x =0.75,
                             segment.alpha = 0.2,
                             fontface = "bold") + 
    geom_hline(yintercept = 44.2, linetype = 3, size =0.5)