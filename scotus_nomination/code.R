remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/francisco06121988/Desktop/rviz/scotus_nomination/")
library(tidyverse);library(waffle);library(readxl)

##
clean_and_retrieve <- function(sheet){
    x <- read_excel("data.xlsx", sheet = sheet) %>%
        filter(!is.na(vote))
    x_tibble <- tibble(party_state = str_extract(x$vote,"[A-Z]{1,2}-[A-Z]{1,2}"),
                       vote = str_extract(x$vote,"Yea|Nay")) %>%
        filter(!is.na(vote)) %>%
        separate(party_state, into = c("party","state"),sep = "-")
    roll_call <- x_tibble %>% group_by(party, vote) %>% tally() %>%
        arrange(party, desc(vote))
    return(roll_call)
}
clean_and_retrieve("sotomayor")
clean_and_retrieve("kagan")
clean_and_retrieve("gorsuch")

##
waffle_1 <- waffle(parts = c(`D-Yea` = 57,`R-Yea` = 9,
                             `R-Nay`  = 31,`I-Yea` = 2),rows =5,
                   colors = c("dodgerblue4", "firebrick4","firebrick1","gainsboro"),
                   legend_pos = "right",size = 0.5,xlab = "1 square = 1 senator") + 
    labs(title = "Roll call vote on the nomination of Sonia Sotomayor",
         subtitle = "Final result: 68-Yea/31-Nay (Nominated by Barack Obama (D))",
         caption = "Source: www.senate.gov") + 
    theme(
        plot.title = element_text(size = 12,face = "bold"),
        plot.subtitle = element_text(size = 9),
        plot.caption = element_text(size = 9)
    )

waffle_2 <- waffle(parts = c(`D-Nay` = 1,`D-Yea` = 56,`R-Yea` = 5,
                             `R-Nay`  = 36,`I-Yea` = 2),rows =5,
                   colors = c("dodgerblue2","dodgerblue4", "firebrick4","firebrick1","gainsboro"),
                   legend_pos = "right",size = 0.5,xlab = "1 square = 1 senator") + 
    labs(title = "Roll call vote on the nomination of Elena Kagan",
         subtitle = "Final result: 63-Yea/37-Nay (Nominated by Barack Obama (D))",
         caption = "Source: www.senate.gov") + 
    theme(
        plot.title = element_text(size = 12,face = "bold"),
        plot.subtitle = element_text(size = 9),
        plot.caption = element_text(size = 9)
    )

waffle_3 <- waffle(parts = c(`D-Nay` = 43,`D-Yea` = 3,`R-Yea` = 51,
                             `I-Nay` = 2),rows =5,
                   colors = c("dodgerblue2","dodgerblue4", "firebrick4","dimgrey"),
                   legend_pos = "right",size = 0.5,xlab = "1 square = 1 senator") + 
    labs(title = "Roll call vote on the nomination of Neil Gorsuch",
         subtitle = "Final result: 54-Yea/45-Nay (Nominated by Donald Trump (R))",
         caption = "Source: www.senate.gov") + 
    theme(
        plot.title = element_text(size = 12,face = "bold"),
        plot.subtitle = element_text(size = 9),
        plot.caption = element_text(size = 9)
    )

waffle_4 <- waffle(parts = c(`D-Nay` = 46,`D-Yea` = 1,`R-Yea` = 49,
                             `I-Nay` = 2),rows =5,
                   colors = c("dodgerblue2","dodgerblue4", "firebrick4","dimgrey"),
                   legend_pos = "right",size = 0.5,xlab = "1 square = 1 senator") + 
    labs(title = "Roll call vote on the nomination of Brett Kavanaugh",
         subtitle = "Final result: 50-Yea/48-Nay (Nominated by Donald Trump (R))",
         caption = "Source: www.senate.gov\nLisa Murkowski (R-AK) voted 'Present'") + 
    theme(
        plot.title = element_text(size = 12,face = "bold"),
        plot.subtitle = element_text(size = 9),
        plot.caption = element_text(size = 9)
    )

iron(waffle_1, waffle_2, waffle_3, waffle_4)