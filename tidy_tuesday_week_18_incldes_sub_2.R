remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd()
library(tidyverse);library(readxl);library(lubridate)


x <- read_excel("week18_dallas_animals.xlsx", sheet =  "simple")
outcomes_total <- x %>% count(outcome_type, sort = TRUE) %>% mutate(outcome_type = str_to_title(outcome_type)) %>%
  mutate(proportion_of_total = n/sum(n)*100)

outcomes_by_month <- x %>% mutate(floor_date = floor_date(intake_date, "month")) %>% group_by(outcome_type,floor_date) %>%
  tally() %>% arrange(outcome_type, floor_date) %>% ungroup() %>%
  mutate(outcome_type = str_to_title(outcome_type)) %>% filter(!outcome_type %in% c("Other", "Missing"))


outcomes_by_month %>% ggplot(aes(x = floor_date, y = n, fill = outcome_type)) + geom_col(show.legend = FALSE) +
  geom_smooth(se = FALSE, show.legend = FALSE) + 
  facet_wrap(~outcome_type, scales = "free") + theme_bw() + 
  labs(x = "Date", y= "Total Cases", title = "@Dallas Shelter Animal Data: Evolution of Outcomes Throughout FY 2017")

outcomes_total %>% mutate(proportion_of_total = round(proportion_of_total,1)) %>%
  ggplot(aes(x = reorder(outcome_type,n), y = n, label = n)) + geom_col(fill = "steelblue")+ theme_bw() + coord_flip() + 
  geom_label() + labs(x = "Outcome Type", y = "Total", title = "@Dallas Shelter Animal Data: Total Outcome Types in FY 2017")

##
outcome_intake <- x %>% group_by(intake_type, outcome_type) %>% tally() %>%
  ungroup() %>%
  mutate(intake_type = str_to_title(intake_type),
         outcome_type =str_to_title(outcome_type)) %>%
  group_by(intake_type) %>% mutate(freq = n/sum(n),
                                   total = sum(n)) %>%
  select(intake_type, outcome_type, n, total, freq)

##
stray_surrender <- outcome_intake %>% filter(intake_type %in% c("Stray", "Owner Surrender"))

library(scales)
stray_surrender %>% select(-n,-total) %>%spread(intake_type, freq,fill = 0) %>%ggplot(aes(`Owner Surrender`, Stray)) +
  geom_jitter(size = 2.5, width = 0, height = 0) +
  geom_text(aes(label = outcome_type), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red", lty= 2) + theme_bw() +
  labs(title = "Comparison of Outcomes: Stray Events x Owner Surrender (Two most frequent intake categories)")


stray_surrender %>% select(-n,-total) %>%spread(intake_type, freq,fill = 0) %>%
  mutate(logratio = log(`Stray`/`Owner Surrender`)) %>%
  arrange(desc(logratio)) %>% mutate(outcome_type = reorder(outcome_type, logratio)) %>%
  ggplot(aes(outcome_type, logratio, fill = logratio >0)) + geom_col(show.legend = FALSE) + coord_flip() + theme_bw() + 
  labs(y = "Log Odds Ratio (Stray/Owner Surrender)", x= "Outcome Type",
       title = "Outcomes Log Odds Ratio: Stray Events x Owner Surrender (Two most frequent intake categories)")
  
  
##
dev.off()

##Look at Euthanized
euthanized <- x %>% mutate(euthanized = ifelse(outcome_type == "EUTHANIZED",1,0)) %>%
  mutate(intake_type = str_to_title(intake_type)) %>% filter(intake_type %in% c("Confiscated", "Owner Surrender", "Stray"))
levels <- c( "Confiscated", "Owner Surrender", "Stray")
euthanized <- euthanized %>% mutate(intake_type = factor(intake_type, levels = levels))

fit.1 <- glm(euthanized ~ intake_type, family = binomial(link = "logit"), data = euthanized)
arm::display(fit.1, detail = TRUE, digits = 4)

arm::invlogit(-1.3658  + 0.1857)

##

x <- x %>% mutate(season = ifelse(month %in% c("SEP", "OCT", "NOV"), "Fall",
                                  ifelse(month %in% c("DEC","JAN", "FEB"),"Winter",
                                         ifelse(month %in% c("MAR", "APR","MAY"),"Spring","Summer"))))

total_wildlife <- x %>% filter(animal_type == "WILDLIFE") %>% group_by(animal_breed) %>% tally(sort = TRUE) %>%
  filter(n > 25)
x %>% filter(animal_type == "WILDLIFE") %>% mutate(animal_breed= str_to_title(animal_breed)) %>%
  group_by(animal_breed, season) %>% tally() %>%
  group_by(animal_breed) %>% mutate(percent = n/sum(n)) %>%
  filter(animal_breed %in% str_to_title(total_wildlife$animal_breed)) %>%
  ggplot(aes(x = reorder(animal_breed,n), y= percent, fill = season)) + geom_col(color = "black") + coord_flip() + theme_minimal() + 
  labs(fill = "Season", x = "Animal", y = "Percent",
       title = "Wildlife: % of intakes by season", caption = "Only species captured at least 25 times in FY 2017") + 
  scale_fill_manual(values = c("red", "forestgreen", "cyan4","darkorchid4"))


##
x %>% filter(intake_type == "CONFISCATED") %>% 
  group_by(animal_type, animal_breed) %>% tally(sort = TRUE) %>%
  arrange(animal_type, desc(n)) %>% top_n(5, wt = n) %>% mutate(animal_breed = str_to_title(animal_breed)) %>%
  ggplot(aes(x = reorder(animal_breed,n), y = n, fill = animal_type)) + geom_col(show.legend = FALSE) + facet_wrap(~animal_type, scales = "free") + 
  coord_flip() + theme_bw() + labs(x = "Breed", y = "Total Confiscated", fill = "Animal Type", title = "Most confiscated breeds by animal type",
                                   subtitle = "Pit Bulls are by far the most confiscated animal")