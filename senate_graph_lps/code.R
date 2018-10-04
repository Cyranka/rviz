remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harrocyranka/Desktop/rviz/senate_graph_lps/")
library(readxl);library(tidyverse);library(waffle)

x <- read_excel("gdp_per_state_pop.xlsx", sheet = 1) %>%
  inner_join(read_excel("gdp_per_state_pop.xlsx", sheet = 2)) %>%
  mutate(prop_total_pop = prop_total_pop*100)


y <- read_csv("list_of_senators.csv") %>%
  magrittr::set_colnames(c("senator","state","date_assumed","party")) %>%
  mutate(state = str_replace(state, "U\\.S\\. Senate ",""),
         party = str_replace(party, " Party","")) %>%
  select(-date_assumed)


y <- y %>% group_by(state, party) %>% tally() %>%
  mutate(control = case_when(
    n == 2 & party == "Republican" ~ "Republican",
    n == 2 & party == "Democratic" ~ "Democrat",
    TRUE ~ "Split"
  ))

x <- x %>% inner_join(y %>% select(state, control) %>% unique())

##Waffle 1
for_waffle <- x %>% group_by(control) %>% summarise(total_gdp = sum(prop_total)) %>%
  mutate(of_total = total_gdp/100*306)

waffle_1 <- waffle(parts =c(`Democrat` = 150, `Republican` = 85.3, `Split` = 68.9), rows = 8,
       colors = c("steelblue", "firebrick", "gray"), legend_pos = "right",
       size = 0.5) + 
  labs(title = "Proportion of the American economy represented by each major political party",
       subtitle = "Democrats represent states responsible for 48.9% of the GDP, while Republicans represent states responsible for 27.9% of the GDP",
       caption = "Split refers to states that have one senator from each party or one Democrat and one Independent\nSource: Bureau of Economic Analysis (2017)") + 
  theme(
    plot.title = element_text(size = 12,face = "bold"),
    plot.subtitle = element_text(size = 9),
    plot.caption = element_text(size = 9)
    )

##Waffle 2
for_waffle2 <- x %>% group_by(control) %>% summarise(total_gdp = sum(prop_total_pop)) %>%
  mutate(of_total = total_gdp/100*304.2)

waffle_2 <- waffle(parts =c(`Democrat` = 131.2, `Republican` = 95.3, `Split` = 77.7), rows = 8,
                   colors = c("steelblue", "firebrick", "gray"), legend_pos = "right",
                   size = 0.5) + 
  labs(title = "Proportion of the American population represented by each major political party",
       subtitle = "Democrats represent states responsible for 43.0% of the population, while Republicans represent states responsible for 31.3% of the population",
       caption = "Split refers to states that have one senator from each party or one Democrat and one Independent\nSource: Census Bureau (2017)") + 
  theme(
    plot.title = element_text(size = 12,face = "bold"),
    plot.subtitle = element_text(size = 9),
    plot.caption = element_text(size = 9)
  )

waffle_3 <- waffle(parts =c(`Democrat` = 143, `Republican` = 155.1, `Independent` = 6.1), rows = 8,
                   colors = c("steelblue", "firebrick", "gray60"), legend_pos = "right",
                   size = 0.5) + 
  labs(title = "Proportion of the Senate represented by each major political party",
       subtitle = "Current split: Republicans 51%, Democrats 47%, Independents 2%",
       caption = "Each tile represents approximately 0.35% of the total\nAdjustments were made to ensure graphs of roughly the same size") + 
  theme(
    plot.title = element_text(size = 12,face = "bold"),
    plot.subtitle = element_text(size = 9),
    plot.caption = element_text(size = 9)
  )


iron(waffle_1, waffle_2, waffle_3)