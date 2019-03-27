remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)


setwd("/Users/harro.cyranka/Desktop/rviz/")
library(tidyverse);library(lubridate);library(forecast)


seattle_pets <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-26/seattle_pets.csv")

# Clean variables and filter dates after 2017 -----------------------------
seattle_pets <- seattle_pets %>%
  separate(license_issue_date, into = c("month", "day", "year")) %>%
  mutate(month = case_when(
    month == "January" ~ 1,
    month == "February" ~2,
    month == "March" ~3,
    month == "April"~4,
    month == "May"~5,
    month == "June" ~6,
    month == "July" ~7,
    month == "August" ~8,
    month == "September" ~9,
    month == "October" ~10,
    month == "November" ~11,
    month == "December" ~12
  )) %>%
  mutate(date = ymd(paste0(year,"-",month,"-",day)),
         zip_code = case_when(
           nchar(zip_code) == 4 ~ paste0("0", zip_code),
           nchar(zip_code) == 3 ~ paste0("00", zip_code),
           TRUE ~ zip_code
         ),
         zip_code = str_replace_all(zip_code, "-[0-9]{3,5}","")) %>%
  select(date, license_number:zip_code) %>%
  arrange(date) %>%
  filter(date >=ymd("2017-01-01"))



x <- seattle_pets %>%
  filter(!is.na(animals_name),
         species %in% c("Cat", "Dog"),
         !is.na(zip_code))

bar_plot <- x %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month, species) %>% tally() %>%
  ggplot(aes(x = month, y = n, fill = species)) + 
  geom_col(position = "dodge", color = "gray70") + 
  hrbrthemes::theme_ft_rc(axis_title_size = 13) + 
  scale_fill_viridis_d(begin = c(0.6), end = 0.9, option = "B") + 
  theme(
    text = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    axis.text.x = element_text(angle = 90),
    axis.title = element_text(color = "white"),
    panel.grid = element_line(color = "gray90"),
    legend.position = "bottom"
  ) + 
  labs(x = "\nDate", y = "Total pets\n",
       title = "Does seattle love dogs more than cats?",
       subtitle = "Registration of canines far outpaces registration for felines") + 
  scale_x_date(breaks = seq(as.Date("2017-01-01"), 
                                 as.Date("2018-12-31"), by = "1 month"),date_labels = "%b/%y") + 
  scale_y_continuous(labels = scales::comma) + 
  guides(fill = guide_legend(title = "Species",
                             title.position = "top",
                             title.hjust = 0.5,
                             label.position = "bottom",
                             keywidth = 3, keyheight = 0.5))


##
library(ggwordcloud)

x %>%
  filter(species == "Dog") %>%
  group_by(animals_name) %>% tally() %>%
  filter(n >= 20) %>% ungroup() %>%
  arrange(desc(n)) %>%
  ggplot(aes(size = n, label = animals_name, color = log(n))) + 
  ggwordcloud::geom_text_wordcloud(family = "Roboto Condensed",
                                   fontface = "bold",
                                   shape = "square") + 
  theme_void() + 
  scale_size_area(max_size = 10) + 
  scale_color_viridis_c(option = "A", begin = 0.3, end = 0.85) + 
  theme(
    plot.background = element_rect(fill = "black"),
    plot.caption = element_text(color = "white", size = 10,family = "Roboto Condensed"),
    legend.position = "bottom",
    plot.title = element_text(color = "white", size = 13, hjust = 0.5, face = "bold", family = "Roboto Condensed"),
    plot.subtitle = element_text(color = "white", hjust = 0.5, face = "bold", family = "Roboto Condensed")
  ) + 
  labs(title = "\n\nWord cloud of most common dog names among registered pets in Seattle",
       subtitle = "Names that have appeared at least 20 times since January 2017",
       caption = "Tidy tuesday week 13: Seattle pet names")

x %>%
  filter(species == "Cat") %>%
  group_by(animals_name) %>% tally() %>%
  filter(n >= 10) %>% ungroup() %>%
  arrange(desc(n)) %>%
  ggplot(aes(size = n, label = animals_name, color = log(n))) + 
  ggwordcloud::geom_text_wordcloud(family = "Roboto Condensed",
                                   fontface = "bold",
                                   shape = "square") + 
  theme_void() + 
  scale_size_area(max_size = 10) + 
  scale_color_viridis_c(option = "D", begin = 0.3, end = 0.85) + 
  theme(
    plot.background = element_rect(fill = "black"),
    plot.caption = element_text(color = "white", size = 10,family = "Roboto Condensed"),
    legend.position = "bottom",
    plot.title = element_text(color = "white", size = 13, hjust = 0.5, face = "bold", family = "Roboto Condensed"),
    plot.subtitle = element_text(color = "white", hjust = 0.5, face = "bold", family = "Roboto Condensed")
  ) + 
  labs(title = "\n\nWord cloud of most common cat names among registered pets in Seattle",
       subtitle = "Names that have appeared at least 10 times since January 2017",
       caption = "Tidy tuesday week 13: Seattle pet names")