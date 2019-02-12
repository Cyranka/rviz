remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)


library(tidyverse)
x <- read_csv("climate_spending.csv") %>%
    filter(year %in% c(2000,2017)) %>%
    mutate(gcc_spending = gcc_spending/1000000,
           department = case_when(
             department == "Commerce (NOAA)" ~"NOAA",
             department == "All Other" ~ "Other federal agencies",
             TRUE ~ department
           ))

total_changes <- x %>% 
    spread(year, gcc_spending) %>%
    mutate(change = round((`2017`/`2000` - 1)*100,1)) %>%
    inner_join(x %>% filter(year == 2017) %>% mutate(label_pos = gcc_spending*1.5))


x %>% mutate(my_color = ifelse(year == 2000,"blue", "red")) %>%
    ggplot(aes(x = gcc_spending,
               y = reorder(department,gcc_spending),
               color = my_color,
               group = department)) + 
    geom_line(size = 1, show.legend = FALSE, color = "black") + 
    geom_point(size = 4, show.legend = TRUE, alpha = 0.5) + 
    scale_x_log10(labels = scales::comma) + 
    hrbrthemes::theme_ipsum_rc(axis_title_size = 12, axis_title_face = "bold") + 
    scale_color_manual(
        values = c("firebrick3", "steelblue"),
        labels = c("2000","2017")
    ) + 
    labs(x = "Climate spending (in millions of dollars)",
         y = "Agency/Department",
         title = "Changes in climate spending between 2000 and 2017",
         subtitle = "Data sorted by total outlays",
         caption = "Tidy tuesday 2019 Week 7: Federal R&D Spending") + 
    theme(
        text = element_text(size = 13),
        panel.grid = element_line(size = 0.05,color = "gray90"),
        legend.position = "bottom"
    ) + 
    guides(color = guide_legend(
      title = "Year",
      title.position = "top",
      title.hjust = 0.5,label.position = "bottom"
    )) + 
    annotate("label", x = 94.5,y = "Interior",label = "70.3% increase", size = 3,
             fontface = "bold") + 
    annotate("label", x = 156,y = "Other federal agencies",label = "52.8% decrease", size = 3,
             fontface = "bold") + 
    annotate("label", x = 186,y = "Agriculture",label = "61.6% increase", size = 3,
             fontface = "bold") + 
    annotate("label", x = 363,y = "Energy",label = "56.3% increase", size = 3,
             fontface = "bold") + 
    annotate("label", x = 513,y = "NOAA",label = "272% increase", size = 3,
             fontface = "bold") + 
    annotate("label", x = 522,y = "NSF",label = "35.8% increase", size = 3,
             fontface = "bold") + 
    annotate("label", x = 1100,y = "NASA",label = "2.6% increase", size = 3,
             fontface = "bold")

###
y <- read_csv("climate_spending.csv")

y %>%
    mutate(gcc_spending = gcc_spending/1000000,
           department = case_when(
               department == "Commerce (NOAA)" ~"NOAA",
               department == "All Other" ~ "Other",
               TRUE ~ department
           )) %>%
    ggplot(aes(x = year, y = gcc_spending, fill = department)) + 
    geom_area(show.legend = FALSE, alpha = 0.9) + facet_wrap(~fct_reorder(department,
                                         gcc_spending, sum, .desc = TRUE),nrow = 1,strip.position = "top") + 
    scale_y_log10(labels = scales::comma) + 
    scale_x_continuous(breaks = c(seq(2000,2015,by = 4), 2015)) + 
    hrbrthemes::theme_ipsum_rc(axis_text_size = 8)+ 
    theme(
        panel.spacing = unit(-0.7, "lines"),
        axis.text.x = element_text(angle = 90,hjust = 30),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size =0.01,color = "gray80"),
        strip.background = element_rect(color = "gray70",fill = "gray90")
    ) + 
    labs(x = "Year", y = "\nClimate spending in millions of dollars (log 10 scale)",
         title = "Yearly changes in climate spending by agency",
         subtitle = "Data sorted by total outlays",
         caption = "Tidy tuesday 2019 Week 7: Federal R&D Spending") + 
    scale_fill_viridis_d(option = "D", begin = 0.2,end = 0.95)