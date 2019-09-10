remove(list =ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

setwd("/Users/harro.cyranka/Desktop/rviz/")
library(tidytext);library(tidyverse);library(sf);library(pdftools)

x <- read_sf("rj_municipios/33MUE250GC_SIR.shp") %>%
  janitor::clean_names()

x <- x %>% 
  mutate(nm_municip = str_to_title(nm_municip))


# Homicide rate -----------------------------------------------------------
k <- pdf_text("homicidios_uf.pdf")
fix_strings <- function(string_position){
  string_1  <- k[[string_position]] %>% str_replace_all("[\r\n\t]", "") %>%
    tm::stripWhitespace() %>%
    str_replace_all("XII|IX|\\sII\\s", "xii|ix|ii")
  
   
  
  
  uf <- str_extract_all(string_1, "[A-Z]{2}")
  city <- str_match_all(string_1,"[A-Z]{2} (.*?) [0-9]{1,10}")
  #population <- str_extract_all(string_1,"[0-9]{1,10}\\.[0-9]{1,10}|")
  homicide_rate <- str_extract_all(string_1,"[0-9]{1,10}\\,[0-9]{1,10}")
  
  
  z <- tibble(
    uf = unlist(uf),
    city = city[[1]][,2],
    #population = unlist(population),
    homicide_rate = unlist(homicide_rate)
  )
  
  return(z)
}


list_1 <- lapply(2:116, function(i){
  print(i)
  fix_strings(i)
  })

rj<- bind_rows(list_1) %>%
  filter(uf == "RJ") %>%
  mutate(homicide_rate = parse_number(str_replace_all(homicide_rate,",",".")),
         city = str_to_title(city))
map <- x %>% right_join(rj, by = c("nm_municip" = "city"))


# Create map --------------------------------------------------------------
map %>%
  mutate(homicide_rate = ifelse(homicide_rate >90, 90,homicide_rate)) %>% 
  ggplot() + 
  geom_sf(aes(fill = homicide_rate),
          size = 0.2, 
          alpha =0.9,
          color = "gray40") + 
  scale_fill_viridis_c(option = "C",
                       breaks = c(0,20,40,60,80,90)) + 
  theme_minimal() + 
  theme(
    text = element_text(color = "white", family = "Open Sans Condensed Bold"),
    plot.background = element_rect(fill = "gray15"),
    plot.title = element_text(size = 18,face = "bold"),
    plot.subtitle = element_text(size = 15),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom"
  ) + 
  labs(title = "Heat map of the homicide rate in the state of Rio de Janeiro in 2017",
       subtitle = "Four cities had no reported homicides: Cambuci, Macuco, Sumidouro, and Natividade\nData provided by the IPEA",
       caption = "Map designed by @harrocyranka") + 
  guides(fill = guide_colorbar(title = "Homicide rate in 2017",
                               title.position = "top",
                               title.hjust = 0.5,barwidth = 20,
                               barheight = 0.6)) + 
  geom_curve(aes(x = -43.58505, y = -22.72557,
                 xend = -43,yend = -23.2),
             curvature = 0.2, size =0.1,
             color = "gray80",linetype = 1
             ) + 
  annotate("text", x = -42.2,y = -23.2,
           label = "City with the highest homicide rate: Queimados (115.6)",color = "gray80",
           family = "Roboto Condensed", 
           fontface = "bold",
           size = 3.5)
  

