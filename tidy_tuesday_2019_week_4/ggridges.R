remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

library(tidyverse);library(ggridges);library(hrbrthemes)


##
x <- read_csv("prison_population.csv") %>%
  filter(pop_category == "Total")

x <- x %>% filter(!is.na(prison_population)) %>%
  mutate(rate_per_10000 = prison_population/population*10000)


x <- x %>% filter(!is.nan(rate_per_10000) & rate_per_10000>0)
##
yrs <- c(seq(1983, 2013, by = 4),2015)

#
p <- ggplot(data = subset(x, year %in% yrs),
            mapping = aes(x = rate_per_10000,y = fct_rev(factor(year))))

p + geom_density_ridges(aes(fill = region),
                        scale =1.5,
                        alpha = 0.8,
                        quantile_lines = TRUE,
                        quantiles = 2) + 
  scale_x_log10() + 
  scale_y_discrete(expand = c(0,2)) + 
  scale_fill_viridis_d(option = "inferno", begin = 0.4,end = 0.8) + 
  facet_wrap(~factor(region, levels = c("Midwest", "South", "Northeast","West")), ncol = 1, scales = "free_y") + 
  theme_ipsum_rc() + 
  theme(
    text = element_text(size = 13),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 12),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  ) + 
  labs(y = NULL, x = "Prison population per 10,000 (log scale)", 
       title = "Evolution of the prison population per 10,000 across different regions",
       subtitle = "Counties with no prison population were excluded") + 
  guides(fill = FALSE)




