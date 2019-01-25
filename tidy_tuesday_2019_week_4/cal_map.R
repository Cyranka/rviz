cal_breaks <- log(c(1,2,4,8,16,32,64,128,256,512,904), base = 2)

counties %>% left_join(l %>% mutate(total_ice = round(total_ice,0)),
                       by = c("county_name" = "county_name", "state_abbv" = "state")) %>%
  filter(state_abbv %in% c("CA")) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = log(round(total_ice,0),2))) +
  geom_polygon(color = "gray60", size = 0.15,
               show.legend = TRUE) + 
  hrbrthemes::theme_ipsum_rc() + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "bottom"
  ) +
  scale_fill_viridis(option = "B", na.value = "gray10", begin = 0.3,
                     end = 0.9,
                     breaks = cal_breaks,
                     labels = as.character(c(1,2,4,8,16,32,64,128,256,512,904))) + 
  guides(fill = guide_colorbar(title = "Total jail from ICE",
                               title.position = "top",barwidth = 15)) + 
  labs(title = "Incarceration by ICE in California",
       subtitle = "Numbers refer to the last year with available data in the last decade",
       caption = "Data from the @verainstitute")