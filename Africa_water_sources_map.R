# Load libraries ____________________
library(dplyr)
library(rnaturalearth)
library(ggspatial)
library(ggplot2)

# List of non African countries
non_africa <- c('Timor-Leste', 'Peru', 'Dominican Republic')
improved_water  <-  c('Piped Water', 'Protected Spring', 'Rainwater Harvesting', 'Protected Shallow Well', 'Borehole')

# Load data
water <- read.csv('water.csv') %>% 
  filter(!(country_name %in% non_africa)) %>% 
  mutate(improved_source = case_when(water_source %in% improved_water ~ 'improved',
                                     TRUE ~ 'unimproved'))

africa <- ne_countries(scale = 'medium', returnclass = 'sf', continent = 'Africa')

africa_points <- st_as_sf(water, coords = c("lon_deg", "lat_deg"), crs = 4326)
water$merged <- !is.na(as.numeric(st_intersects(africa_points$geometry, africa))) 

water <-   water %>% filter(merged == TRUE) 

# Create map
ggplot(data = africa) + 
  geom_sf(fill = 'antiquewhite') +
  geom_point(
    data = water,
    mapping = aes(x = lon_deg, y = lat_deg, color = improved_source),
    size = 0.01,
    alpha = 0.09) +
  xlab('Longitude') +
  ylab('Latitude') +
  theme(legend.position = 'none') +
  ggtitle('Sources of water in Africa', 
          subtitle = paste0('Data from ', length(unique(water$country_name)), ' countries. About ', 
                                                          round(nrow(water %>% filter(improved_source == "improved"))/nrow(water)*100,0), "% of sources are improved")) +
  annotation_scale(location = 'bl', width_hint = 0.5) +
  labs(caption = "Data: waterpointdata.org | Visualization: @lukorir") +
  annotation_north_arrow(location = 'tr', which_north = 'true',
                         pad_x = unit(0.1, 'in'),
                         pad_y = unit(0.05, 'in'), 
                         style = north_arrow_fancy_orienteering) + 
  coord_sf(xlim = c(-20, 52), ylim = c(-40, 40), expand = FALSE) +
  scale_color_manual(values = c("improved" = "#99FF00", "unimproved" = "#FF3300")) +
    theme(panel.grid.major = element_line(color = gray(.5), 
                                          linetype = 'dashed', 
                                          size = 0.5), 
          panel.background = element_rect(fill = 'aliceblue')) +
  geom_rect(aes(xmin = -18, xmax = 10, ymin = -11, ymax = -20),
            alpha = 1/5,
            fill = "white") +  
  annotate(geom = 'text', x = -7.5,   y = -17, label = "Improved",  size = 6, color = "#99FF00") +
  annotate(geom = 'text', x = -5,   y = -13, label = "Unimproved",  size = 6, color = "#FF3300") 
  
  ggsave("africa_water_sources_map.png", width = 6, height = 6, dpi = "screen")


