# r_maps

This repository provides examples of how to visualize geographic data. The function create_gradient_state_tile_map 
provides users with a standard function to turn any state, value vectors into a ready formatted tilemap.

Dependencies
```r
library(tidyverse)
library(geofacet)
```
Tile maps are excellent alternatives to standard, chloropleth maps. This function will create a gradient
tile map if provided a state and a value.
```r
create_gradient_state_tile_map <- function(state, value, title, legend_title, 
		low_color='#ccdbe5', high_color="#114365", state_grid='us_state_grid2') {
  
  df <- as.tibble(data.frame(state, value))
  
  fig <- df %>% 
    mutate(x = 1) %>% # size of the bar being plotted. All bars should be same size to make perfect squares
    mutate(label_y = .5) %>%  # this location of state labels
    mutate(label_x = 1) %>% 
    ggplot()+
    geom_bar(mapping=aes(x=x, fill=value))  +
    facet_geo(~ state, grid=state_grid) +
    scale_fill_continuous(low = low_color, high= high_color, guide = guide_colorbar(title = legend_title)) + # creates shading pattern
    ggtitle(title) +
    theme_classic() + # theme classic removes many ggplot defaults (grey background, etc)
    theme(plot.title = element_text(size = 28), # format the plot
          plot.margin = unit(c(1,1,1,1), "cm"),
          legend.text=element_text(size=16),
          legend.title = element_text(size=16),
          axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks = element_blank(),
          strip.text.x = element_blank(),
          axis.line = element_blank()) +
    geom_text(aes(x=label_x, y=label_y, label=state), color='#ffffff', size=10) 
  
  return(fig)
}

tile_map <- create_gradient_state_tile_map(gdp_area$state, gdp_area$gdp, title='State GDP 2016 \n', 
	legend_title = "GDP, ($, trillions)" )
tile_map
```
![Image of Tilemap](https://github.com/NickDoesData/r_maps/blob/master/plots/gdp_tile_map.png)

