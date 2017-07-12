
library(quantmod)
library(tidyverse)
library(geofacet)
library(stringr)
library(tidycensus)
library(fiftystater)
library(jsonlite)

# insert your own API key here
census_api_key(fromJSON("auth.json", flatten=TRUE))

#########################################
# Data Collection
#########################################


# Convert state codes to state gdp FRED code
state_codes = state.abb
state_codes[51] = 'DC'
state_gdps <- paste(state_codes, "NGSP", sep="")


# retrieve GDPs and combine the time series object into a vector
gdps <- lapply(state_gdps, function(sym) {
  getSymbols(sym, src="FRED", auto.assign=FALSE)
})
# merge all time series vectors together
gdps <- do.call(merge, gdps)

# convert time series vectors to dataframe
df = data.frame(date=time(gdps), coredata(gdps) )

# dataframe currently has a column for each state. convert columns
# into one state column.
df <- df %>% 
  gather(state_gdps, key='state', value='gdp')

# remove "NGSP" suffix so only state code remains
df$state <- gsub("NGSP", "", df$state)
df$date <- as.integer(format(df$date, '%Y'))

df$gdp <- df$gdp / 1000000


# obtain state land area
state_land <- get_decennial(geography = 'state', variables = 'AREALAND', year=2010)

# add DC to the standard state code repo
states <- data.frame('state_code'=state.abb, 'state_name'=state.name)
dc <- data.frame(state_code='DC', state_name='District of Columbia' )
states <- rbind(states, dc)

# combine state cd, name, land area
states <- states %>% 
  left_join(state_land, by = c('state_name' = 'NAME')) %>% 
  select(state_code, state_name,value)

# rename columns for states
names(states) <- c('state', 'state_full','area')

# create a 2016 subset of gdp and land area
gdp_2016 <- filter(df, date==2016)
gdp_area <- gdp_2016 %>% 
  left_join(states)

gdp_area$area <- gdp_area$area / 1000000000000


#########################################
# chloropleth map of state gdps
#########################################

low_color='#ccdbe5' 
high_color="#114365"
legend_title = 'GDP ($, trillions)'


ggplot(gdp_area, aes(map_id = state_full)) + 
  geom_map(aes(fill = gdp ),color="#ffffff",size=.15, map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  labs(x = "", y = "") +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  scale_fill_continuous(low = low_color, high= high_color, guide = guide_colorbar(title = legend_title)) + # creates shading pattern
  theme(#legend.position = "bottom", 
    panel.background = element_blank()) + 
  fifty_states_inset_boxes() +
  ggtitle('State GDP 2016', subtitle = 'California had the highest GDP by a wide margin')

ggsave('chloropleth_gdp_map.png', width = 5.3, height = 4.2, dpi = 120)


##########################################
## scatter plot of GDP and land area
##########################################

# top states are states which will be annotated
top_states <- gdp_area %>% 
  filter(gdp > .9 | area > 1 | state == 'IN')

# calculate r2 between variables for scatter plot
r2 = str_c("R-squared = ", format(cor(gdp_area$gdp, gdp_area$area) ** 2, digits=2, nsmall = 2))

# create plot
gdp_area %>% 
  ggplot() +
  geom_point(aes(x=area, y=gdp, color= (area > .5 | gdp > .9 | state=='IN'))) +
  scale_color_manual(name = '', values = c('black', '#1f77b4')) +
  geom_text(aes(x=area, y=gdp,label=state), color='#1f77b4', data=top_states, vjust=-.4) +
  ggtitle('State GDP vs. Land Area', subtitle = 'There is little correlation between GDP and state size' ) +
  xlab('Land Area (Sq. Km, millions)') + 
  ylab('GDP ($, trillions)') +
  annotate('text', x=1.4, y=2.65, label=r2) +
  scale_y_continuous(limits=c(0,2.75)) +
  scale_x_continuous(limits=c(0,1.6)) +
  theme(legend.position="none")

ggsave('state_gdp_area_scatter.png', width = 5.3, height = 4.2, dpi = 120)



#########################################
# simple bar chart of state gdps
#########################################

gdp_area %>% 
  ggplot()+
  geom_bar(mapping=aes(x=reorder(state, gdp), y=gdp), stat = 'identity') +
  coord_flip() +
  scale_y_continuous(expand = c(0,0), limits = c(0,2.75)) +
  ylab("GDP ($, trillions)") +
  xlab('State') +
  ggtitle('State GDP 2016') +
  theme(axis.text = element_text(size=6))

ggsave('state_gdp_bar_chart.png', width = 5.3, height = 5.3, dpi = 120)




#########################################
# tile map 
#########################################

create_gradient_state_tile_map <- function(state, value, title, legend_title, low_color='#ccdbe5', high_color="#114365", state_grid='us_state_grid2') {
  
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

# Using 2016 values, create a tile map
tile_map <- create_gradient_state_tile_map(gdp_area$state, gdp_area$gdp, title='State GDP 2016 \n', legend_title = "GDP, ($, trillions)" )
tile_map

ggsave('gdp_tile_map.png', width = 16, height = 9, dpi = 120)



#########################################
# tile area map 
#########################################

# create state tile map
ggplot(df, aes(date, gdp)) +
  geom_area(fill='#114365') +
  facet_geo(~ state, grid = "us_state_grid2", label="name") +
  scale_x_continuous(labels = function(x) paste0("'", substr(x, 3, 4))) +
  labs(title = "GDP by State 1997-2016 ($, trillions)",
       caption = "Data Source: St. Louis FRED",
       x = "Year",
       y = "GDP") +
  theme(strip.text.x = element_text(size = 8))

ggsave('gdp_tile_area_map.png', width=16, heigh=9, dpi = 120)







