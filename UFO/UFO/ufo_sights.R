library(tidyverse)

ufo <- read_csv("ufo-sightings-transformed.csv")

ufo_per_country <- ufo %>% 
  group_by(Country) %>% 
  summarise(
    n = n()
  )

ufo_per_country %>% 
  ggplot() + 
  geom_col(aes(x = Country, y = n, fill = Country))

ufo_usa <- ufo %>% 
  filter(Country_Code == "USA") %>% 
  group_by(Region) %>% 
  summarise(
    tot_avistamentos = n()
  )

ufo_usa %>% 
  arrange(desc(tot_avistamentos)) %>% 
  head(10) %>% 
  ggplot() +
  geom_col(aes(x = Region, y = tot_avistamentos, fill = Region)) 

ufo %>% 
  filter(Country_Code == "USA") %>% 
  group_by(Season) %>% 
  summarise(
    tot_season = n()
  ) %>% 
  ggplot() +
  geom_col(aes(x = Season, y = tot_season, fill = Season))

ufo %>% 
  filter(Country %in% c("Brazil", "Russia", "India", "China", "South Africa")) %>% 
  group_by(Country) %>% 
  summarise(
    n = n()
  )

################################
# Gráfico
ufo_simples <- ufo %>% 
  select(longitude, latitude, Date_time, UFO_shape)

library(plotly)

geo_properties <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  showsubunits = FALSE,
  landcolor = toRGB('gray10'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

ufos_graphs <- plot_geo(ufo_simples,
                        lat = ~latitude,
                        lon = ~longitude,
                        marker = list(size = 2, color = "#FFFFCC", 
                                      opacity = 0.25)) %>% 
  add_markers(hoverinfo = "none") %>% 
  config(displayModeBar = FALSE) %>% 
  layout(geo = geo_properties,
         font = list(family = "DM Sans"),
         title = "UFO Sightings in the US\n1949-2014")

ufos_graphs

sum(ufo_usa$tot_avistamentos)
