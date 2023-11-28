install.packages("plotly")
library(plotly)
library(tidyverse)

getwd()
setwd("C:/Joas/UFERSA/CC/EPC/minicurso/Mapa")

minwage_df <- read_csv("Minimum Wage Data.csv")

minwage_df <- minwage_df %>% 
  select(Year, State, Wage = Department.Of.Labor.Cleaned.High.Value)

states <- read_csv("states.csv")

states <- states %>% 
  select(State = state, code)

novabase <- inner_join(minwage_df, states, by = "State")

novabase <- novabase %>% 
  select(Year, code, Wage)


novabase <- inner_join(novabase, states, by = "code")

novabase <- novabase %>% 
  mutate(hover = paste0(State, "\n$", Wage))

fontStyle <- list(
  family = "DM Sans",
  size = 15,
  color = "black"
)

label <- list(
  bgcolor = "#EEEEEE",
  bordercolor = "transparent",
  font = fontStyle
)

minwage_graph <- plot_geo(novabase,
                          locationmode = 'USA-states',
                          frame = ~Year) %>% 
  add_trace(locations = ~code,
            z = ~Wage,
            zmin = 0,
            zmax = max(novabase$Wage),
            color = ~Wage,
            colorscale = 'Electric',
            text = ~hover,
            hoverinfo = 'text') %>% 
  layout(geo = list(scope = 'usa'),
         font = list(family = "DM Sans"),
         title = "Minimum Wage in the US\n1968 - 2020") %>%
  style(hoverlabel = label) %>% 
  config(displayModeBar = FALSE) %>% 
  colorbar(tickprefix = "$")

minwage_graph



