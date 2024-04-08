#shiny libraries
suppressMessages(library(shiny))
suppressMessages(library(shinythemes))
suppressMessages(library(shinydashboard))
suppressMessages(library(shinyWidgets))

#shiny data table & charting libraries
suppressMessages(library(DT))
suppressMessages(library(plotly))
suppressMessages(library(maps))
suppressMessages(library(mapdata))
suppressMessages(library(rworldmap))
suppressMessages(library(jpeg))

#tidyverse libraries
suppressMessages(library(dplyr))
suppressMessages(library(ggrepel))
suppressMessages(library(scales))
suppressMessages(library(ggpubr))

suppressMessages(library(rsconnect))
suppressMessages(library(png))
df = as.data.frame(read.csv("curated_data.csv"))

country = unique(df$nationality)
league_name = unique(df$league_name)
team = unique(df$club_name)

coor = read.csv("world_country_latitude_and_longitude_values.csv")
coor = coor %>%
  select(latitude, longitude, country)