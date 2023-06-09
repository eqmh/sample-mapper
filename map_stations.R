#### This script maps stations from surveyed areas 
#### #### from: https://www.r-graph-gallery.com/19-map-leafletr.html
#### Enrique Montes
#### June 08, 2023

# Library
library(leaflet)
library(tidyverse)

setwd("~/Documents/sample-mapper")

rm(list=ls()) ## clear variables

df1 <- read.csv("AOML_SFP_regional_WQ_surface_v16.csv", header = TRUE)
df2 <- read.csv("SFER_stations.csv", header = TRUE)

# Convert 'Date' column in df1 to Date format
df1$Date <- as.Date(df1$Date, format = "%m/%d/%Y")


# Group df1 by 'Station' and get the minimum date for each group
earliest_dates <- df1 %>%
  group_by(Station) %>%
  summarize(earliest_date = min(Date, na.rm = TRUE))

# Merge earliest_dates with df2 to get the earliest date for each site in df2
df2_with_earliest_dates <- merge(df2, earliest_dates, by = "Station", all.x = TRUE)
df2_with_earliest_dates$earliest_date[is.na(df2_with_earliest_dates$earliest_date)] <- as.Date("2022-10-11")

# Convert 'earliest_dates' to decimal year format
df2_with_earliest_dates$earliest_date <- as.numeric(format(df2_with_earliest_dates$earliest_date, "%Y")) +
  as.numeric(format(df2_with_earliest_dates$earliest_date, "%j")) / 365

# Round the decimal year to one decimal place
df2_with_earliest_dates$earliest_date <- round(df2_with_earliest_dates$earliest_date, 1)

# Create a leaflet map
m <- leaflet(df2_with_earliest_dates) %>%
  addTiles() %>% 
  setView( lat=26, lng=-82, zoom=7) %>%
  addProviderTiles("Esri.WorldImagery")  # Add base tiles to the map

# Convert color_palette to a function
color_palette <- colorNumeric(
  palette = "RdYlBu",
  domain = df2_with_earliest_dates$earliest_date
)

# Add circle markers to the map
m <- m %>% addCircleMarkers(
  lng = ~dec.lon,
  lat = ~dec.lat,
  color = ~color_palette(earliest_date),  # Updated column name
  radius = 5,
  opacity = 1,
  fillOpacity = 0.8,
  label = ~paste("Station:", Station,
                 "Earliest Date:", earliest_date)
)

# Add color scale legend
m <- m %>% addLegend(
  position = "bottomright",
  pal = color_palette,
  values = ~earliest_date,
  title = "Start year",
  opacity = 1
)

# Display the map
m
