#### This script maps sampling statistics from surveyed areas 
#### #### from: https://www.r-graph-gallery.com/19-map-leafletr.html
#### Enrique Montes
#### November 12, 2022

# Library
library(leaflet)
library(tidyverse)

# setwd("/Volumes/GoogleDrive/My Drive/MBON/eDNA")

rm(list=ls()) ## clear variables

data <- read.csv("edna-metadata-summary-oct2022.csv", header = TRUE)

lon <- data$longitude
lat <- data$latitude

station_ls <- unique(data$station)

df <- matrix(ncol = 4, nrow = length(station_ls))
for (i in 1:length(station_ls)){
  station_id <- station_ls[i]
  edna_tb <- filter(data, station == station_id)
  edna_recs <- nrow(edna_tb)
  edna_lon <- edna_tb$longitude[1]
  edna_lat <- edna_tb$latitude[1]
  
  df[i,] <- cbind(station_id, edna_lon, edna_lat, edna_recs)
}

df <- as.data.frame(df)
colnames(df) <- c("station", "longitude", "latitude", "eDNA")
df$longitude <- as.numeric(as.character(df$longitude))
df$latitude <- as.numeric(as.character(df$latitude))
df$eDNA <- as.numeric(as.character(df$eDNA))

# for number of eDNA
mybins <- seq(0, 160, by=40)
mypalette <- colorBin( palette="YlOrBr", domain=df$eDNA, na.color="transparent", bins=mybins)

# Prepare the text for the tooltip:
mytext <- paste(
  "Station: ", df$station, "<br/>", 
  "Longitude: ", df$longitude, "<br/>", 
  "Latitude: ", df$latitude, "<br/>", 
  "eDNA samples: ", df$eDNA, sep="") %>%
  lapply(htmltools::HTML)

# Final Map
m <- leaflet(df) %>% 
  addTiles()  %>% 
  setView( lat=26, lng=-82, zoom=7) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(~longitude, ~latitude, 
                   fillColor = ~mypalette(eDNA), fillOpacity = 0.7, color="white", radius=12, stroke=FALSE,
                   label = mytext,
                   labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
  ) %>%
  addLegend( pal=mypalette, values=~eDNA, opacity=0.9, title = "Number of eDNA samples", position = "bottomright" )

m 