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



