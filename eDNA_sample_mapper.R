
library(readxl)
library(leaflet)
library(dplyr)
library(scales)

# load data table
data <- read_excel("merged.xlsx")
sfer_list <- read.csv('SFER_stations.csv')

# create list of unique stations
sta_list <- unique(data$station)

# Filter the dataset to include only rows with stations in sta_list and exclude "not applicable"
filtered_data <- data[data$station %in% sta_list & data$station != "not applicable", ]

# Step 3: Count the number of instances for each station
station_counts <- as.data.frame(table(filtered_data$station))
colnames(station_counts) <- c("Station", "number_of_samples")

# Step 4: Merge the mean coordinates and station counts
final_table <- merge(sfer_list, station_counts, by = "Station")

# Aggregate to find min and max dates for each station
date_range <- aggregate(collection_date ~ Station, data = filtered_data, 
                        FUN = function(x) c(min_Date = min(x), max_Date = max(x)))
date_range_expanded <- data.frame(
  Station = date_range$Station,
  min_Date = date_range$collection_date[, "min_Date"],
  max_Date = date_range$collection_date[, "max_Date"]
)

# Merge the date ranges into 'final_table'
final_table <- merge(final_table, date_range_expanded, by = "Station", all.x = TRUE)

# # FOR MAPPING:
# Step 1: Normalize date range (duration in days) for color coding
final_table$max_Date <- as.Date(final_table$max_Date, format = "%Y-%m-%d")
final_table$min_Date <- as.Date(final_table$min_Date, format = "%Y-%m-%d")

final_table <- final_table %>%
  mutate(Duration_Days = as.numeric(max_Date - min_Date))  # Duration in days

# Normalize duration to a scale of 0 to 1 for color mapping
final_table <- final_table %>%
  mutate(Duration_Scaled = rescale(Duration_Days, to = c(0, 1)))

# Generate color palette (blue for short durations, red for long durations)
color_palette <- colorNumeric(palette = "RdYlGn", domain = final_table$Duration_Scaled, reverse = TRUE)

# Step 2: Scale the number of samples for circle size
final_table <- final_table %>%
  mutate(Circle_Size = rescale(number_of_samples, to = c(3, 20)))  # Scale to reasonable sizes

# Step 3: Create the interactive map
leaflet(data = final_table) %>%
  addTiles() %>%
  setView( lat=26, lng=-82, zoom=7) %>%
  addProviderTiles("Esri.WorldImagery") %>% # Add base tiles to the map
  addCircleMarkers(
    ~dec.lon, ~dec.lat,  # Use station coordinates
    radius = ~Circle_Size,            # Circle size proportional to number of samples
    color = ~color_palette(Duration_Scaled),  # Color based on scaled duration
    fillOpacity = 0.8,                # Set circle opacity
    popup = ~paste0(
      "<b>Station:</b> ", Station, "<br>",
      "<b>Mean Latitude:</b> ", round(dec.lat, 4), "<br>",
      "<b>Mean Longitude:</b> ", round(dec.lon, 4), "<br>",
      "<b>Number of Samples:</b> ", number_of_samples, "<br>",
      "<b>Min Date:</b> ", min_Date, "<br>",
      "<b>Max Date:</b> ", max_Date, "<br>",
      "<b>Date Range (Days):</b> ", Duration_Days
    )  # Popup with station info
  ) %>%
  addLegend(
    "bottomright", 
    pal = color_palette, 
    values = ~Duration_Scaled,
    title = "Date Range (Scaled)",
    opacity = 1
  )




