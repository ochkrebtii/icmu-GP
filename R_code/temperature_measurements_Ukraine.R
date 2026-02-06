# Install and load necessary libraries
if (!require("sf")) install.packages("sf")
if (!require("giscoR")) install.packages("giscoR")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("ggrepel")) install.packages("ggrepel")

if (!require("rnaturalearth")) install.packages("rnaturalearth")
if (!require("rnaturalearthdata")) install.packages("rnaturalearthdata")

library(sf)
library(giscoR)
library(ggplot2)
library(ggrepel)
library(dplyr)

library(rnaturalearth)

# Get Ukraine outline from an alternative source
ukraine_outline <- ne_countries(country = "ukraine", scale = "medium", returnclass = "sf")


# Get the official outline of Ukraine
# 'resolution = 1' is high detail; 'epsg = 4326' is standard Lat/Lon
ukraine_outline <- gisco_get_countries(country = "Ukraine", resolution = "1")

# Create a data frame for our 10 stations (using the coordinates from before)
station_data <- data.frame(
  station = c("KIEV/ZHULIANY", "LVIV INTL", "KHARKIV", "ODESA", "DNIPROPETROVSK", 
              "CHERNIHIV", "IVANO-FRANKIVSK", "POLTAVA", "SUMY", "ZAPORIZHZHYA"),
  lat = c(50.40, 49.81, 49.92, 46.43, 48.36, 51.48, 48.88, 49.61, 50.85, 47.82),
  lng = c(30.45, 23.95, 36.28, 30.77, 35.10, 31.23, 24.69, 34.49, 34.67, 35.22),
  temp = c(2.5, 1.2, -3.4, 5.8, -1.1, 0.5, 2.1, -0.8, -2.5, 1.4) # Example latest temps
)

# Convert station data to an sf object
stations_sf <- st_as_sf(station_data, coords = c("lng", "lat"), crs = 4326)

# Build the plot
ggplot() +
  # Draw the Ukraine outline
  geom_sf(data = ukraine_outline, fill = "#f9f9f9", color = "darkgrey", size = 0.5) +
  # Add the points colored by temperature
  geom_sf(data = stations_sf, aes(color = temp), size = 4) +
  # Add labels that don't overlap using ggrepel
  geom_text_repel(
    data = station_data, 
    aes(x = lng, y = lat, label = paste0(station, " (", temp, "°C)")),
    size = 3, fontface = "bold", box.padding = 0.5
  ) +
  # Style the colors (Blue for cold, Red for warm)
  scale_color_gradient2(low = "blue", mid = "yellow", high = "red", midpoint = 0) +
  # Aesthetic clean-up
  theme_minimal() +
  labs(
    title = "Latest Air Temperatures Across Ukraine",
    subtitle = "Source: NOAA Integrated Surface Database (ISD)",
    color = "Temp (°C)",
    x = "Longitude", y = "Latitude"
  ) +
  theme(panel.background = element_rect(fill = "aliceblue", color = NA))




