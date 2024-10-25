# Load libraries
library(sf)
library(dplyr)
library(ggplot2)
library(stringr)
library(rvest)

# File paths and URLs
districts_file <- "/Users/R Studio/bezirksgrenzen.geojson"  # Berlin districts GeoJSON path
christmas_markets_url <- "https://www.berlin.de/sen/web/service/maerkte-feste/weihnachtsmaerkte/index.php/index/all.geojson?q="  # URL to the Christmas markets GeoJSON
population_url <- "https://de.wikipedia.org/wiki/Bevölkerung_von_Berlin"  # Wikipedia page with population data

# Function to clean and standardize district names
clean_district_names <- function(name) {
  name %>%
    str_replace_all("-", " ") %>%  # Replace dashes with spaces
    str_replace_all("[^\\w\\säöüÄÖÜß]", "") %>%  # Remove any non-alphanumeric and non-German characters
    str_squish() %>%  # Remove extra whitespace
    str_to_title()  # Convert to title case
}

# Read Berlin districts GeoJSON file
berlin_districts <- st_read(districts_file) %>%
  mutate(Gemeinde_name = clean_district_names(Gemeinde_name))

# Read the Christmas markets GeoJSON directly from the URL
christmas_markets <- st_read(christmas_markets_url)

# Ensure the coordinate systems are the same
st_crs(christmas_markets) <- st_crs(berlin_districts)

# Filter markets to keep only those within the Berlin districts
christmas_markets_berlin <- st_intersection(christmas_markets, berlin_districts)

# Calculate centroids of the Berlin districts to position the names
berlin_districts_centroids <- st_centroid(berlin_districts) %>%
  mutate(
    split_name = str_wrap(Gemeinde_name, width = 10)  # Wrap names into lines of 10 characters
  )

# Join Christmas markets with district data and count the number of markets per district
markets_per_district <- st_join(christmas_markets_berlin, berlin_districts, join = st_intersects) %>%
  st_drop_geometry() %>%
  group_by(Gemeinde_name.y) %>%
  summarise(Christmas_Markets = n()) %>%
  mutate(Gemeinde_name.y = clean_district_names(Gemeinde_name.y))

# Scrape population data from Wikipedia
population_data <- read_html(population_url) %>%
  html_node("table.wikitable") %>%
  html_table() %>%
  select(Bezirk = 1, Bevölkerung = 2) %>%
  mutate(
    # Clean the district names
    Bezirk = str_squish(Bezirk) %>%
      str_replace_all("-", " ") %>%
      str_replace_all("\\s+", " ") %>%
      str_to_title(),
    
    # Remove duplicates within the district names
    Bezirk = sapply(Bezirk, function(x) {
      words <- unlist(str_split(x, " "))
      unique_words <- unique(words)
      paste(unique_words, collapse = " ")
    }),
    
    # Clean the population numbers
    Bevölkerung = as.numeric(gsub("\\.", "", Bevölkerung))
  ) %>%
  filter(!is.na(Bevölkerung))

# Merge the markets and population data
final_table <- markets_per_district %>%
  left_join(population_data, by = c("Gemeinde_name.y" = "Bezirk")) %>%
  mutate(
    Market_Density_per_1000_residents = (Christmas_Markets / Bevölkerung) * 1000  # Markets per 1,000 residents
  ) %>%
  select(District = Gemeinde_name.y, Population = Bevölkerung, Christmas_Markets, Market_Density_per_1000_residents) %>%
  arrange(desc(Market_Density_per_1000_residents))

# Join the density data back to the berlin_districts for visualization
berlin_districts <- berlin_districts %>%
  mutate(District = clean_district_names(Gemeinde_name)) %>%
  left_join(final_table, by = c("District" = "District"))

# Manually set coordinates for Hertie School (approximate coordinates for Friedrichstraße 180)
hertie_coords <- c(13.388860, 52.520008)  # Longitude, Latitude for Hertie School

# Create an sf point for Hertie School
hertie_point <- st_as_sf(data.frame(
  name = "Hertie",
  geometry = st_sfc(st_point(hertie_coords)),
  stringsAsFactors = FALSE
), crs = st_crs(berlin_districts))

# Distance Calculation between Hertie School and each Christmas market
distances <- st_distance(hertie_point, christmas_markets_berlin) %>% as.numeric()

# Create a table with distances and market information
distance_table <- christmas_markets_berlin %>%
  st_drop_geometry() %>%
  mutate(Distance_to_Hertie_m = distances) %>%
  arrange(Distance_to_Hertie_m) %>%
  slice(1:5)  # Select the 5 shortest distances

# Display the optimized distance table for readability
optimized_distance_table <- distance_table %>%
  select(Christmas_Market = title, Distance_to_Hertie_m)  # Adjust column names for clarity

# Print the optimized table
print(optimized_distance_table)

# Mark the closest five Christmas markets with a distinct color
closest_markets <- christmas_markets_berlin[which(christmas_markets_berlin$id %in% distance_table$id), ]

# Plot the Berlin districts colored by market density and include points and names
ggplot(data = berlin_districts) +
  geom_sf(aes(fill = Market_Density_per_1000_residents), color = "white", size = 0.4, alpha = 0.8) +  # District borders in white
  scale_fill_gradient(low = "#9AE89E", high = "#01402E", name = "Market Density\n(per 1,000 residents)") +  # Green gradient for density
  # Plot each Christmas market as a red point
  geom_sf(data = christmas_markets_berlin, aes(geometry = geometry), color = "#D94854", size = 2, shape = 21, fill = "#D94854") +
  # Plot the closest Christmas markets as blue points
  geom_sf(data = closest_markets, aes(geometry = geometry), color = "gold", size = 3, shape = 21, fill = "gold") +
  # Plot the Hertie School Point in yellow
  geom_sf(data = hertie_point, 
          aes(geometry = geometry), 
          color = "#F2D94A", 
          size = 3, 
          shape = 23, 
          fill = "#F2D94A") +
  geom_text(data = hertie_point, 
            aes(x = st_coordinates(geometry)[1], 
                y = st_coordinates(geometry)[2] + 0.001, 
                label = name), 
            size = 3,  
            color = "white", 
            fontface = "bold") +
  # Add district names at centroids
  geom_text(data = berlin_districts_centroids, 
            aes(x = st_coordinates(geometry)[, 1], 
                y = st_coordinates(geometry)[, 2], 
                label = split_name), 
            size = 3,  
            color = "white", 
            fontface = "bold",
            lineheight = 0.8) + 
  labs(title = "Berlin Districts with Christmas Markets Density",
       subtitle = "Number of Christmas Markets per 1,000 Residents",
       caption = "Data Source: Open Data Berlin & Wikipedia") +
  theme_minimal() +
  theme(axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        panel.grid = element_blank()) 
