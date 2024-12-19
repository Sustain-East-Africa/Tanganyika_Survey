#locations
library(mapview)
library(sf)
library(dplyr)
library(readxl)
library(readr)
library(leaflet)
library(tidyr)
library(sea)
library(viridis)

SEA_palette <- c(
  "#D4A373", # Earthy Beige
  "#B57F50", # Warm Terracotta
  "#705D4D", # Deep Taupe
  "#9E805E", # Golden Brown
  "#A7B77D", # Olive Green
  "#6A8A65", # Moss Green
  "#468D7E", # Deep Teal
  "#9DB1AA", # Muted Seafoam
  "#D9C8A1", # Warm Sand
  "#CDA275", # Camel
  "#8C7C62", # Muted Olive
  "#7B9BA6", # Slate Blue
  "#8E99B3", # Dusky Periwinkle
  "#5C7183", # Steel Blue
  "#364F6B", # Deep Slate Blue
  "#778472", # Muted Sage Green
  "#A5A58D", # Khaki
  "#C9CBA3", # Soft Willow Green
  "#DBC2CF", # Dusty Rose
  "#9C6644", # Reddish Brown
  "#5E503F"  # Dark Earth Brown
)

###########################################################################################

# prepare data -----------------------------------------------------------
hhs <- readRDS("LTP_Baseline_2024_Clean.rds")

#individual locations
hhs_locations <- readRDS("LTP_Baseline_2024_Clean.rds") %>% 
  select("locat", "locatn_lat", "locatn_long", "interviewer_name", "village", "livelihood_activities", "hh_members") %>% 
  rename("y" = "locatn_lat", "x" = "locatn_long") %>%
  filter(!is.na(x) & !is.na(y)) %>%
  st_as_sf(coords = c("x", "y"), crs = 4326)

###########################################################################################

# sampling by village -----------------------------------------------------------

hhs_locations_village <- readRDS("LTP_Baseline_2024_Clean.rds") %>% 
  select("locat", "locatn_lat", "locatn_long", "interviewer_name", "village", "livelihood_activities", "hh_members") %>% 
  rename("y" = "locatn_lat", "x" = "locatn_long")

hhs_location1 <- hhs_locations_village %>%
  group_by(village) %>%
  summarize(num_samples = n())
hhs_location2 <- hhs_locations_village %>%
  group_by(village) %>%
  summarize(
    mean_latitude = mean(y, na.rm = TRUE),
    mean_longitude = mean(x, na.rm = TRUE))
hhs_location <- merge(hhs_location1, hhs_location2, by = 'village')

map <- leaflet(data = hhs_location) %>%
  addTiles()

map <- map %>%
  addCircleMarkers(
    lat = ~mean_latitude,
    lng = ~mean_longitude,
    radius = ~sqrt(num_samples)*3,
    color = ~SEA_palette,
    fillOpacity = 0.7
  ) %>%
  addLegend("bottomright", 
            colors = SEA_palette,
            labels = unique(hhs_location$village),
            opacity = 1)

print(map)
mapshot(map, file = "img/village_sample_size_map.png")