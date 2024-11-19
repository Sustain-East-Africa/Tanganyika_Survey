#locations
library(mapview)
library(sf)
library(dplyr)
library(readxl)
library(readr)
library(leaflet)
library(tidyr)
library(sea)

SEA_palette <- c("#d77e5e", "#a4b792", "#e6e7e2", "#3d5919", "#202C39", "#381D2A", "#000000",
                 "#f2a084", "#b9c9b1", "#f0f1ed", "#5b7a2e", "#404b58", "#522a3b", "#1a1a1a", 
                 "#b15e42", "#839c7a", "#d2d2c8", "#293c14", "#151d29", "#2a171e", "#3c3c3c")
###########################################################################################

# prepare data -----------------------------------------------------------
hhs <- readRDS("tanganyika_clean.rds")

#individual locations
hhs_locations <- readRDS("tanganyika_clean.rds") %>% 
  select("locat", "locatn_lat", "locatn_long", "interviewer_name", "village", "livelihood_activities", "hh_members") %>% 
  rename("y" = "locatn_lat", "x" = "locatn_long") %>%
  filter(!is.na(x) & !is.na(y)) %>%
  st_as_sf(coords = c("x", "y"), crs = 4326)

###########################################################################################

# sampling by village -----------------------------------------------------------

hhs_locations_village <- readRDS("tanganyika_clean.rds") %>% 
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