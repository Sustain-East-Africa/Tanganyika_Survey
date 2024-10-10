library(tidyverse)
library(lubridate)
library(openxlsx)
library(readxl)
library(scales)
library(googlesheets4)
library(stats)
library(stats4)
library(survey)
library(srvyr, warn.conflicts = FALSE)
library(sjPlot)
library(cowplot)
library(egg)

rm(list=ls())

tanganyika <- read_excel("tanganyika_survey_report/tnc_tanganyika_survey_form.xlsx", sheet = "sample submissions")

# Set survey design
tanganyika_survey <- as_survey_design(tanganyika)

# Convert to date 
tanganyika <- tanganyika %>%
  mutate(`START TIME` = ymd_hms(`START TIME`),`End Time` = ymd_hms(`End Time`))

# Extract date and time components
# tanganyika <- tanganyika %>%
#   mutate(
#     date = as.Date(`START TIME`),
#     start_time = format(`START TIME`, "%H:%M:%S"),
#     end_time = format(`End Time`, "%H:%M:%S")) %>%
#   select(date, start_time, end_time, everything()) 

# Filter data based on FPIC agreement
tanganyika <- tanganyika %>%
  filter(`FPIC STATEMENT (AGREED OR REFUSED) May we proceed with the interview?` == "AGREED")

### Dividing the data frame into manageable chunks (per section) 

# Household Roster and General Information
hh <- tanganyika %>% select(1:15)

# Water, Toilet, Assets, House Information
hh_info <- tanganyika %>% select(16:19, 29:33, 43:48)

# Function to escape special characters in the options
escape_special_chars <- function(options) {
  str_replace_all(options, "([\\(\\)\\[\\]\\{\\}\\+\\*\\.\\^\\$\\|])", "\\\\\\1")}

# List of water treatment options
water_treatment_options <- list("BOIL", "ADD BLEACH/CHLORINE", "STRAIN THROUGH A CLOTH", "USE WATER FILTER (CERAMIC/SAND/COMPOSITE/ETC.)", "SOLAR DISINFECTION", "LET IT STAND AND SETTLE",
                                "OTHER", "I DO NOT WANT TO ANSWER", "I DON'T KNOW")

# Escaping special characters in the options
escaped_options <- escape_special_chars(water_treatment_options)

# Function to separate column options
separate_options <- function(column, escaped_options) {
  pattern <- str_c(escaped_options, collapse = "|")
  separated <- str_replace_all(column, pattern, function(x) paste0("|", x))
  separated <- str_remove(separated, "^\\|")
  return(separated)}

# Applying the function to the relevant columns
hh_info <- hh_info %>%
  mutate(across(
    c(`16. What do you usually do to make the water safer to drink in the dry season?`, 
      `19. What do you usually do to make the water safer to drink in the wet season?`), 
    ~ separate_options(., escaped_options)))

# List of toilet facility options
toilet_facility_options <- list("VENTILATED IMPROVED PIT (VIP) LATRINE", "PIT LATRINE WITH SLAB", "PIT LATRINE WITHOUT SLAB/OPEN PIT",
  "FLUSH/ POUR FLUSH TO PIPED SEWER SYSTEM", "FLUSH/ POUR FLUSH TO PIT LATRINE", "FLUSH/ POUR FLUSH TO ELSEWHERE",
  "COMPOSTING TOILET/ECOSAN", "BUCKET", "NO FACILITY/BUSH/FIELD", "DID NOT GRANT PERMISSION", "OTHER", "I DO NOT WANT TO ANSWER", "I DON'T KNOW")

# Escape special characters in the toilet facility options
escaped_toilet_facility_options <- escape_special_chars(toilet_facility_options)

# Function to separate column options
separate_options_toilet <- function(column, escaped_options) {
  pattern <- str_c(escaped_options, collapse = "|")
  separated <- str_replace_all(column, pattern, function(x) paste0("|", x))
  separated <- str_remove(separated, "^\\|")
  return(separated)}

# Applying the function to the relevant toilet facility column(s)
hh_info <- hh_info %>%
  mutate(across(c(`20. What type of toilet facility does your household use?`),  
    ~ separate_options_toilet(., escaped_toilet_facility_options)))

# List of handwashing facility options
handwashing_facility_options <- list("HAS WATER","HAS SOAP","HAS SAND","HAS ASH","HAS TIPPY TAP","I DO NOT WANT TO ANSWER","I DON'T KNOW")
escaped_handwashing_facility_options <- escape_special_chars(handwashing_facility_options)

# Function to separate column options
separate_options_handwashing <- function(column, escaped_options) {
  pattern <- str_c(escaped_options, collapse = "|")
  separated <- str_replace_all(column, pattern, function(x) paste0("|", x))
  separated <- str_remove(separated, "^\\|")
  return(separated)}

# Applying the function to the relevant handwashing facility column(s)
hh_info <- hh_info %>%
  mutate(across(
    c(`22b)Can you please show me where/how members of your household most often wash their hands?`),
    ~ separate_options_handwashing(., escaped_handwashing_facility_options)))

# Household Items Questions
hh_items <- tanganyika %>% select(56:79)
hh_items <- hh_items %>%
  unite("combined_response", `Tanesco Power`:`A solar panel`, sep = "|", remove = FALSE)

hh_items_long <- hh_items %>%
  pivot_longer(cols = c("Tanesco Power","A radio","A television","A normal mobile phone","A working smartphone","An iron",
                        "A refrigerator","A generator","A clock","A bed or mattress","A settee or sofa","A solar panel"), 
               names_to = "Option", values_to = "Response")
hh_items_long <- hh_items_long %>%
  relocate(Option, Response, .after = combined_response)

# Household Assets
hh_assets <- tanganyika %>% select(80:86)

# Combine hh_assets options into one column
hh_assets <- hh_assets %>%
  unite("combined_response", Bicycle:`Boat with motor`, sep = "|", remove = FALSE)

hh_assets_long <- hh_assets %>%
  pivot_longer(cols = c("Bicycle", "Motorbike", "Car/truck", "Canoe/boat without motor", "Boat with motor"), 
               names_to = "Option", 
               values_to = "Response")
# Sample plot
ggplot(hh_assets_long, aes(x = Option, fill = Response)) +
  geom_bar(position = "dodge") +
  labs(title = "Does any member of your household own:", x = "Options", y = "Count") +
  theme_minimal()

### Livelihoods and Credit 
lh <- tanganyika %>% select(87, 101:116, 126:127, 141:145)

# Consumption and Food Security
food <- tanganyika %>% select(146:153, 169:174)

# Governance and Participation
gov <- tanganyika %>% select(175:184, 193:194, 204:205, 220:222)

# BMU questions
BMU <- tanganyika %>% select(223:247, 255:260)

# Fishing Section
fishing <- tanganyika %>% select(263:277)

# Livelihood Practices of Fishers per Village
fish_village <- tanganyika %>% select(280, 289, 304:395)

# Livelihood Practices of Fish Traders
fish_traders <- tanganyika %>% select(398:481)

# Livelihood Practices of Fish Processors
fish_processors <- tanganyika %>% select(484, 494:495, 510:560)


