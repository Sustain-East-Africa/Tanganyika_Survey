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

### Preparing the data frame ###

# Read excel file (shift to google spreadsheet API?)
tanganyika <- read_excel("tanganyika_survey_report/tnc_tanganyika_survey_form.xlsx", sheet = "sample submissions")

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
  filter(`FPIC STATEMENT (AGREED OR REFUSED)
 
 May we proceed with the interview?` == "AGREED")

### Dividing the data frame into manageable chunks (per section) and cleaning these###

## Household Roster and General Information ##
hh <- tanganyika %>% select(1:15)

## Water, Toilet, Assets, House Information ##
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

## Household Items Questions ##
hh_items <- tanganyika %>% select(56:79)
hh_items <- hh_items %>%
  unite("combined_response", `Tanesco Power`:`A solar panel`, sep = "|", remove = FALSE)

hh_items_long <- hh_items %>%
  pivot_longer(cols = c("Tanesco Power","A radio","A television","A normal mobile phone","A working smartphone","An iron",
                        "A refrigerator","A generator","A clock","A bed or mattress","A settee or sofa","A solar panel"), 
               names_to = "Option", values_to = "Response")
hh_items_long <- hh_items_long %>%
  relocate(Option, Response, .after = combined_response)

## Household Assets ##
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

## Livelihoods and Credit ##
lh <- tanganyika %>% select(87, 101:116, 126:127, 141:145)

# 31. 
livelihood_options <- list("FISHING","FISH TRADING","FISH PROCESSING","AGRICULTURE","LIVESTOCK KEEPING","BUSINESS",
                           "DAY LABOR","EMPLOYEE","PENSIONS","REMITTANCES","OTHER","I DO NOT WANT TO ANSWER","I DON'T KNOW")
escaped_livelihood_options <- escape_special_chars(livelihood_options)

# Function to separate column options
separate_options_livelihood <- function(column, escaped_options) {
  pattern <- str_c(escaped_options, collapse = "|")
  separated <- str_replace_all(column, pattern, function(x) paste0("|", x))
  separated <- str_remove(separated, "^\\|")
  return(separated)}

# Applying the function to the relevant livelihood column(s)
lh <- lh %>%
  mutate(across(
    c(`31. Could you indicate all the different activities that members of the household engage in to obtain food or cash income for the household, including remittances and pensions?`),  # Replace with your actual column name
    ~ separate_options_livelihood(., escaped_livelihood_options)))

# List of expense options
loan_options <- list("FARMING","FOOD","OTHER HOUSEHOLD EXPENSES","BUSINESS","MEDICAL EXPENSES",
                     "SCHOOL FEES","OTHER","I DO NOT WANT TO ANSWER","I DON'T KNOW")

# Escape special characters in the expense options
escaped_loan_options <- escape_special_chars(loan_options)

# Function to separate column options
separate_options_loan <- function(column, escaped_options) {
  pattern <- str_c(escaped_options, collapse = "|")
  separated <- str_replace_all(column, pattern, function(x) paste0("|", x))
  separated <- str_remove(separated, "^\\|")
  return(separated)}

# Applying the function to the relevant expense column(s)
lh <- lh %>%
  mutate(across(
    c(`35. What was the money from the loans used for?`),  
    ~ separate_options_loan(., escaped_loan_options)))

# List of borrowing source options
borrowing_source_options <- list("FAMILY","FRIENDS/NEIGHBORS","TRADERS","COCOBA / SACCOS / FICOS / VIKOBA","MICROCREDIT INSTITUTION","BANK",
                                 "MPESA","AIRTEL MONEY","HALO PESA","TIGO PESA","I DO NOT WANT TO ANSWER","OTHER","I DON'T KNOW")

escaped_borrowing_source_options <- escape_special_chars(borrowing_source_options)

# Function to separate column options
separate_options_borrowing_source <- function(column, escaped_options) {
  pattern <- str_c(escaped_options, collapse = "|")
  separated <- str_replace_all(column, pattern, function(x) paste0("|", x))
  separated <- str_remove(separated, "^\\|")
  return(separated)}

# Applying the function to the relevant borrowing source column(s)
lh <- lh %>%
  mutate(across(
    c(`36. Who did you or your household members borrow from?`), 
    ~ separate_options_borrowing_source(., escaped_borrowing_source_options)))

## Consumption and Food Security ##
food <- tanganyika %>% select(146:153, 169:174)

# List of month options
month_options <- list("OCTOBER. 2024","SEPTEMBER. 2024","AUGUST. 2024","JULY. 2024","JUNE. 2024","MAY. 2024",
                      "APRIL. 2024","MARCH. 2024","FEBRUARY. 2024","JANUARY. 2024","DECEMBER. 2023","NOVEMBER. 2023",
                      "I DO NOT WANT TO ANSWER","I DON'T KNOW")
escaped_month_options <- escape_special_chars(month_options)

# Function to separate column options
separate_options_months <- function(column, escaped_options) {
  pattern <- str_c(escaped_options, collapse = "|")
  separated <- str_replace_all(column, pattern, function(x) paste0("|", x))
  separated <- str_remove(separated, "^\\|")
  return(separated)}

# Applying the function to the relevant month column(s)
food <- food %>%
  mutate(across(
    c(`46. Over the last 12 months, in which months, were the food shortages or worries about having enough food the worst?`),  
    ~ separate_options_months(., escaped_month_options)))

# Governance and Participation
gov <- tanganyika %>% select(175:184, 193:194, 204:205, 220:222)

# List of conflict options
conflict_options <- list("FISHING IN PROTECTED FISH BREEDING AREAS","USING ILLEGAL FISHING GEAR","CATCHING UNDERSIZED FISH","PRIVATE (FARM) LAND BOUNDARIES",
                         "FISHING IN THE NATIONAL PARK","OTHER","I DO NOT WANT TO ANSWER","I DON'T KNOW")
escaped_conflict_options <- escape_special_chars(conflict_options)

# Function to separate column options
separate_options_conflicts <- function(column, escaped_options) {
  pattern <- str_c(escaped_options, collapse = "|")
  separated <- str_replace_all(column, pattern, function(x) paste0("|", x))
  separated <- str_remove(separated, "^\\|")
  return(separated)}

# Applying the function to the relevant conflict column(s)
gov <- gov %>%
  mutate(across(
    c(`60. Can you describe what these disputes or conflicts are mostly about?`),  
    ~ separate_options_conflicts(., escaped_conflict_options)))

# List of interaction options
conflict_parties_options <- list("AMONG LOCAL VILLAGERS","LOCAL VILLAGERS WITH PEOPLE FROM OTHER VILLAGES","LOCAL VILLAGERS WITH GOVERNMENT",
                            "LOCAL VILLAGERS WITH IMMIGRANTS","AMONG FISHERS","BMUS WITH FISHERS","OTHER","I DO NOT WANT TO ANSWER","I DON'T KNOW")
escaped_conflict_parties_options <- escape_special_chars(conflict_parties_options)

# Function to separate column options
separate_options_conflict_parties <- function(column, escaped_options) {
  pattern <- str_c(escaped_options, collapse = "|")
  separated <- str_replace_all(column, pattern, function(x) paste0("|", x))
  separated <- str_remove(separated, "^\\|")
  return(separated)}

# Applying the function to the relevant interaction column
gov <- gov %>%
  mutate(across(
    c(`61. Among whom do these disputes or conflicts mostly occur?`),  
    ~ separate_options_conflict_parties(., escaped_conflict_parties_options)))

# BMU questions
BMU <- tanganyika %>% select(223:247, 255:260)

# List of resolution options
resolution_options <- list("GO TO THE VILLAGE GOVERNMENT","NEGOTIATE WITH EACH OTHER","DO NOTHING",
                           "GO TO THE WARD OR DISTRICT GOVERNMENT","OTHER","I DO NOT WANT TO ANSWER","I DON'T KNOW")
escaped_resolution_options <- escape_special_chars(resolution_options)

# Function to separate column options
separate_options_resolutions <- function(column, escaped_options) {
  pattern <- str_c(escaped_options, collapse = "|")
  separated <- str_replace_all(column, pattern, function(x) paste0("|", x))
  separated <- str_remove(separated, "^\\|")
  return(separated)}

# Applying the function to the relevant resolution column(s)
BMU <- BMU %>%
  mutate(across(
    c(`78. If there is a dispute or conflict between BMUs and fishers, how do people in your village try to solve it?`),  
    ~ separate_options_resolutions(., escaped_resolution_options)))

# Fishing Section
fishing <- tanganyika %>% select(263:277)

# Livelihood Practices of Fishers per Village
fish_village <- tanganyika %>% select(280, 289, 304:395)

# Livelihood Practices of Fish Traders
fish_traders <- tanganyika %>% select(398:481)

# Livelihood Practices of Fish Processors
fish_processors <- tanganyika %>% select(484, 494:495, 510:560)


