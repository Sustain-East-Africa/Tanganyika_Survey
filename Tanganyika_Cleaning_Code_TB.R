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
tanganyika <- read_excel("tanganyika_survey_report/TNC_Tanganyika_-_Main_Questionnaire_-_all_versions_-_English_-_2024-10-15-05-07-33.xlsx", sheet = "TNC Tanganyika - Main Questi...")

tanganyika <- tanganyika %>%
  mutate(`START TIME` = ymd_hms(`START TIME`),`End Time` = ymd_hms(`End Time`),
         date = as.Date(`START TIME`),
         start_time = format(`START TIME`, "%H:%M:%S"),
         end_time = format(`End Time`, "%H:%M:%S")) %>%
  select(date, start_time, end_time, everything()) %>%
  select(-`START TIME`, 
         -`End Time`, 
         -`ENUMERATOR TO READ OUT THE INTRODUCTION SHEET`, 
         -`There are no right or wrong answers to questions; we are just interested in getting the true information about your household and your views. If you do not wish to proceed, please tell us why you have refused.`) %>%
  filter(tanganyika$`FPIC STATEMENT (AGREED OR REFUSED)

May we proceed with the interview?` == "AGREED")

# Cleaning column headers
#colnames(tanganyika) <- gsub("...\\d+", "", colnames(tanganyika))

### Dividing the data frame into manageable chunks (per section) and cleaning these ###
## Household Roster and General Information ##
hh <- tanganyika %>% select(1:10, 12:14)
hh <- hh %>% rename_with(~ c("date", "start_time", "end_time", "interviewer_name", 
                             "supervisor_name", "hh_code", "village", "sub_village", 
                             "FPIC", "hh_members", "respondent_code", "born_ward", 
                             "years_lived"))

## Water, Toilet, Assets, House Information ##
hh_info <- tanganyika %>% select(15:18, 28:32, 43:47)
hh_info <- hh_info %>% rename_with(~ c("drinking_water_dry", "other_drinking_water_dry", "water_treatment_dry", "treatment_method_dry", "other_water_treatment_dry", "drinking_water_wet", 
                                       "other_drinking_water_wet", "water_treatment_wet", "treatment_method_wet", "toilet_facilities", "other_toilet_facilities", 
                                       "shared_facilities", "handwashing_place", "handwashing_show"))

# Function to escape special characters in the options
escape_special_chars <- function(options) {
  str_replace_all(options, "([\\(\\)\\[\\]\\{\\}\\+\\*\\.\\^\\$\\|])", "\\\\\\1")}

# General function to separate column options
separate_options <- function(column, options) {
  escaped_options <- escape_special_chars(options)
  pattern <- str_c(escaped_options, collapse = "|")
  separated <- str_replace_all(column, pattern, function(x) paste0("|", x))
  str_remove(separated, "^\\|")}

# List of options for different columns
water_treatment_options <- c("BOIL", "ADD BLEACH/CHLORINE", "STRAIN THROUGH A CLOTH", 
                             "USE WATER FILTER (CERAMIC/SAND/COMPOSITE/ETC.)", "SOLAR DISINFECTION", 
                             "LET IT STAND AND SETTLE", "OTHER", "I DO NOT WANT TO ANSWER", "I DON'T KNOW")

toilet_facility_options <- c("VENTILATED IMPROVED PIT (VIP) LATRINE", "PIT LATRINE WITH SLAB", 
                             "PIT LATRINE WITHOUT SLAB/OPEN PIT", "FLUSH/ POUR FLUSH TO PIPED SEWER SYSTEM", 
                             "FLUSH/ POUR FLUSH TO PIT LATRINE", "FLUSH/ POUR FLUSH TO ELSEWHERE", 
                             "COMPOSTING TOILET/ECOSAN", "BUCKET", "NO FACILITY/BUSH/FIELD", 
                             "DID NOT GRANT PERMISSION", "OTHER", "I DO NOT WANT TO ANSWER", "I DON'T KNOW")

handwashing_facility_options <- c("HAS WATER", "HAS SOAP", "HAS SAND", "HAS ASH", 
                                  "HAS TIPPY TAP", "I DO NOT WANT TO ANSWER", "I DON'T KNOW")

# Apply separation function to relevant columns
hh_info <- hh_info %>%
  mutate(across(c(treatment_method_dry, treatment_method_wet), ~ separate_options(., water_treatment_options))) %>%
  mutate(across(c(toilet_facilities), ~ separate_options(., toilet_facility_options))) %>%
  mutate(across(c(handwashing_show), ~ separate_options(., handwashing_facility_options)))

## Household Items Questions ##
hh_items <- tanganyika %>% select(57:69)
hh_items <- hh_items %>%
  rowwise() %>% mutate(household_item = paste(
    names(hh_items)[c_across(everything()) == "YES"],collapse = "|")) %>% ungroup() %>% select(household_item)

## PPI Food Questions ##
ppi <- tanganyika %>% select(72:75)
ppi <- ppi %>%
  rowwise() %>% mutate(ppi_food = paste(
    names(ppi)[c_across(everything()) == "YES"],collapse = "|")) %>% ungroup() %>% select(ppi_food)

## Fuel, Floor, and Wall Materials Questions
house <- tanganyika %>% select(76:85)
house <- house %>% rename_with(~ c("cooking_fuel", "other_fuel", "efficient_stove", 
                                   "stove_usage", "floor_material", "other_floor", 
                                   "wall_material", "other_wall", "roof_material", 
                                   "other_roof"))

## Household Assets ##
hh_assets <- tanganyika %>% select(88:92)
hh_assets <- hh_assets %>%
  rowwise() %>% mutate(household_assets = paste(
    names(hh_assets)[c_across(everything()) == "YES"],collapse = "|")) %>% ungroup() %>% select(household_assets)

## Livelihoods and Credit ##
lh <- tanganyika %>% select(93, 109:124, 134:135, 149:153)
lh <- lh %>% rename_with(~ c("livelihood_activities", "other_livelihood", "lh_ranking", "fishing", "trading", "processing", "agriculture", 
                             "livestock", "business", "labour", "employee", "pension", "remittance", "other_lh", "household_ability", "borrow_status",
                             "loan_usage", "other_loan_usage", "borrowing_source", "other_borrowing_source", "not_borrowed", "not_borrowed_other", "cocoba_saccos", "mobile_money"))

# 31. livelihood_activities
livelihood_options <- list("FISHING","FISH TRADING","FISH PROCESSING","AGRICULTURE","LIVESTOCK KEEPING","BUSINESS",
                           "DAY LABOR","EMPLOYEE","PENSIONS","REMITTANCES","OTHER", "TAILOR", "I DO NOT WANT TO ANSWER","I DON'T KNOW")
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
    c(livelihood_activities),  
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
    c(loan_usage),  
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

lh <- lh %>%
  mutate(across(c(borrowing_source), 
    ~ separate_options_borrowing_source(., escaped_borrowing_source_options)))

## Consumption and Food Security ##
food <- tanganyika %>% select(154:161, 176:181)
food <- food %>% rename_with(~ c("dagaa", "migebuka", "other_fish", "primary_source", "primary_source_other", "eat_changed", "worry_shortage", "food_shortage",
                                 "shortage_reason", "shortage_reason_other", "food_availability", "availability_changed", "availability_reason", "availability_reason_other"))


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
    c(food_shortage),  
    ~ separate_options_months(., escaped_month_options)))

## Governance and Participation ##
gov <- tanganyika %>% select(182:191, 201:202, 212:214, 229:231)
gov <- gov %>% rename_with(~ c("hh_influence", "people_trusted", "nearby_trusted", "government_trusted", "village_membership", "group_name", "meeting_attendance", "disputes_conflicts", 
                               "more_less", "conflict_reason", "conflict_other", "conflict_parties", "parties_other", "fair_resolution", "influential_leaders", "other_influential", "leader_position", "BMU_activity"))


# List of conflict options
conflict_options <- list("FISHING IN PROTECTED FISH BREEDING AREAS","USING ILLEGAL FISHING GEAR","CATCHING UNDERSIZED FISH","PRIVATE (FARM) LAND BOUNDARIES",
                        "CLOSURE OF THE LAKE CAUSING THE COMMUNITY TO BE UNABLE TO FISH","FISHING IN THE NATIONAL PARK","OTHER","I DO NOT WANT TO ANSWER","I DON'T KNOW")
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
    c(conflict_reason),  
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
    c(conflict_parties),  
    ~ separate_options_conflict_parties(., escaped_conflict_parties_options)))

# List of interaction options for leaders
leaders_options <- list("RELIGIOUS LEADERS", "POLITICAL PARTY LEADERS", "WITCHDOCTORS", "CIVIL SERVANTS", 
                        "POLITICAL LEADERS", "SPORTS LEADERS", "FISHING EQUIPMENT OWNERS", "FISHING EQUIPMENT LEADERS", 
                        "RESPECTED ELDERS", "FISHING EQUIPMENT REPAIRERS", "FARMER", "OTHER", "I DO NOT WANT TO ANSWER", "I DON'T KNOW")
escaped_leaders_options <- escape_special_chars(leaders_options)

# Function to separate column options for leaders
separate_options_leaders <- function(column, escaped_options) {
  pattern <- str_c(escaped_options, collapse = "|")
  separated <- str_replace_all(column, pattern, function(x) paste0("|", x))
  separated <- str_remove(separated, "^\\|")
  return(separated)
}

# Applying the function to the relevant interaction column for leaders
gov <- gov %>%
  mutate(across(
    c(influential_leaders),  
    ~ separate_options_leaders(., escaped_leaders_options)))

# BMU questions
BMU <- tanganyika %>% select(232:256, 265:270)
colnames(BMU) <- gsub("...\\d+", "", colnames(BMU))

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

## Fishing Section ##
fishing <- tanganyika %>% select(272:289, 298, 312)

# List of boat options
boat_options <- list("DON’T FISH FROM BOAT","CANOE WITHOUT MOTOR","LARGE BOAT WITHOUT ENGINE","LARGE BOAT WITH ENGINE",
                     "ENGINE BOAT","CANOE BOAT","I DO NOT WANT TO ANSWER","I DON'T KNOW")

# Escape special characters in the boat options
escaped_boat_options <- escape_special_chars(boat_options)

# Function to separate column options
separate_options_boats <- function(column, escaped_options) {
  pattern <- str_c(escaped_options, collapse = "|")
  separated <- str_replace_all(column, pattern, function(x) paste0("|", x))
  separated <- str_remove(separated, "^\\|")
  return(separated)}

# Applying the function to the relevant boat column(s)
fishing <- fishing %>%
  mutate(across(
    c(`99. Thinking about fishing, which boat type do you currently use, if any?`),  
    ~ separate_options_boats(., escaped_boat_options)))

# List of fishing gear options
fishing_gear_options <- list("RING NETS (PURSE-SEINE)","LONG LINE","BEACH SEINE","LIFT NETS","GILL NETS","BASKET TRAP",
                             "MONO FILAMENT","HAND NET","POLE/HANDHELD FISHING ROD","MOSQUITO NET","OTHER","I DO NOT WANT TO ANSWER","I DON'T KNOW")

# Escape special characters in the fishing gear options
escaped_fishing_gear_options <- escape_special_chars(fishing_gear_options)

# Function to separate column options
separate_options_fishing_gear <- function(column, escaped_options) {
  pattern <- str_c(escaped_options, collapse = "|")
  separated <- str_replace_all(column, pattern, function(x) paste0("|", x))
  separated <- str_remove(separated, "^\\|")
  return(separated)}

# Applying the function to the relevant fishing gear column(s)
fishing <- fishing %>%
  mutate(across(
    c(`100. Which types of fishing gear do you currently use:`),  
    ~ separate_options_fishing_gear(., escaped_fishing_gear_options)))

# Livelihood Practices of Fishers per Village (Excluding short response)
fish_village <- tanganyika %>% select(313:320, 320, 327, 333, 339, 345, 351, 357, 358, 361, 363:376, 377:387, 390:400, 402)

# 101. <i>MAELEKEZO KWA MCHUKUA TAARIFA: WAULIZE WAHOJIWA KUONYESHA UMUHIMU WA KILA SHUGHULI YA RIZIKI. TUMIA NAMBA 1 KWA SHUGHULI ILE MUHIMU ZAIDI.</i>
fish_ranked <- fish_village %>% select(2:7)

# Reshape the data from wide to long format
fish_ranked_long <- fish_ranked %>%
  pivot_longer(cols = everything(),
               names_to = "Fish_Species",
               values_to = "Importance_Rank")

# Sample plot of the importance of each fish species
ggplot(fish_ranked_long, aes(x = Fish_Species, y = Importance_Rank)) +
  geom_boxplot() + 
  labs(title = "Importance of Fish Species", 
       x = "Fish Species", 
       y = "Importance Ranking") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 102. Over the past 5 years to now, which type of fish are you targeting - and in which seasons?
fish_season <- fish_village %>% select(8:13)
fish_season_long <- fish_season %>%
  pivot_longer(cols = -1, 
               names_to = "Fish_Species",
               values_to = "Season") %>%
  filter(!is.na(Season) & Season != "")  

season_summary <- fish_season_long %>%
  group_by(Fish_Species, Season) %>%
  summarise(Count = n()) %>%
  ungroup()

# Create the plot
ggplot(season_summary, aes(x = Fish_Species, y = Count, fill = Season)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Seasonal Targeting of Fish Species",
       x = "Fish Species",
       y = "Count of Responses",
       fill = "Season") +
  theme_minimal()

# 109. Over the past years to now, what is the highest sale prices for these kinds of fish?
fish_price_best <- fish_village %>% select(17:23)
fish_price_best_long <- fish_price_best %>%
  pivot_longer(cols = -1,  
               names_to = "Fish_Species", 
               values_to = "Sale_Price") %>%
  filter(!is.na(Sale_Price) & Sale_Price != "")  

# 109. Over the past years to now, what is the lowest sale prices for these kinds of fish?
fish_price_lowest <- fish_village %>% select(24:30)
fish_price_lowest_long <- fish_price_lowest %>%
  pivot_longer(cols = -1,  
               names_to = "Fish_Species", 
               values_to = "Sale_Price") %>%
  filter(!is.na(Sale_Price) & Sale_Price != "")  

# 110. 
satisfaction <- fish_village %>% select(31:41)
satisfaction_long <- satisfaction %>%
  pivot_longer(cols = -1,  # Exclude the first empty column
               names_to = "Aspect", 
               values_to = "Satisfaction_Level") %>%
  filter(!is.na(Satisfaction_Level) & Satisfaction_Level != "I DO NOT WANT TO ANSWER")  # Remove empty or non-numeric responses

satisfaction_long <- satisfaction_long %>%
  mutate(Satisfaction_Level = factor(Satisfaction_Level,
                                     levels = c("1 VERY UNSATISFIED", 
                                                "2 UNSATISFIED", 
                                                "3 NEUTRAL", 
                                                "4 SATISFIED", 
                                                "5 VERY SATISFIED"),
                                     ordered = TRUE))

# Livelihood Practices of Fish Traders
fish_traders <- tanganyika %>% select(405:407, 408:409, 412, 413, 419, 425, 431, 437, 443, 449:462, 463:473, 476:481, 482:486, 489)
colnames(fish_traders) <- gsub("...\\d+", "", colnames(fish_traders))

# 127.
trade_season <- fish_traders %>% select(6:12)
trade_season_long <- trade_season %>%
  pivot_longer(cols = -1, 
               names_to = "Fish_Species",
               values_to = "Season") %>%
  filter(!is.na(Season) & Season != "")  

trade_summary <- fish_season_long %>%
  group_by(Fish_Species, Season) %>%
  summarise(Count = n()) %>%
  ungroup()

# 128. best
trade_price_best <- fish_traders %>% select(13:19)
trade_price_best_long <- trade_price_best %>%
  pivot_longer(cols = -1,  
               names_to = "Fish_Species", 
               values_to = "Sale_Price") %>%
  filter(!is.na(Sale_Price) & Sale_Price != "")  

# 128. lowest
trade_price_lowest <- fish_traders %>% select(20:26)
trade_price_lowest_long <- trade_price_lowest %>%
  pivot_longer(cols = -1,  
               names_to = "Fish_Species", 
               values_to = "Sale_Price") %>%
  filter(!is.na(Sale_Price) & Sale_Price != "")

# 129. trade satisfaction
trade_satisfaction <- fish_traders %>% select(27:37)
trade_satisfaction_long <- trade_satisfaction %>%
  pivot_longer(cols = -1,  # Exclude the first empty column
               names_to = "Aspect", 
               values_to = "Satisfaction_Level") %>%
  filter(!is.na(Satisfaction_Level) & Satisfaction_Level != "I DO NOT WANT TO ANSWER")  # Remove empty or non-numeric responses

trade_satisfaction_long <- trade_satisfaction_long %>%
  mutate(Satisfaction_Level = factor(Satisfaction_Level,
                                     levels = c("1 VERY UNSATISFIED", 
                                                "2 UNSATISFIED", 
                                                "3 NEUTRAL", 
                                                "4 SATISFIED", 
                                                "5 VERY SATISFIED"),
                                     ordered = TRUE))

# Livelihood Practices of Fish Processors
fish_processors <- tanganyika %>% select(492:494, 495, 505, 506, 521, 522:528, 529:542, 543:553, 556:566, 569)
colnames(fish_processors) <- gsub("...\\d+", "", colnames(fish_processors))

# 143
processing_options <- list("SUNDRYING","SALTING","SMOKING","FREEZING","COLD STORAGE","DEEP FRYING",
  "OTHER","I DO NOT WANT TO ANSWER","I DON'T KNOW")
escaped_processing_options <- escape_special_chars(processing_options)
separate_options_processing <- function(column, escaped_processing_options) {
  pattern <- str_c(escaped_processing_options, collapse = "|")
  separated <- str_replace_all(column, pattern, function(x) paste0("|", x))
  separated <- str_remove(separated, "^\\|")
  return(separated)}

# Applying the function to the relevant processing column(s)
fish_processors <- fish_processors %>%
  mutate(across(
    c(`143. What form of fish processing do you participate in?`),  
    ~ separate_options_processing(., escaped_processing_options)))


# 144
processing_equipment_options <- list("TWIGS", "WIRE MESH", "NETS", "REFRIGERATOR", "SALT", "BASIN", 
  "COOLBOX", "FIREWOOD", "CORRUGATED SHEET", "COOKING OIL", "COOKING POT", "OTHER", "I DO NOT WANT TO ANSWER", "I DON'T KNOW")

escaped_processing_equipment_options <- escape_special_chars(processing_equipment_options)
separate_options_processing_equipment <- function(column, escaped_options) {
  pattern <- str_c(escaped_options, collapse = "|")
  separated <- str_replace_all(column, pattern, function(x) paste0("|", x))
  separated <- str_remove(separated, "^\\|")
  return(separated)}

# Applying the function to the relevant processing equipment column(s)
fish_processors <- fish_processors %>%
  mutate(across(
    c(`144. What equipment and materials do you use?`),  
    ~ separate_options_processing_equipment(., escaped_processing_equipment_options)))

# 145. 
process_season <- fish_processors %>% select(8:14)
process_season_long <- process_season %>%
  pivot_longer(cols = -1, 
               names_to = "Fish_Species",
               values_to = "Season") %>%
  filter(!is.na(Season) & Season != "")  

process_season_summary <- process_season_long %>%
  group_by(Fish_Species, Season) %>%
  summarise(Count = n()) %>%
  ungroup()

# 146. highest
process_price_best <- fish_processors %>% select(15:21)
process_price_best_long <- process_price_best %>%
  pivot_longer(cols = -1,  
               names_to = "Fish_Species", 
               values_to = "Sale_Price") %>%
  filter(!is.na(Sale_Price) & Sale_Price != "")  

# 146. lowest
process_price_lowest <- fish_processors %>% select(22:28)
process_price_lowest_long <- process_price_lowest %>%
  pivot_longer(cols = -1,  
               names_to = "Fish_Species", 
               values_to = "Sale_Price") %>%
  filter(!is.na(Sale_Price) & Sale_Price != "")

# 147. 
processors_satisfaction <- fish_processors %>% select(29:39)
processors_satisfaction_long <- processors_satisfaction %>%
  pivot_longer(cols = -1,  # Exclude the first empty column
               names_to = "Aspect", 
               values_to = "Satisfaction_Level") %>%
  filter(!is.na(Satisfaction_Level) & Satisfaction_Level != "I DO NOT WANT TO ANSWER")  # Remove empty or non-numeric responses

processors_satisfaction_long <- processors_satisfaction_long %>%
  mutate(Satisfaction_Level = factor(Satisfaction_Level,
                                     levels = c("1 VERY UNSATISFIED", 
                                                "2 UNSATISFIED", 
                                                "3 NEUTRAL", 
                                                "4 SATISFIED", 
                                                "5 VERY SATISFIED"),
                                     ordered = TRUE))

################################################################################

tanganyika_clean <- bind_cols(hh, hh_info, hh_items, ppi, house, hh_assets, lh, food, gov,
                              BMU, fishing, fish_village, fish_traders, fish_processors)

tanganyika_clean <- tanganyika_clean %>% 
  mutate(
    stype = case_when(
      village == "ISASA" ~ "Isasa",
      village == "MTAKUJA" ~ "Mtakuja",
      village == "KIPILI" ~ "Kipili",
      village == "KICHANGANI" ~ "Kichangani")) %>%
  mutate(
    fpc = case_when(
      village == "ISASA" ~ 281, #Total number of households in each village (strata)
      village == "MTAKUJA" ~ 364,
      village == "KIPILI" ~ 383,
      village == "KICHANGANI" ~ 325))

saveRDS(tanganyika_clean, "tanganyika_clean.rds")

