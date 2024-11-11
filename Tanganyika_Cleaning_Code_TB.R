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


### Preparing the survey demographic data frame ###

# Read excel file
survey_demo <- read_excel("tanganyika_survey_report/Tanganyika_Responses.xlsx", sheet = "repeat_detail") %>%
  rename_with(~ c("code", "relationship_hh_head", "sex", "age", "attending_school", "education_level", 
                  "marital_status", "activity", "activity_reviewed", "other_activity", "fishing"), .cols = 1:11) %>%
  mutate(code = toupper(gsub("\\.0$", "", code)))

saveRDS(survey_demo, "survey_demo.rds")

### Preparing the survey submissions data frame ###

# Read excel file
tanganyika <- read_excel("tanganyika_survey_report/Tanganyika_Responses.xlsx", sheet = "TNC Tanganyika - Main Questionn")

tanganyika <- tanganyika %>%
  mutate(`START TIME` = as.Date(`START TIME`),
         `End Time` = as.Date(`End Time`),
         date = `START TIME`,
         start_time = as.character(date),
         end_time = as.character(date)) %>%
  select(date, start_time, end_time, everything()) %>%
  select(-`START TIME`, 
         -`End Time`, 
         -`ENUMERATOR TO READ OUT THE INTRODUCTION SHEET`, 
         -`There are no right or wrong answers to questions; we are just interested in getting the true information about your household and your views. If you do not wish to proceed, please tell us why you have refused.`) %>%
  # Filter for agreed responses
  filter(`FPIC STATEMENT (AGREED OR REFUSED)
 
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
BMU <- BMU %>% rename_with(~ c("awareness_bmu", "bmu_member","tnc_support_bmu", "agency_support", "agency_other", "bmu_women", "bmu_youth", "bmu_activity", "meetings", "patrolling", 
                               "illegal_fishing", "illegal_gears", "raise_awareness", "collect_fees", "engage_activities", "forms_revenue", "forms_other", "collect_data", "other_activity",
                               "other_specify_bmu", "good_leaders", "leaders_elected", "trust_collaboration", "conflcit_interest", "conflict_resolution", "resolution_other", "bmu_bylaws", 
                               "bylaws_followed", "bmu_practice", "bmu_challenges", "bmu_improved"))


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
    c(conflict_resolution),  
    ~ separate_options_resolutions(., escaped_resolution_options)))

## Fishing Section ##
fishing <- tanganyika %>% select(272:289, 298, 312)
fishing <- fishing %>% rename_with(~ c("fisher_present", "fisher_code", "fisheries_resources", "rights_access", "security_rights", "relationship_officer", "current_problems", 
                                       "decision_making", "participation_description", "satisfaction_involvement", "awareness_reserves", "purpose_reserves", "opinion_reserves", 
                                       "opinion_reason", "sustainability_population", "sufficiency_fish", "sufficiency_reasons", "boat_type", "fishing_gear", "gear_other"))


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
    c(boat_type),  
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
    c(fishing_gear),  
    ~ separate_options_fishing_gear(., escaped_fishing_gear_options)))

# Livelihood Practices of Fishers per Village (Excluding short response)
fish_village <- tanganyika %>% select(313:320, 321, 327, 333, 339, 345, 351, 357:362, 363:376, 377:387, 388, 389, 390:400, 401, 402)
fish_village <- fish_village %>% rename_with(~ c("fish_importance", "dagaa_importance", "migebuka_importance", "kungura_importance", "ngege_importance", "kuhe_importance", "sangara_importance", "target_type", "dagaa_season", "migebuka_season", "kungura_season", "ngege_season", "kuhe_season", "sangara_season", "time_input", "time_comparison", "time_reason", "selling_destination", "catch_comparison", "catch_reason",
                                                 "sale_price_best", "dagaa_best", "migebuka_best", "kungura_best", "ngege_best", "kuhe_best", "sangara_best",
                                                 "sale_price_worst", "dagaa_worst", "migembuka_worst", "kungura_worst", "ngege_worst", "kuhe_worst", "sangara_worst",
                                                 "satisfaction", "satisfaction_skills", "tools_used", "catch_gained", "market_supply", "satisfaction_purchase", "satisfaction_market", "satisfaction_income", "satisfaction_capital", "business_skills", "organization_support",
                                                 "fishing_challenges", "fishing_opportunities", "bmu_helpfulness", "group_membership", "cooperative", "cocoba_group", "other_group", "other_group_specify", "tnc_support", "other_agency_support", "name_agency", "group_helpfulness", "group_challenges", "group_improvements", "activity_long_term"))

# Livelihood Practices of Fish Traders
fish_traders <- tanganyika %>% select(406:412, 413, 419, 425, 431, 437, 443, 449:462, 463:473, 474:481, 482:486, 489)
fish_traders <- fish_traders %>% rename_with(~ c("trader_present", "trader_code", "trading_form", "trading_form_other", "supply_chain", "fish_sell", "trade_target_type", "dagaa_trade_season", "migebuka_trade_season", "kungura_trade_season", "ngege_trade_season", "kuhe_trade_season", "sangara_trade_season", 
                                                 "trade_best", "dagaa_trade_best", "migebuka_trade_best", "kungura_trade_best", "ngege_trade_best", "kuhe_trade_best", "sangara_trade_best",
                                                 "trade_worst", "dagaa_trade_worst", "migebuka_trade_worst", "kungura_trade_worst", "ngege_trade_worst", "kuhe_trade_worst", "sangara_trade_worst",
                                                 "satisfaction_trade", "satisfaction_trade_skills", "trade_tools_used", "trade_productivity", "trade_market_supply", "satisfaction_trade_purchase", "trade_market", "trade_income", "trade_capital", "trade_business_skills", "_trade_organization_support",
                                                 "trading_challenges", "trading_opportunities", "business_group_membership", "cooperative_fico", "cooperative_fico_name", "cocoba_savings", "cocoba_savings_name", "other_trading_group", "other_trading_group_name", "trading_group_helpfulness", "trading_tnc_supported", "trading_other_agency", "trading_other_agency_name", "trading_long_term"))

# Livelihood Practices of Fish Processors
fish_processors <- tanganyika %>% select(492:494, 495, 505, 506, 521, 522:528, 529:542, 543:553, 556:566, 569)
colnames(fish_processors) <- gsub("...\\d+", "", colnames(fish_processors))

################################################################################

tanganyika_clean <- bind_cols(hh, hh_info, hh_items, ppi, house, hh_assets, lh, food, gov,
                              BMU, fishing, fish_village, fish_traders, fish_processors)

tanganyika_clean <- tanganyika_clean %>% 
  mutate(
    stype = case_when(
      village == "ISASA" ~ "Isasa",
      village == "MTAKUJA" ~ "Mtakuja",
      village == "KIPILI" ~ "Kipili",
      village == "KICHANGANI" ~ "Kichangani",
      village == "MANDA KERENGE" ~ "Manda Kerenge",
      village == "NTANGANYIKA" ~ "Ntanganyika",
      village == "KALUNGU" ~ "Kalungu",
      village == "MKINGA" ~ "Mkinga",
      village == "MAJENGO MAPYA" ~ "Majengo Mapya",
      village == "MANDA UHURU" ~ "Manda Uhuru",
      village == "MPASA" ~ "Mpasa",
      village == "KILAMBO CHA MKOLECHI" ~ "Kilambo cha Mkolechi",
      village == "KALA" ~ "Kala",
      village == "TUNDU" ~ "Tundu",
      village == "WAMPEMBE" ~ "Wampembe",
      village == "LYAPINDA" ~ "Lyapinda",
      village == "KATENGE" ~ "Katenge",
      village == "KIZUMBI" ~ "Kizumbi",
      village == "NG'ANGA" ~ "Ng'anga",
      village == "IZINGA" ~ "Izinga",
      village == "MWINZA" ~ "Mwinza")) %>%
  mutate(
    fpc = case_when(
      village == "ISASA" ~ 281, #Total number of households in each village (strata)
      village == "MTAKUJA" ~ 364,
      village == "KIPILI" ~ 383,
      village == "KICHANGANI" ~ 325,
      village == "MANDA KERENGE" ~ 656,
      village == "NTANGANYIKA" ~ 229,
      village == "KALUNGU" ~ 566,
      village == "MKINGA" ~ 325,
      village == "MAJENGO MAPYA" ~ 210,
      village == "MANDA UHURU" ~ 147,
      village == "MPASA" ~ 795,
      village == "KILAMBO CHA MKOLECHI" ~ 344,
      village == "KALA" ~ 358,
      village == "TUNDU" ~ 301,
      village == "WAMPEMBE" ~ 802,
      village == "LYAPINDA" ~ 600,
      village == "KATENGE" ~ 143,
      village == "KIZUMBI" ~ 293,
      village == "NG'ANGA" ~ 172,
      village == "IZINGA" ~ 527,
      village == "MWINZA" ~ 356))

saveRDS(tanganyika_clean, "tanganyika_clean.rds")

