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

################################################################################

### Preparing the survey demographic data frame ###

# Read excel file
survey_demo <- read_excel("LTP_Baseline_2024_Raw.xlsx", sheet = "repeat_detail") %>%
  rename_with(~ c("code", "relationship_hh_head", "sex", "age", "attending_school", "education_level", 
                  "marital_status", "activity", "activity_reviewed", "other_activity", "fishing"), .cols = 1:11) %>% #Renames the first 11 columns of the repeat_details sheet
  mutate(code = toupper(gsub("\\s+", "", gsub("\\.0$", "", code))),        # Cleans the household IDs in the "code" column by removing ".0" and spacing as well as changing to upper case
         code = gsub("[\r\n]", "", code),  
         code = case_when(
               code == "759D" & sex == "FEMALE" ~ "759E",  # Change Female "759D" to "759E" 
               code == "658E658D" ~ "658E", 
               code == "830" & relationship_hh_head == "SON OR DAUGHTER" ~ "830D",
               code == "458M" ~ "458N",
               code == "458L" ~ "458M",
               code == "458" & relationship_hh_head == "GRANDCHILD" ~ "458L", # Reordering family member in 458 due to Grandchild 458L having been labelled as 458 initially
               code == "403403A403B403C" ~ "403",
               code == "188" & relationship_hh_head == "WIFE OR HUSBAND" ~ "188A",  
               code == "188" & relationship_hh_head == "SON OR DAUGHTER" ~ "188B", # Reordering household 188 correctly
               code == "12300C" ~ "1230C",
               code == "903A" ~ "903",
               code == "903B" ~ "903A",
               code == "903C" ~ "903B",
               code == "903D" ~ "903C",
               code == "903E" ~ "903D",
               code == "905A" & education_level == "STANDARD 7" ~ "895A",
               code == "985B" ~ "895B",
               code == "113" & relationship_hh_head == "BROTHER/SISTER" ~ "113F",
               code == "9E" & age == 10 ~ "9F",
               code == "371E" ~ "271E",
               code == "4" ~ "1245F",
               code == "1245F" ~ "1245G",
               code == "1245G" ~ "1245H",
               code == "1484" & age == 34 ~ "1494",
               code == "1725" & relationship_hh_head == "SON OR DAUGHTER" ~ "1725F",
               code == "1442" & age == 26 ~ "1542",
               code == "2384D" ~ "1384D",
               code == "1428E" ~ "1384E",
               code == "1382B" ~ "1382C",
               code == "1382" & age == 16 ~ "1382B",
               code == "3124" & age == 39 ~ "3124A",
               code == "3124A" & age == 20 ~ "3124B",
               code == "3124" & age == 15 ~ "3124C",
               code == "3124C" & age == 12 ~ "3124D",
               code == "3124D" & age == 10 ~ "3124E",
               code == "3124E" & age == 7 ~ "3124F",
               code == "3124F" & age == 3 ~ "3124G",
               code == "2993" & age == 7 ~ "2993E",
               code == "2733" & age == 6 ~ "2733E",
               code == "2" ~ "2747",
               code == "3" ~ "2246",
               code == "4455G" ~ "3455G",
               code == "1000A" ~ "3006A",
               code == "2180C" & age == 4 ~ "2180D",
               code == "8" ~ "2181H",
               code == "3160A" & age == 1 ~ "3160F",
               code == "3216E" ~ "3216D",
               code == "4145E" ~ "4541E",
               code == "9" ~ "4941",
               code == "4847A" & age == 17 ~ "4847B",
               code == "4847B" & age == 15 ~ "4847C",
               code == "4847C" & age == 13 ~ "4847D",
               code == "4847D" & age == 11 ~ "4847E",
               code == "4847E" & age == 7 ~ "4847F",
               code == "4847F" & age == 5 ~ "4847G",
               code == "4847G" & age == 3 ~ "4847H",
               code == "4411D" & age == 11 ~ "4411E",
               code == "4411E" & age == 7 ~ "4411F",
               code == "4417E" & age == 7 ~ "4417F",
               code == "4417F" & age == 4 ~ "4417G",
               code == "4328" ~ "4328A",
               code == "4328B" ~ "4328",
               code == "4328B" & age == 20 ~ "4328B",
               code == "4328D" & age == 9 ~ "4328E",
               code == "4328E" & age == 6 ~ "4328F",
               code == "4718" & age == 23 ~ "4718A",
               code == "4718A" & age == 33 ~ "4718",
               code == "4719" & age == 20 ~ "4719A",
               code == "4719A" & age == 21 ~ "4719",
               code == "4719C" & age == 12 ~ "4719B",
               code == "4039" & age == 38 ~ "4039A",
               code == "4039A" & age == 41 ~ "4039",
               code == "3933B" & age == 4 ~ "3933C",
               code == "1934C" ~ "3914C",
               code == "5160D" & age == 0 ~ "5160D",
               code == "7700" & age == 40 ~ "7700A",
               code == "7700A" & age == 45 ~ "7700",
               code == "7761" ~ "7716A",
               code == "7716B" & age == 35 ~ "7716",
               code == "7716A" & age == 30 ~ "7716B",
               code == "7092E" & age == 2 ~ "7092F",
               code == "7490B" & age == 17 ~ "7490F",
               code == "7490C" & age == 10 ~ "7490B",
               code == "7490D" & age == 8 ~ "7490C",
               code == "7490E" & age == 2 ~ "7490D",
               code == "7490F" & age == 0 ~ "7490E",
               code == "6023B" & age == 19 ~ "6023C",
               code == "6023C" & age == 14 ~ "6023B",
               code == "7395C," ~ "7395C",
               code == "6480," ~ "6480",
               code == "7173B" & age == 13 ~ "7173C",
               code == "7173C" & age == 11 ~ "7173D",
               code == "7280A" & age == 17 ~ "7283A",
               code == "7280B" & age == 12 ~ "7283B",
               code == "7280C" & age == 10 ~ "7283C",
               code == "7280D" & age == 8 ~ "7283D",
               code == "5642F" & age == 10 ~ "5642G",
               code == "5294E" & age == 0 ~ "5294G",
               code == "5847" & age == 22 ~ "5847A",
               code == "6840" & age == 20 ~ "6840A", 
               code == "6840A" & age == 67 ~ "6840", # appending correct ID to household head label
               code == "6475" & age == 26 ~ "6475A", 
               code == "6475A" & age == 30 ~ "6475",
               code == "6470" & age == 50 ~ "6470A", 
               code == "6470A" & age == 55 ~ "6470",
               code == "6474" & age == 19 ~ "6474A", 
               code == "6474A" & age == 20 ~ "6474",
               code == "6480" & age == 65 ~ "6480A", 
               code == "6480A" & age == 68 ~ "6480",
               TRUE ~ code),
    relationship_hh_head = case_when(code == "903" ~ "HEAD OF THE HOUSEHOLD", # appending household head label to correct ID
                                     code == "3245" ~ "HEAD OF THE HOUSEHOLD",
                                     code == "2205B" ~ "SON OR DAUGHTER", # son was labelled as husband or wife
                                     code == "3845C" ~ "SON OR DAUGHTER", # Daughter was mislabeled as wife 
                                     code == "4537B" ~ "OTHER RELATIVE", # 45 yr old divorced female mislabeled as son or daughter
                                     code == "3381D" ~ "SON OR DAUGHTER", # 14 year old female was mislabeled as a parent amongst other siblings
                                     code == "3381E" ~ "SON OR DAUGHTER", # 10 year old male was mislabeled as a parent amongst other siblings
             TRUE ~ relationship_hh_head),
    sex = case_when(code == "3258A" ~ "FEMALE", # Partner of male head of household was mislabeled as male
                    code == "6761A" ~ "FEMALE", # Wife of the head of household was mislabeled as male
                    TRUE ~ sex),
    fishing = case_when(code == "658A" ~ "NO", # Household member listed fish processing, not fishing
                        code == "571" ~ "NO", # Household member listed fish trading, not fishing
                        code == "571A" ~ "NO", # Household member listed fish trading, not fishing
                        code == "704" ~ "NO", # Household member listed fish trading, not fishing
                        code == "704A" ~ "NO", # Household member listed fish trading, not fishing
                        code == "730A" ~ "NO", # Household member listed fish trading, not fishing
                        code == "528" ~ "NO", # Household member listed fish trading, not fishing
                        code == "135" ~ "NO", # Household member listed fish trading, not fishing
                        code == "895" ~ "NO", # Household member listed fish trading, not fishing
                        code == "905A" ~ "NO", # Household member listed fish processing, not fishing
                        code == "625" ~ "YES", # Household member listed fishing
                        code == "1063" ~ "NO", # Household member not involved in fishing
                        code == "1923" ~ "YES", # Household member listed fishing
                        code == "1591" ~ "YES", # Household member listed fishing
                        code == "1287" ~ "NO", # Household member listed fish trading, not fishing
                        code == "1953F" ~ "NO", # Household member not involved in fishing
                        code == "1968" ~ "NO", # Household member not involved in fishing
                        code == "2379" ~ "NO", # Household member not involved in fishing
                        code == "2535" ~ "NO", # Household member not involved in fishing
                        code == "2834" ~ "NO", # Household member not involved in fishing
                        code == "3350" ~ "YES", # Household member listed fishing
                        code == "338A" ~ "YES", # Household member listed fishing
                        code == "338F" ~ "YES", # Household member listed fishing
                        code == "332A" ~ "YES", # Household member listed fishing
                        code == "3359B" ~ "YES", # Household member listed fishing
                        code == "2267A" ~ "YES", # Household member listed fishing
                        code == "2267C" ~ "YES", # Household member listed fishing
                        code == "4531A" ~ "YES", # Household member listed fishing
                        code == "4532A" ~ "YES", # Household member listed fishing
                        code == "4533A" ~ "YES", # Household member listed fishing
                        TRUE ~ fishing),
    relationship_hh_head = if_else(code == "2834A", "WIFE OR HUSBAND", relationship_hh_head), sex = if_else(code == "2834A", "FEMALE", sex), # Wife was initially labelled as male household head with ID 2834A
    relationship_hh_head = if_else(relationship_hh_head == "CO-WIFE", "WIFE OR HUSBAND", relationship_hh_head)) %>% # change all instances of "co-wife to "wife or husband"
  mutate(main_activity = if_else(!is.na(activity_reviewed), activity_reviewed, coalesce(activity, activity_reviewed))) %>%
  select(-activity, -activity_reviewed) %>%  # Remove the activity and activity_reviewed columns after replacing with the corrected "main_activity" column that accounts for translation error
  relocate(main_activity, .after = marital_status) %>% # move main_activity column to correct position 
         filter(!(code == "3272" & relationship_hh_head == "PARENT"))
         
saveRDS(survey_demo, "survey_demo.rds")

################################################################################

### Preparing the survey submissions data frame ###

# Read excel file
tanganyika <- read_excel("LTP_Baseline_2024_Raw.xlsx", sheet = "TNC Tanganyika - Main Questionn")

# Edit date, start time, and end time 
tanganyika <- tanganyika %>%
  mutate(`START TIME...1` = as.Date(`START TIME...1`),
         `End Time` = as.Date(`End Time`),
         date = `START TIME...1`,
         start_time = as.character(date),
         end_time = as.character(date)) %>%
  select(date, start_time, end_time, everything()) %>%
  select(-`START TIME...1`, 
         -`End Time`, 
         -`ENUMERATOR TO READ OUT THE INTRODUCTION SHEET`, 
         -`There are no right or wrong answers to questions; we are just interested in getting the true information about your household and your views. If you do not wish to proceed, please tell us why you have refused.`) %>%
  # Filter for agreed responses
  filter(`FPIC STATEMENT (AGREED OR REFUSED)
 
 May we proceed with the interview?` == "AGREED")

## Addressing data validation issues and incorrect submission entries

tanganyika <- tanganyika %>%
  rename(code = colnames(tanganyika)[12]) %>%
  mutate(
    code = toupper(gsub("\\s+", "", gsub("\\.0$", "", code))),  
    code = gsub("[\r\n]", "", code),
    `31. Could you indicate all the different activities that members of the household engage in to obtain food or cash income for the household, including remittances and pensions?/TAILOR` = if_else(`HOUSEHOLD ID CODE` == 3124, 1, `31. Could you indicate all the different activities that members of the household engage in to obtain food or cash income for the household, including remittances and pensions?/TAILOR`), # 3124A mentioned being a tailor in household roster, this was not accounted for in Q.31
    `31. Could you indicate all the different activities that members of the household engage in to obtain food or cash income for the household, including remittances and pensions?/OTHER` = if_else(`HOUSEHOLD ID CODE` == 3024, 1, `31. Could you indicate all the different activities that members of the household engage in to obtain food or cash income for the household, including remittances and pensions?/OTHER`), # 3024B mentioned being a carpenter in household roster, this was not accounted for in Q.31
    `31a) You selected 'Other', please specify` = if_else(`HOUSEHOLD ID CODE` == 3024, "Fundi selemara", `31a) You selected 'Other', please specify`),
    `HOUSEHOLD ID CODE` = case_when(`HOUSEHOLD ID CODE` == "4000" & `Please help me with the number of the people who normally sleep and eat their meals together in this household, starting with the household head, then the immediate family and then the extended family and other household members.` == 1 ~ "3948",
                                    `HOUSEHOLD ID CODE` == "1542" ~ "3216",
                                    `HOUSEHOLD ID CODE` == "3598" ~ "3579",
                                    `HOUSEHOLD ID CODE` == "6678" ~ "5697",
                                    TRUE ~ as.character(`HOUSEHOLD ID CODE`)),
      code = case_when(code == "1475" & `HOUSEHOLD ID CODE` == "1479" ~ "1479", # submitted wrong household ID code
                       code == "15641564A1564B1564C" ~ "1564", # submitted multiple household ID codes
                       code == "3295" ~ "1559", # submitted wrong household ID code
                       code == "3038" ~ "2038", # submitted wrong household ID code
                       code == "3490" ~ "3480", # submitted wrong household ID code
                       code == "3034" ~ "3430", # submitted wrong household ID code
                       code == "33053305A3305B3305C3305D3305E3305F" ~ "3305", # submitted all household members instead of one
                       code == "30063006A3006B3006C3006D3006E3006F" ~ "3006", # submitted all household members instead of one
                       code == "7280" ~ "7283", # submitted wrong household ID code
                       TRUE ~ code),
    `Please help me with the number of the people who normally sleep and eat their meals together in this household, starting with the household head, then the immediate family and then the extended family and other household members.` = case_when(`Please help me with the number of the people who normally sleep and eat their meals together in this household, starting with the household head, then the immediate family and then the extended family and other household members.` == 5 & `HOUSEHOLD ID CODE` == "3803" ~ 6, # submitted wrong numbe rof people in hosuehold
                                                                                                                                                                                                                                                        `Please help me with the number of the people who normally sleep and eat their meals together in this household, starting with the household head, then the immediate family and then the extended family and other household members.` == 4 & `HOUSEHOLD ID CODE` == "7991" ~ 5, # submitted wrong numbe rof people in hosuehold
                                                                                                                                                                                                                                                        TRUE ~ `Please help me with the number of the people who normally sleep and eat their meals together in this household, starting with the household head, then the immediate family and then the extended family and other household members.`))

################################################################################

### Dividing the data frame into manageable chunks (per section) and cleaning these ###
## Household Roster and General Information ##
hh <- tanganyika %>% select(1, 4:10, 12:14)
hh <- hh %>% rename_with(~ c("date", "interviewer_name", 
                             "supervisor_name", "hh_code", "village", "sub_village", 
                             "FPIC", "hh_members", "respondent_code", "born_ward", 
                             "years_lived"))

## Water, Toilet, Assets, House Information ##
hh_info <- tanganyika %>% select(15:18, 28:32, 42:47)
hh_info <- hh_info %>% rename_with(~ c("drinking_water_dry", "other_drinking_water_dry", "water_treatment_dry", "treatment_method_dry", "other_water_treatment_dry", "drinking_water_wet", 
                                       "other_drinking_water_wet", "water_treatment_wet", "treatment_method_wet", "other_treatment_method_wet", "toilet_facilities", "other_toilet_facilities", 
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

################################################################################

## Household Items Questions ##
hh_items <- tanganyika %>% select(57:69)
hh_items <- hh_items %>% rename_with(~ c("tanesco_power","radio", "television","mobile_phone", "smartphone", "iron", "refrigerator", "generator", "clock", "bed_mattress", "sofa", "table", "solar_panel"))
hh_items <- hh_items %>%
  rowwise() %>%
  mutate(household_item = paste(names(hh_items)[c_across(everything()) == "YES"], collapse = "|")) %>%
  ungroup()
hh_items <- hh_items %>%
  select(household_item, everything())

################################################################################

## PPI Food Questions ##
ppi <- tanganyika %>% select(72:75)
ppi <- ppi %>% rename_with(~ c("beef", "milk", "rice", "flour"))

# ppi <- ppi %>%
#   rowwise() %>% mutate(ppi_food = paste(
#     names(ppi)[c_across(everything()) == "YES"],collapse = "|")) %>% ungroup() %>% select(ppi_food)

## Fuel, Floor, and Wall Materials Questions
house <- tanganyika %>% select(76:85)
house <- house %>% rename_with(~ c("cooking_fuel", "other_cooking_fuel", "efficient_stove", "efficient_stove_use", "floor_material", "other_floor_material", 
                                   "wall_material", "other_wall_material", "roof_material", "other_roof_material"))

################################################################################

## Household Assets ##
hh_assets <- tanganyika %>% select(88:92)
hh_assets <- hh_assets %>%
  rowwise() %>% mutate(household_assets = paste(
    names(hh_assets)[c_across(everything()) == "YES"],collapse = "|")) %>% ungroup() %>% select(household_assets)

################################################################################

## Livelihoods and Credit ##
lh <- tanganyika %>% select(6, 93, 109:124, 134:135, 149:153)
lh <- lh %>% rename_with(~ c("hh_code","livelihood_activities", "other_livelihood", "lh_ranking", "fishing", "trading", "processing", "agriculture", 
                             "livestock", "business", "labour", "employee", "pension", "remittance", "other_lh", "household_ability", "borrow_status",
                             "loan_usage", "other_loan_usage", "borrowing_source", "other_borrowing_source", "not_borrowed", "not_borrowed_other", "cocoba_saccos", "mobile_money")) %>%
                         mutate(trading = case_when(trading == 2 & hh_code == "4188" ~ 1, TRUE ~ trading),
                                business = case_when(business == 3 & hh_code == "4188" ~ 2, TRUE ~ business), # Append ranking for household ID 4188 
                                other_livelihood = if_else(hh_code == "483", NA_character_, other_livelihood),
                                other_lh = if_else(hh_code == "483", NA_real_, other_lh),
                                employee = if_else(hh_code == "483", 3, employee),
                                livelihood_activities = if_else(hh_code == "483", "FISHING AGRICULTURE EMPLOYEE", livelihood_activities)) %>% select(-1) # Append ranking for household ID 438 

# 31. livelihood_activities
livelihood_options <- list("FISHING","FISH TRADING","FISH PROCESSING","AGRICULTURE","LIVESTOCK KEEPING","BUSINESS", "BOAT BUILDER",
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

################################################################################

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

################################################################################

# Governance and Participation #
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

################################################################################

# BMU questions
BMU <- tanganyika %>% select(232:256, 265:270)
BMU <- BMU %>% rename_with(~ c("awareness_bmu", "bmu_member","tnc_support_bmu", "agency_support", "agency_other", "bmu_women", "bmu_youth", "bmu_activity", "meetings", "patrolling", 
                               "illegal_fishing", "illegal_gears", "raise_awareness", "collect_fees", "engage_activities", "forms_revenue", "forms_other", "collect_data", "other_activity",
                               "other_specify_bmu", "good_leaders", "leaders_elected", "trust_collaboration", "conflcit_interest", "conflict_resolution", "resolution_other", "bmu_bylaws", 
                               "bylaws_followed", "bmu_practice", "bmu_challenges", "bmu_improved"))

# List of resolution options
resolution_options <- list("GO TO VILLAGE GOVERNMENT","NEGOTIATE WITH EACH OTHER","DO NOTHING",
                           "GO TO WARD OR DISTRICT GOVERNMENT","OTHER","I DO NOT WANT TO ANSWER","I DON'T KNOW")
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

################################################################################

# Fishing Section
fishing <- tanganyika %>% select(272:289, 298, 312)
fishing <- fishing %>% rename_with(~ c("fisher_present", "fisher_code", "fisheries_resources", "rights_access", "security_rights", "relationship_officer", "current_problems", 
                                       "decision_making", "participation_description", "satisfaction_involvement", "awareness_reserves", "purpose_reserves", "opinion_reserves", 
                                       "opinion_reason", "sustainability_population", "sufficiency_fish", "sufficiency_reasons", "boat_type", "fishing_gear", "gear_other")) %>%
  mutate(fisher_code = toupper(gsub("\\s+", "", gsub("\\.0$", "", fisher_code))),        # Cleans the household IDs in the "code" column by removing ".0" and spacing as well as changing to upper case
         fisher_code = gsub("[\r\n]", "", fisher_code),  
         fisher_code = case_when(
           fisher_code == "7728" ~ "7302", 
           fisher_code == "1384" ~ "1382", 
           fisher_code == "1.5316B2.5316C" ~ "5316", TRUE ~ fisher_code)) # Append correct household ID code for the fisher that's present

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

################################################################################

# Livelihood Practices of Fishers per Village (Excluding short response)
fish_village <- tanganyika %>% select(313:320, 321, 327, 333, 339, 345, 351, 357:362, 363:376, 377:387, 388, 389, 390:400, 401:404)
fish_village <- fish_village %>% rename_with(~ c("fish_importance", "dagaa_importance", "migebuka_importance", "kungura_importance", "ngege_importance", "kuhe_importance", "sangara_importance", "target_type", "dagaa_season", "migebuka_season", "kungura_season", "ngege_season", "kuhe_season", "sangara_season", "time_input", "time_comparison", "time_reason", "selling_destination", "catch_comparison", "catch_reason",
                                                 "sale_price_best", "dagaa_best", "migebuka_best", "kungura_best", "ngege_best", "kuhe_best", "sangara_best",
                                                 "sale_price_worst", "dagaa_worst", "migebuka_worst", "kungura_worst", "ngege_worst", "kuhe_worst", "sangara_worst",
                                                 "satisfaction", "satisfaction_skills", "tools_used", "catch_gained", "market_supply", "satisfaction_purchase", "satisfaction_market", "satisfaction_income", "satisfaction_capital", "business_skills", "organization_support",
                                                 "fishing_challenges", "fishing_opportunities", "bmu_helpfulness", "group_membership", "cooperative", "cocoba_group", "other_group", "other_group_specify", "tnc_support", "other_agency_support", "name_agency", "group_helpfulness", "group_challenges", "group_improvements", "activity_long_term", "activity_long_term_why", "fishing_environmental_impact"))

################################################################################

# Livelihood Practices of Fish Traders
fish_traders <- tanganyika %>% select(406:412, 413, 419, 425, 431, 437, 443, 449:462, 463:473, 474:481, 482:488, 489:491)
fish_traders <- fish_traders %>% rename_with(~ c("trader_present", "trader_code", "trading_form", "trading_form_other", "supply_chain", "fish_sell", "trade_target_type", "dagaa_trade_season", "migebuka_trade_season", "kungura_trade_season", "ngege_trade_season", "kuhe_trade_season", "sangara_trade_season", 
                                                 "trade_best", "dagaa_trade_best", "migebuka_trade_best", "kungura_trade_best", "ngege_trade_best", "kuhe_trade_best", "sangara_trade_best",
                                                 "trade_worst", "dagaa_trade_worst", "migebuka_trade_worst", "kungura_trade_worst", "ngege_trade_worst", "kuhe_trade_worst", "sangara_trade_worst",
                                                 "satisfaction_trade", "satisfaction_trade_skills", "trade_tools_used", "trade_productivity", "trade_market_supply", "satisfaction_trade_purchase", "trade_market", "trade_income", "trade_capital", "trade_business_skills", "trade_organization_support",
                                                 "trading_challenges", "trading_opportunities", "business_group_membership", "cooperative_fico", "cooperative_fico_name", "cocoba_savings", "cocoba_savings_name", "other_trading_group", "other_trading_group_name", "trading_group_helpfulness", "trading_tnc_supported", "trading_other_agency", "trading_other_agency_name", "trading_group_challenges", "trading_group_opportunities", "trading_long_term", "trading_long_term_why", "trading_environmental_impact"))

################################################################################

# Livelihood Practices of Fish Processors
fish_processors <- tanganyika %>% select(493,494, 495, 505, 506, 521, 522:528, 529:542, 543:555, 556:568, 569:571)
fish_processors <- fish_processors %>% rename_with(~ c("processor_present", "processor_code", "processing_form", "processing_form_other", "processing_equipment", "processing_equipment_other", "processing_target_type", "dagaa_process_season", "migebuka_process_season", "kungura_process_season", "ngege_process_season", "kuhe_process_season", "sangara_process_season", 
                                                 "process_best", "dagaa_process_best", "migebuka_process_best", "kungura_process_best", "ngege_process_best", "kuhe_process_best", "sangara_process_best",
                                                 "process_worst", "dagaa_process_worst", "migebuka_process_worst", "kungura_process_worst", "ngege_process_worst", "kuhe_process_worst", "sangara_process_worst",
                                                 "satisfaction_process", "satisfaction_process_skills", "process_tools_used", "process_productivity", "process_market_supply", "satisfaction_process_materials", "process_market", "process_income", "process_capital", "process_business_skills", "process_organization_support", "processing_challenges", "processing_opportunities",
                                                 "process_business_group_membership", "cooperative_fico_process", "cooperative_fico_name_process", "cocoba_savings_process", "cocoba_savings_name_process", "other_process_group", "other_process_group_name", "process_tnc_supported", "process_other_agency", "process_other_agency_name", "process_group_helpfulness", "process_group_challenges", "process_group_opportunities",  "process_long_term", "process_long_term_why", "processing_environmental_impact"))

# List of processing equipment options
processing_form_options <- list("SUNDRYING", "SALTING", "SMOKING", "FREEZING", "COLD STORAGE", "DEEP FRYING", "OTHER", "I DO NOT WANT TO ANSWER", "I DON'T KNOW")

# Escape special characters in the options
escaped_processing_form_options <- escape_special_chars(processing_form_options)

# Function to separate column options
separate_options_processing_form <- function(column, escaped_options) {
  pattern <- str_c(escaped_options, collapse = "|")
  separated <- str_replace_all(column, pattern, function(x) paste0("|", x))
  separated <- str_remove(separated, "^\\|")
  return(separated)}

# Applying the function to the relevant column(s)
fish_processors <- fish_processors %>%
  mutate(across(
    c(processing_form),  
    ~ separate_options_processing_form(., escaped_processing_form_options)))

# List of processing equipment options
processing_equipment_options <- list("TWIGS", "WIRE MESH", "NETS", "REFRIGERATOR", "SALT", "BASIN",
                                     "COOLBOX", "FIREWOOD", "CORRUGATED SHEET", "COOKING OIL", "COOKING POT",
                                     "OTHER", "I DO NOT WANT TO ANSWER", "I DON'T KNOW")

# Escape special characters in the options
escaped_processing_equipment_options <- escape_special_chars(processing_equipment_options)

# Function to separate column options
separate_options_processing_equipment <- function(column, escaped_options) {
  pattern <- str_c(escaped_options, collapse = "|")
  separated <- str_replace_all(column, pattern, function(x) paste0("|", x))
  separated <- str_remove(separated, "^\\|")
  return(separated)}

# Applying the function to the relevant column(s)
fish_processors <- fish_processors %>%
  mutate(across(
    c(processing_equipment),  
    ~ separate_options_processing_equipment(., escaped_processing_equipment_options)))

################################################################################

# Add locations
loc <- tanganyika %>% select(574:576)
loc <- loc %>% rename_with(~ c("locat", "locatn_lat", "locatn_long"))

################################################################################

tanganyika_clean <- bind_cols(hh, hh_info, hh_items, ppi, house, hh_assets, lh, food, gov,
                              BMU, fishing, fish_village, fish_traders, fish_processors, loc)

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
      village == "MWINZA" ~ "Mwinza"), village = stype) %>%
  mutate(
    fpc = case_when(
      village == "Isasa" ~ 281, #Total number of households in each village (strata)
      village == "Mtakuja" ~ 364,
      village == "Kipili" ~ 383,
      village == "Kichangani" ~ 325,
      village == "Manda Kerenge" ~ 656,
      village == "Ntanganyika" ~ 229,
      village == "Kalungu" ~ 566,
      village == "Mkinga" ~ 325,
      village == "Majengo Mapya" ~ 210,
      village == "Manda Uhuru" ~ 147,
      village == "Mpasa" ~ 795,
      village == "Kilambo cha Mkolechi" ~ 344,
      village == "Kala" ~ 358,
      village == "Tundu" ~ 301,
      village == "Wampembe" ~ 802,
      village == "Lyapinda" ~ 600,
      village == "Katenge" ~ 143,
      village == "Kizumbi" ~ 293,
      village == "Ng'anga" ~ 172,
      village == "Izinga" ~ 527,
      village == "Mwinza" ~ 356))

# Ensure duplicate entries are removed from the survey_demo data frame
survey_demo <- survey_demo[!duplicated(survey_demo$code), ]
setdiff(tanganyika_clean$hh_code, survey_demo$code)

# Merge the specific columns from survey_demo into tanganyika_clean
tanganyika_clean <- merge(
  tanganyika_clean,
  survey_demo[, c("code", "sex", "age", "education_level", "marital_status", "main_activity")],
  by.x = "hh_code",    # Column in tanganyika_clean
  by.y = "code",       # Column in survey_demo
  all.x = TRUE,       # Only retain rows with matching IDs
  all.y = FALSE        # Optional: ensures only matching rows are included (default FALSE)
)

tanganyika_clean <- tanganyika_clean[, c("date", "hh_code", "sex", "age", "education_level", "marital_status", "main_activity",
                               setdiff(names(tanganyika_clean), c("date", "hh_code", "sex", "age", "education_level", "marital_status", "main_activity")))]

saveRDS(tanganyika_clean, "LTP_Baseline_2024_Clean.rds")
write.xlsx(tanganyika_clean, "LTP_Baseline_2024_Clean.xlsx")
