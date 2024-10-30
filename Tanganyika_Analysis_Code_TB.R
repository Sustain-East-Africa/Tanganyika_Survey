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

ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)
rm(list=ls())

# Add Sustain East Africa colour pallette
SEA_palette <- c("#d77e5e", "#a4b792", "#e6e7e2", "#3d5919", "#202C39", "#381D2A", "#000000","#202C39", "#d77e5e")

# import cleaned Tanganyika household survey data with --------------------------------- 
tanganyika_clean <- readRDS("tanganyika_clean.rds")
head(tanganyika_clean)

######################################################################################################################
############# Survey based household information (water provision & toilet facilities)  ############
######################################################################################################################

# List of variables to plot
hh_info_to_plot <- c("born_ward","drinking_water_dry", "other_drinking_water_dry", "water_treatment_dry", 
                     "other_water_treatment_dry", "drinking_water_wet", "other_drinking_water_wet", "water_treatment_wet",
                     "toilet_facilities", "other_toilet_facilities", "shared_facilities", "handwashing_place", "cooking_fuel", 
                     "cooking_fuel", "efficient_stove", "stove_usage", "floor_material", "wall_material", "roof_material")

# Define plotting and summary table function
hh_info_survey_variable <- function(variable_name) {
  strat_design <- tanganyika_clean %>% as_survey_design(strata = stype, fpc = fpc, variables = c(stype, fpc, village, !!sym(variable_name)))
  
  # Stratify by village and variable, calculate proportions
  village_data <- strat_design %>%
    group_by(village, !!sym(variable_name)) %>%
    summarise(
      proportion = survey_mean(vartype = "ci", na.rm = TRUE), 
      n = unweighted(n()))
  
  # Save summary table
  summary_table <- village_data %>%
    select(village, !!sym(variable_name), n, proportion, proportion_low, proportion_upp) %>%
    rename(Total_Household_Responses = n, Proportion = proportion, Lower_CI = proportion_low, Upper_CI = proportion_upp)
  
  # Save summary table as CSV
  table_file_name <- here::here("images", paste0(variable_name, "_summary_table.csv"))
  write.csv(summary_table, table_file_name, row.names = FALSE)
  
  plot <- ggplot(village_data, aes(x = village, y = proportion, group = !!sym(variable_name), fill = !!sym(variable_name))) +
    geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
    geom_errorbar(aes(ymax = pmin(proportion_upp, 1), ymin = pmax(proportion_low, 0)),
                  position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
    guides(fill = guide_legend(title = NULL)) +
    scale_fill_manual(values = c("#A9CCE3",  "#2E86C1", "#F5B7B1", "#D091BB", "#BBD4A6", "#FAD7A0", "#DFDFDF", SEA_palette)) +
    labs(title = paste("Proportion of", variable_name, "by Village"), x = "Village", y = "Proportion of Households") +
    scale_y_continuous(limits = c(0, 1)) +
    theme_minimal()
 
   return(list(plot = plot, table = summary_table))}

# Generate and save plots and tables
plots_and_tables <- lapply(hh_info_to_plot, function(var) hh_info_survey_variable(var))
for (i in seq_along(hh_info_to_plot)) {
  plot_file_name <- here::here("images", paste0(hh_info_to_plot[i], ".png"))
  ggsave(filename = plot_file_name, plot = plots_and_tables[[i]]$plot, width = 10, height = 7)}

# treatment_method_dry ----------------

treatment_dry_expanded <- tanganyika_clean %>% select(hh_code, village, treatment_method_dry, stype, fpc) %>%
  separate_rows(treatment_method_dry, sep = "\\|") %>%
  mutate(treatment_method_dry = trimws(treatment_method_dry)) %>% drop_na()  

treatment_dry_design <- treatment_dry_expanded %>%
  as_survey_design(strata = stype, fpc = fpc)

treatment_data <- treatment_dry_design %>%
  group_by(village, treatment_method_dry) %>%
  summarise(proportion = survey_mean(vartype = "ci", na.rm = TRUE), n = unweighted(n())) %>% ungroup()

ggplot(treatment_data, aes(x = village, y = proportion, group = treatment_method_dry, fill = treatment_method_dry)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(aes(ymax = pmin(proportion_upp, 1), ymin = pmax(proportion_low, 0)),
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill = guide_legend(title = NULL)) +
  scale_fill_manual(values = c("#A9CCE3",  "#2E86C1", "#F5B7B1", "#D091BB", "#BBD4A6", "#FAD7A0", "#DFDFDF", SEA_palette)) +
  labs(title = paste("Proportion of dry season water treatment methods by village"), x = "Village", y = "Proportion of Households") +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal()

## treatment_method_wet ----------------

treatment_wet_expanded <- tanganyika_clean %>% select(hh_code, village, treatment_method_wet, stype, fpc) %>%
  separate_rows(treatment_method_wet, sep = "\\|") %>%
  mutate(treatment_method_wet = trimws(treatment_method_wet)) %>% drop_na()  

treatment_wet_design <- treatment_wet_expanded %>%
  as_survey_design(strata = stype, fpc = fpc)

treatment_data_wet <- treatment_wet_design %>%
  group_by(village, treatment_method_wet) %>%
  summarise(proportion = survey_mean(vartype = "ci", na.rm = TRUE), n = unweighted(n())) %>% ungroup()

ggplot(treatment_data_wet, aes(x = village, y = proportion, group = treatment_method_wet, fill = treatment_method_wet)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(aes(ymax = pmin(proportion_upp, 1), ymin = pmax(proportion_low, 0)),
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill = guide_legend(title = NULL)) +
  scale_fill_manual(values = c("#A9CCE3",  "#2E86C1", "#F5B7B1", "#D091BB", "#BBD4A6", "#FAD7A0", "#DFDFDF", SEA_palette)) +
  labs(title = paste("Proportion of wet season water treatment methods by village"), x = "Village", y = "Proportion of Households") +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal()

## handwashing_show ----------------

handwashing_show_expanded <- tanganyika_clean %>% select(hh_code, village, handwashing_show, stype, fpc) %>%
  separate_rows(handwashing_show, sep = "\\|") %>%
  mutate(handwashing_show = trimws(handwashing_show)) %>% drop_na()  

handwashing_show_design <- handwashing_show_expanded %>%
  as_survey_design(strata = stype, fpc = fpc)

handwashing_show_data <- handwashing_show_design %>%
  group_by(village, handwashing_show) %>%
  summarise(proportion = survey_mean(vartype = "ci", na.rm = TRUE), n = unweighted(n())) %>% ungroup()

ggplot(handwashing_show_data, aes(x = village, y = proportion, group = handwashing_show, fill = handwashing_show)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(aes(ymax = pmin(proportion_upp, 1), ymin = pmax(proportion_low, 0)),
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill = guide_legend(title = NULL)) +
  scale_fill_manual(values = c("#A9CCE3",  "#2E86C1", "#F5B7B1", "#D091BB", "#BBD4A6", "#FAD7A0", "#DFDFDF", SEA_palette)) +
  labs(title = paste("Proportion of hand washing methods by village"), x = "Village", y = "Proportion of Households") +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal()

## household_item ----------------

household_item_expanded <- tanganyika_clean %>% select(hh_code, village, household_item, stype, fpc) %>%
  separate_rows(household_item, sep = "\\|") %>%
  mutate(household_item = trimws(household_item)) %>% drop_na()  

household_item_design <- household_item_expanded %>%
  as_survey_design(ids = hh_code, strata = stype, fpc = fpc) # Define cluster ID to prevent fpc from >100%

household_item_data <- household_item_design %>%
  group_by(village, household_item) %>%
  summarise(proportion = survey_mean(vartype = "ci", na.rm = TRUE), n = unweighted(n())) %>% ungroup()

ggplot(household_item_data, aes(x = village, y = proportion, group = household_item, fill = household_item)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(aes(ymax = pmin(proportion_upp, 1), ymin = pmax(proportion_low, 0)),
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill = guide_legend(title = NULL)) +
  scale_fill_manual(values = c("#A9CCE3",  "#2E86C1", "#F5B7B1", "#D091BB", "#BBD4A6", "#FAD7A0", "#DFDFDF", SEA_palette)) +
  labs(title = paste("Proportion of household items by village"), x = "Village", y = "Proportion of Households") +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal()

## ppi_food ----------------

ppi_food_expanded <- tanganyika_clean %>% select(hh_code, village, ppi_food, stype, fpc) %>%
  separate_rows(ppi_food, sep = "\\|") %>%
  mutate(ppi_food = trimws(ppi_food)) %>% drop_na()  

ppi_food_design <- ppi_food_expanded %>%
  as_survey_design(ids = hh_code, strata = stype, fpc = fpc) # Define cluster ID to prevent fpc from >100%

ppi_food_data <- ppi_food_design %>%
  group_by(village, ppi_food) %>%
  summarise(proportion = survey_mean(vartype = "ci", na.rm = TRUE), n = unweighted(n())) %>% ungroup()

ggplot(ppi_food_data, aes(x = village, y = proportion, group = ppi_food, fill = ppi_food)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(aes(ymax = pmin(proportion_upp, 1), ymin = pmax(proportion_low, 0)),
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill = guide_legend(title = NULL)) +
  scale_fill_manual(values = c("#A9CCE3",  "#2E86C1", "#F5B7B1", "#D091BB", "#BBD4A6", "#FAD7A0", "#DFDFDF", SEA_palette)) +
  labs(title = paste("Proportion of food items consumed in the last week by village"), x = "Village", y = "Proportion of Households") +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal()

## household_assets ----------------

household_assets_expanded <- tanganyika_clean %>% select(hh_code, village, household_assets, stype, fpc) %>%
  separate_rows(household_assets, sep = "\\|") %>%
  mutate(household_assets = trimws(household_assets)) %>% drop_na()  

household_assets_design <- household_assets_expanded %>%
  as_survey_design(ids = hh_code, strata = stype, fpc = fpc) # Define cluster ID to prevent fpc from >100%

household_assets_data <- household_assets_design %>%
  group_by(village, household_assets) %>%
  summarise(proportion = survey_mean(vartype = "ci", na.rm = TRUE), n = unweighted(n())) %>% ungroup()

ggplot(household_assets_data, aes(x = village, y = proportion, group = household_assets, fill = household_assets)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(aes(ymax = pmin(proportion_upp, 1), ymin = pmax(proportion_low, 0)),
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill = guide_legend(title = NULL)) +
  scale_fill_manual(values = c("#A9CCE3",  "#2E86C1", "#F5B7B1", "#D091BB", "#BBD4A6", "#FAD7A0", "#DFDFDF", SEA_palette)) +
  labs(title = paste("Proportion of hosuehold assets by village"), x = "Village", y = "Proportion of Households") +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal()

######################################################################################################################
############# Livelihood and Credit Aspects of Households  ############
######################################################################################################################

# List of variables to plot
livelihood_credit_to_plot <- c("household_ability", "borrow_status", "loan_usage", "other_loan_usage", "borrowing_source", 
                               "other_borrowing_source", "not_borrowed", "not_borrowed_other", "cocoba_saccos", "mobile_money") 

# Define plotting and summary table function
livelihood_credit_survey_variable <- function(variable_name) {
  strat_design <- tanganyika_clean %>% as_survey_design(strata = stype, fpc = fpc, variables = c(stype, fpc, village, !!sym(variable_name)))
  
  # Stratify by village and variable, calculate proportions
  village_data <- strat_design %>%
    group_by(village, !!sym(variable_name)) %>%
    summarise(
      proportion = survey_mean(vartype = "ci", na.rm = TRUE), 
      n = unweighted(n()))
  
  # Save summary table
  summary_table <- village_data %>%
    select(village, !!sym(variable_name), n, proportion, proportion_low, proportion_upp) %>%
    rename(Total_Household_Responses = n, Proportion = proportion, Lower_CI = proportion_low, Upper_CI = proportion_upp)
  
  # Save summary table as CSV
  table_file_name <- here::here("images", paste0(variable_name, "_summary_table.csv"))
  write.csv(summary_table, table_file_name, row.names = FALSE)
  
  plot <- ggplot(village_data, aes(x = village, y = proportion, group = !!sym(variable_name), fill = !!sym(variable_name))) +
    geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
    geom_errorbar(aes(ymax = pmin(proportion_upp, 1), ymin = pmax(proportion_low, 0)),
                  position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
    guides(fill = guide_legend(title = NULL)) +
    scale_fill_manual(values = c("#A9CCE3",  "#2E86C1", "#F5B7B1", "#D091BB", "#BBD4A6", "#FAD7A0", "#DFDFDF", SEA_palette)) +
    labs(title = paste("Proportion of", variable_name, "by Village"), x = "Village", y = "Proportion of Households") +
    scale_y_continuous(limits = c(0, 1)) +
    theme_minimal()
  
  return(list(plot = plot, table = summary_table))}

# Generate and save plots and tables
plots_and_tables <- lapply(livelihood_credit_to_plot, function(var) livelihood_credit_survey_variable(var))
for (i in seq_along(livelihood_credit_to_plot)) {
  plot_file_name <- here::here("images", paste0(livelihood_credit_to_plot[i], ".png"))
  ggsave(filename = plot_file_name, plot = plots_and_tables[[i]]$plot, width = 10, height = 7)}

## livelihood_activities ---------------

livelihood_activities_expanded <- tanganyika_clean %>% select(hh_code, village, livelihood_activities, stype, fpc) %>%
  separate_rows(livelihood_activities, sep = "\\|") %>%
  mutate(livelihood_activities = trimws(livelihood_activities)) %>% drop_na()  

livelihood_activities_design <- livelihood_activities_expanded %>%
  as_survey_design(ids = hh_code, strata = stype, fpc = fpc) # Define cluster ID to prevent fpc from >100%

livelihood_activities_data <- livelihood_activities_design %>%
  group_by(village, livelihood_activities) %>%
  summarise(proportion = survey_mean(vartype = "ci", na.rm = TRUE), n = unweighted(n())) %>% ungroup()

ggplot(livelihood_activities_data, aes(x = village, y = proportion, group = livelihood_activities, fill = livelihood_activities)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(aes(ymax = pmin(proportion_upp, 1), ymin = pmax(proportion_low, 0)),
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill = guide_legend(title = NULL)) +
  scale_fill_manual(values = c("#A9CCE3",  "#2E86C1", "#F5B7B1", "#D091BB", "#BBD4A6", "#FAD7A0", "#DFDFDF", SEA_palette)) +
  labs(title = paste("Proportion of hosuehold assets by village"), x = "Village", y = "Proportion of Households") +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal()

## livelihood ranking ---------------


######################################################################################################################
############# Consumption and Food Security  ############
######################################################################################################################

# List of variables to plot
food_to_plot <- c("dagaa", "migebuka", "other_fish", "primary_source", "primary_source_other", "eat_changed", "worry_shortage",
                  "shortage_reason", "food_availability", "availability_changed", "availability_reason") 

# Define plotting function
food_survey_variable <- function(variable_name) {
  strat_design <- tanganyika_clean %>% as_survey_design(strata = stype, fpc = fpc, variables = c(stype, fpc, village, !!sym(variable_name)))
  
  # Stratify by village and variable, calculate proportions
  village_data <- strat_design %>%
    group_by(village, !!sym(variable_name)) %>%
    summarise(
      proportion = survey_mean(vartype = "ci", na.rm = TRUE), 
      n = unweighted(n()))
  
  # Save summary table
  summary_table <- village_data %>%
    select(village, !!sym(variable_name), n, proportion, proportion_low, proportion_upp) %>%
    rename(Total_Household_Responses = n, Proportion = proportion, Lower_CI = proportion_low, Upper_CI = proportion_upp)
  
  # Save summary table as CSV
  table_file_name <- here::here("images", paste0(variable_name, "_summary_table.csv"))
  write.csv(summary_table, table_file_name, row.names = FALSE)
  
  plot <- ggplot(village_data, aes(x = village, y = proportion, group = !!sym(variable_name), fill = !!sym(variable_name))) +
    geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
    geom_errorbar(aes(ymax = pmin(proportion_upp, 1), ymin = pmax(proportion_low, 0)),
                  position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
    guides(fill = guide_legend(title = NULL)) +
    scale_fill_manual(values = c("#A9CCE3",  "#2E86C1", "#F5B7B1", "#D091BB", "#BBD4A6", "#FAD7A0", "#DFDFDF", SEA_palette)) +
    labs(title = paste("Proportion of", variable_name, "by Village"), x = "Village", y = "Proportion of Households") +
    scale_y_continuous(limits = c(0, 1)) +
    theme_minimal()
  
  return(list(plot = plot, table = summary_table))}

# Generate and save plots and tables
plots_and_tables <- lapply(food_to_plot, function(var) food_survey_variable(var))
for (i in seq_along(food_to_plot)) {
  plot_file_name <- here::here("images", paste0(food_to_plot[i], ".png"))
  ggsave(filename = plot_file_name, plot = plots_and_tables[[i]]$plot, width = 10, height = 7)}



### List of Figures in the Tuungane Baseline Report ###
### Add in the cleaned data and develop plots and tables accordingly

# Population pyramid 

# Self-assessment of the ability to meet daily needs at village level

# Land acquisition method in percentage of all plots 

# Crops grown 

# Farming problems 

# Proportion of households with at least one fisher at village level 

# The importance of fishing and agriculture for fishers’ income 

# Type of fishing boats used at village level 

# Fishing gear 

# Percent of the catch that is eaten 

# Relative importance of different species at village level 

# Will there be sufficient fish in the future?

# Frequency of fish consumption 

# Change in the consumption of fish compared to 5 years ago 

# Asset ownership 

# Transport ownership 

# Main water source in the dry and wet season: % of households using a source 

# Main type of water treatment in the dry season 

# Number of rooms used for sleeping 

# Age-specific school attendance rates 

# Proportion of households that borrowed money in the last year at village level 

# Distribution of borrowed amounts 

# Purpose of the loan 

# Source of loans 

# Reason for not having borrowed any money in the previous year 

# Composite wellbeing indicator: mean scores 

# Perception of the relationship between TANAPA and the village at village level 

# Knowledge about an environmental management committee in the village 

# Proportion that attended a public village meeting 

# Statement: “There is sufficient forest close to this village to meet our day-to-day needs.” 

# Statements: “Deforestation causes siltation” and “Siltation is harmful to fish” 

# Statements about protection of village forests, and chimpanzees 

# Statement: “Mahale Mountains National Park should continue to be protected” by village. 

# Statement: “The national park provides benefits for our community.” 

# Proportion of households that collect forest products 

# Number of different forest products collected 

# Age and sex of the person responsible for the collection of forest products 

# Proportion of households that collect and sell some of the forest products. 

# Source of firewood 

# Proportion of households that think the village population has increased over the last 5 years 

# Problems caused by population growth 

# Occurrence of disputes at village level 

# Disease prevalence 

# Change in access to medical care compared to 5 years ago at village level 

# Diet: frequency of eating fruit & vegetables, fish, and meat or poultry 


