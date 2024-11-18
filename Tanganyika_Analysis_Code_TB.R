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
library(sea)

ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)
rm(list=ls())

# Add Sustain East Africa colour pallette
SEA_palette <- c("#d77e5e", "#a4b792", "#e6e7e2", "#3d5919", "#202C39", "#381D2A", "#000000")

# import cleaned Tanganyika household survey data with --------------------------------- 
tanganyika_clean <- readRDS("tanganyika_clean.rds")
head(tanganyika_clean)

demo <- readRDS("survey_demo.rds")

######################################################################################################################
############# Population Pyramid  ############
######################################################################################################################
# Define age groups with factor levels to ensure correct order
age_levels <- c("<5", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
                "35-39", "40-44", "45-49", "50-54", "55-59", "60-64",
                "65-69", "70-74", "75-79", "80+")

# Categorize age groups and ensure they have the correct order
demo <- demo %>%
  mutate(age_group = case_when(
    age >= 80 ~ "80+",
    age >= 75 & age < 80 ~ "75-79",
    age >= 70 & age < 75 ~ "70-74",
    age >= 65 & age < 70 ~ "65-69",
    age >= 60 & age < 65 ~ "60-64",
    age >= 55 & age < 60 ~ "55-59",
    age >= 50 & age < 55 ~ "50-54",
    age >= 45 & age < 50 ~ "45-49",
    age >= 40 & age < 45 ~ "40-44",
    age >= 35 & age < 40 ~ "35-39",
    age >= 30 & age < 35 ~ "30-34",
    age >= 25 & age < 30 ~ "25-29",
    age >= 20 & age < 25 ~ "20-24",
    age >= 15 & age < 20 ~ "15-19",
    age >= 10 & age < 15 ~ "10-14",
    age >= 5 & age < 10 ~ "5-9",
    age < 5 ~ "<5"
  )) %>%
  mutate(age_group = factor(age_group, levels = age_levels))  # Set factor levels for ordering

# Calculate overall percentages by age group and gender, excluding unwanted sex value
total_count <- nrow(demo %>% filter(sex != "I DO NOT WANT TO ANSWER"))  # Total population count for reference

demo_summary <- demo %>%
  filter(sex != "I DO NOT WANT TO ANSWER") %>%
  count(age_group, sex) %>%
  mutate(percentage = n / total_count * 100)  # Calculate percentage of entire population

# Plot population pyramid
ggplot(demo_summary, aes(x = age_group, y = ifelse(sex == "MALE", -percentage, percentage), fill = sex)) +
  geom_bar(stat = "identity", width = 0.8) +
  coord_flip() +
  scale_y_continuous(labels = abs, limits = c(-10, 10)) +
  labs(title = "Population Pyramid", x = "Age Group", y = "Percentage of Total Population") +
  scale_fill_manual(values = c("MALE" =  "#3d5919", "FEMALE" =  "#a4b792")) +
  theme_minimal()


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
  # Set survey design
  strat_design <- tanganyika_clean %>%
    as_survey_design(strata = stype, fpc = fpc, variables = c(stype, fpc, village, !!sym(variable_name)))
  
  # Group by village and the current variable, then calculate proportions and totals
  village_data <- strat_design %>%
    group_by(village, !!sym(variable_name)) %>%
    summarise(proportion = survey_mean(vartype = "ci", na.rm = TRUE), total = survey_total(vartype = "ci", na.rm = TRUE), n = unweighted(n()))
  
  # Save summary table
  summary_table <- village_data %>%
    select(village, !!sym(variable_name), n, total, total_low, total_upp, proportion, proportion_low, proportion_upp) %>%
    rename(n = n, Total = total, Lower_Total_CI = total_low, Upper_Total_CI = total_upp, Proportion = proportion, Lower_Proportion_CI = proportion_low, Upper_Proportion_CI = proportion_upp)
  
  # Save summary table as CSV
  table_file_name <- here::here("images", paste0(variable_name, "_summary_table.csv"))
  write.csv(summary_table, table_file_name, row.names = FALSE)
  
  # Create plot
  plot <- ggplot(village_data, aes(x = village, y = proportion, group = !!sym(variable_name), fill = !!sym(variable_name))) +
    geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
    geom_errorbar(aes(ymax = pmin(proportion_upp, 1), ymin = pmax(proportion_low, 0)),
                  position = position_dodge(preserve = "single", width = 0.95), width = 0.1)+
    guides(fill = guide_legend(title = NULL)) +
    scale_fill_manual(values = c("#A9CCE3",  "#2E86C1", "#F5B7B1", "#D091BB", "#BBD4A6", "#FAD7A0", "#DFDFDF", SEA_palette)) +
    labs(title = paste("Proportion of", variable_name, "by Village"), x = "Village", y = "Proportion of Households") +
    scale_y_continuous(limits = c(0, 1)) +
    theme_minimal()
  
  return(list(plot = plot, table = summary_table))}

# Generate and save plots and tables
plots_and_tables <- lapply(hh_info_to_plot, function(var) hh_info_survey_variable(var))

# Save plots
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
  summarise(proportion = survey_mean(vartype = "ci", na.rm = TRUE), total = survey_total(vartype = "ci", na.rm = TRUE), n = unweighted(n())) %>% ungroup()

ggplot(treatment_data, aes(x = village, y = proportion, group = treatment_method_dry, fill = treatment_method_dry)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(aes(ymax = pmin(proportion_upp, 1), ymin = pmax(proportion_low, 0)),
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill = guide_legend(title = NULL)) +
  scale_fill_manual(values = c( SEA_palette, "#A9CCE3",  "#2E86C1", "#F5B7B1", "#D091BB", "#BBD4A6", "#FAD7A0", "#DFDFDF")) +
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
  summarise(proportion = survey_mean(vartype = "ci", na.rm = TRUE), total = survey_total(vartype = "ci", na.rm = TRUE), n = unweighted(n())) %>% ungroup()

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
  summarise(proportion = survey_mean(vartype = "ci", na.rm = TRUE), total = survey_total(vartype = "ci", na.rm = TRUE), n = unweighted(n())) %>% ungroup()

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
  summarise(proportion = survey_mean(vartype = "ci", na.rm = TRUE), total = survey_total(vartype = "ci", na.rm = TRUE), n = unweighted(n())) %>% ungroup()

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
livelihood_credit_to_plot <- c("household_ability", "borrow_status", "other_loan_usage", 
                               "other_borrowing_source", "not_borrowed", "not_borrowed_other", "cocoba_saccos", "mobile_money") 

# Define plotting and summary table function
livelihood_credit_survey_variable <- function(variable_name) {
  # Set survey design
  strat_design <- tanganyika_clean %>%
    as_survey_design(strata = stype, fpc = fpc, variables = c(stype, fpc, village, !!sym(variable_name)))
  
  # Group by village and the current variable, then calculate proportions and totals
  village_data <- strat_design %>%
    group_by(village, !!sym(variable_name)) %>%
    summarise(proportion = survey_mean(vartype = "ci", na.rm = TRUE), total = survey_total(vartype = "ci", na.rm = TRUE), n = unweighted(n()))
  
  # Save summary table
  summary_table <- village_data %>%
    select(village, !!sym(variable_name), n, total, total_low, total_upp, proportion, proportion_low, proportion_upp) %>%
    rename(n = n, Total = total, Lower_Total_CI = total_low, Upper_Total_CI = total_upp, Proportion = proportion, Lower_Proportion_CI = proportion_low, Upper_Proportion_CI = proportion_upp)
  
  # Save summary table as CSV
  table_file_name <- here::here("images", paste0(variable_name, "_summary_table.csv"))
  write.csv(summary_table, table_file_name, row.names = FALSE)
  
  # Create plot
  plot <- ggplot(village_data, aes(x = village, y = proportion, group = !!sym(variable_name), fill = !!sym(variable_name))) +
    geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
    geom_errorbar(aes(ymax = pmin(proportion_upp, 1), ymin = pmax(proportion_low, 0)),
                  position = position_dodge(preserve = "single", width = 0.95), width = 0.1)+
    guides(fill = guide_legend(title = NULL)) +
    scale_fill_manual(values = c("#A9CCE3",  "#2E86C1", "#F5B7B1", "#D091BB", "#BBD4A6", "#FAD7A0", "#DFDFDF", SEA_palette)) +
    labs(title = paste("Proportion of", variable_name, "by Village"), x = "Village", y = "Proportion of Households") +
    scale_y_continuous(limits = c(0, 1)) +
    theme_minimal()
  
  return(list(plot = plot, table = summary_table))}

# Generate and save plots and tables
plots_and_tables <- lapply(livelihood_credit_to_plot, function(var) livelihood_credit_survey_variable(var))

# Save plots
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

## loan_usage --------------

loan_usage_expanded <- tanganyika_clean %>% select(hh_code, village, loan_usage, stype, fpc) %>%
  separate_rows(loan_usage, sep = "\\|") %>%
  mutate(loan_usage = trimws(loan_usage)) %>% drop_na()  

loan_usage_design <- loan_usage_expanded %>%
  as_survey_design(strata = stype, fpc = fpc)

loan_usage_data <- loan_usage_design %>%
  group_by(village, loan_usage) %>%
  summarise(proportion = survey_mean(vartype = "ci", na.rm = TRUE), total = survey_total(vartype = "ci", na.rm = TRUE), n = unweighted(n())) %>% ungroup()

ggplot(loan_usage_data, aes(x = village, y = proportion, group = loan_usage, fill = loan_usage)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(aes(ymax = pmin(proportion_upp, 1), ymin = pmax(proportion_low, 0)),
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill = guide_legend(title = NULL)) +
  scale_fill_manual(values = c("#A9CCE3",  "#2E86C1", "#F5B7B1", "#D091BB", "#BBD4A6", "#FAD7A0", "#DFDFDF", SEA_palette)) +
  labs(title = paste("Proportion of loan usage by village"), x = "Village", y = "Proportion of Households") +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal()

## borrowing_source ---------------

borrowing_source_expanded <- tanganyika_clean %>% select(hh_code, village, borrowing_source, stype, fpc) %>%
  separate_rows(borrowing_source, sep = "\\|") %>%
  mutate(borrowing_source = trimws(borrowing_source)) %>% drop_na()  

borrowing_source_design <- borrowing_source_expanded %>%
  as_survey_design(strata = stype, fpc = fpc)

borrowing_source_data <- borrowing_source_design %>%
  group_by(village, borrowing_source) %>%
  summarise(proportion = survey_mean(vartype = "ci", na.rm = TRUE), total = survey_total(vartype = "ci", na.rm = TRUE), n = unweighted(n())) %>% ungroup()

ggplot(borrowing_source_data, aes(x = village, y = proportion, group = borrowing_source, fill = borrowing_source)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(aes(ymax = pmin(proportion_upp, 1), ymin = pmax(proportion_low, 0)),
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill = guide_legend(title = NULL)) +
  scale_fill_manual(values = c("#A9CCE3",  "#2E86C1", "#F5B7B1", "#D091BB", "#BBD4A6", "#FAD7A0", "#DFDFDF", SEA_palette)) +
  labs(title = paste("Proportion of borrowing sources by village"), x = "Village", y = "Proportion of Households") +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal()

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
      proportion = survey_mean(vartype = "ci", na.rm = TRUE), total = survey_total(vartype = "ci", na.rm = TRUE), n = unweighted(n()))
  
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


######################################################################################################################
############# Governance  ############
######################################################################################################################

# List of variables to plot
gov_to_plot <- c("hh_influence", "people_trusted", "nearby_trusted", "government_trusted", "village_membership", "meeting_attendance", "disputes_conflicts", 
                 "more_less", "conflict_other", "parties_other", "fair_resolution", "other_influential", "leader_position", "BMU_activity") 

# Define plotting function
gov_survey_variable <- function(variable_name) {
  strat_design <- tanganyika_clean %>% as_survey_design(strata = stype, fpc = fpc, variables = c(stype, fpc, village, !!sym(variable_name)))
  
  # Stratify by village and variable, calculate proportions
  village_data <- strat_design %>%
    group_by(village, !!sym(variable_name)) %>%
    summarise(
      proportion = survey_mean(vartype = "ci", na.rm = TRUE), 
      total = survey_total(vartype = "ci", na.rm = TRUE),
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
plots_and_tables <- lapply(gov_to_plot, function(var) gov_survey_variable(var))
for (i in seq_along(gov_to_plot)) {
  plot_file_name <- here::here("images", paste0(gov_to_plot[i], ".png"))
  ggsave(filename = plot_file_name, plot = plots_and_tables[[i]]$plot, width = 10, height = 7)}

## group_name -----------------

## conflict_reason ---------------

conflict_reason_expanded <- tanganyika_clean %>% select(hh_code, village, conflict_reason, stype, fpc) %>%
  separate_rows(conflict_reason, sep = "\\|") %>%
  mutate(conflict_reason = trimws(conflict_reason)) %>% drop_na()  

conflict_reason_design <- conflict_reason_expanded %>%
  as_survey_design(strata = stype, fpc = fpc)

conflict_reason_data <- conflict_reason_design %>%
  group_by(village, conflict_reason) %>%
  summarise(proportion = survey_mean(vartype = "ci", na.rm = TRUE), total = survey_total(vartype = "ci", na.rm = TRUE), n = unweighted(n())) %>% ungroup()

ggplot(conflict_reason_data, aes(x = village, y = proportion, group = conflict_reason, fill = conflict_reason)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(aes(ymax = pmin(proportion_upp, 1), ymin = pmax(proportion_low, 0)),
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill = guide_legend(title = NULL)) +
  scale_fill_manual(values = c("#A9CCE3",  "#2E86C1", "#F5B7B1", "#D091BB", "#BBD4A6", "#FAD7A0", "#DFDFDF", SEA_palette)) +
  labs(title = paste("Proportion of conflict reasons by village"), x = "Village", y = "Proportion of Households") +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal()

## conflict_parties ----------------

conflict_parties_expanded <- tanganyika_clean %>% select(hh_code, village, conflict_parties, stype, fpc) %>%
  separate_rows(conflict_parties, sep = "\\|") %>%
  mutate(conflict_parties = trimws(conflict_parties)) %>% drop_na()  

conflict_parties_design <- conflict_parties_expanded %>%
  as_survey_design(strata = stype, fpc = fpc)

conflict_parties_data <- conflict_parties_design %>%
  group_by(village, conflict_parties) %>%
  summarise(proportion = survey_mean(vartype = "ci", na.rm = TRUE), total = survey_total(vartype = "ci", na.rm = TRUE), n = unweighted(n())) %>% ungroup()

ggplot(conflict_parties_data, aes(x = village, y = proportion, group = conflict_parties, fill = conflict_parties)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(aes(ymax = pmin(proportion_upp, 1), ymin = pmax(proportion_low, 0)),
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill = guide_legend(title = NULL)) +
  scale_fill_manual(values = c("#A9CCE3",  "#2E86C1", "#F5B7B1", "#D091BB", "#BBD4A6", "#FAD7A0", "#DFDFDF", SEA_palette)) +
  labs(title = paste("Proportion of conflict reasons by village"), x = "Village", y = "Proportion of Households") +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal()

## influential_leaders ---------------

influential_leaders_expanded <- tanganyika_clean %>% select(hh_code, village, influential_leaders, stype, fpc) %>%
  separate_rows(influential_leaders, sep = "\\|") %>%
  mutate(influential_leaders = trimws(influential_leaders)) %>% drop_na()  

influential_leaders_design <- influential_leaders_expanded %>%
  as_survey_design(strata = stype, fpc = fpc)

influential_leaders_data <- influential_leaders_design %>%
  group_by(village, influential_leaders) %>%
  summarise(proportion = survey_mean(vartype = "ci", na.rm = TRUE), total = survey_total(vartype = "ci", na.rm = TRUE), n = unweighted(n())) %>% ungroup()

ggplot(influential_leaders_data, aes(x = village, y = proportion, group = influential_leaders, fill = influential_leaders)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(aes(ymax = pmin(proportion_upp, 1), ymin = pmax(proportion_low, 0)),
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill = guide_legend(title = NULL)) +
  scale_fill_manual(values = c("#A9CCE3",  "#2E86C1", "#F5B7B1", "#D091BB", "#BBD4A6", "#FAD7A0", "#DFDFDF", SEA_palette)) +
  labs(title = paste("Proportion of conflict reasons by village"), x = "Village", y = "Proportion of Households") +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal()

######################################################################################################################
############# Beach Management Units (BMU)  ############
######################################################################################################################

# List of variables to plot
BMU_to_plot <- c("awareness_bmu", "bmu_member","tnc_support_bmu", "agency_support", "agency_other", "bmu_women", "bmu_youth", 
                  "good_leaders", "leaders_elected", "trust_collaboration", "conflcit_interest", "bmu_bylaws", "bylaws_followed", "bmu_practice")

# Define plotting function
BMU_survey_variable <- function(variable_name) {
  strat_design <- tanganyika_clean %>% as_survey_design(strata = stype, fpc = fpc, variables = c(stype, fpc, village, !!sym(variable_name)))
  
  # Stratify by village and variable, calculate proportions
  village_data <- strat_design %>%
    group_by(village, !!sym(variable_name)) %>%
    summarise(
      proportion = survey_mean(vartype = "ci", na.rm = TRUE), 
      total = survey_total(vartype = "ci", na.rm = TRUE),
      n = unweighted(n())) %>%
    filter(!is.na(!!sym(variable_name)))
  
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
plots_and_tables <- lapply(BMU_to_plot, function(var) BMU_survey_variable(var))
for (i in seq_along(BMU_to_plot)) {
  plot_file_name <- here::here("images", paste0(BMU_to_plot[i], ".png"))
  ggsave(filename = plot_file_name, plot = plots_and_tables[[i]]$plot, width = 10, height = 7)}

## bmu_activity --------------

## conflict_resolution ------------
conflict_resolution_expanded <- tanganyika_clean %>% select(hh_code, village, conflict_resolution, stype, fpc) %>%
  separate_rows(conflict_resolution, sep = "\\|") %>%
  mutate(conflict_resolution = trimws(conflict_resolution)) %>% drop_na()  

conflict_resolution_design <- conflict_resolution_expanded %>%
  as_survey_design(strata = stype, fpc = fpc)

conflict_resolution_data <- conflict_resolution_design %>%
  group_by(village, conflict_resolution) %>%
  summarise(proportion = survey_mean(vartype = "ci", na.rm = TRUE), total = survey_total(vartype = "ci", na.rm = TRUE), n = unweighted(n())) %>% ungroup()

ggplot(conflict_resolution_data, aes(x = village, y = proportion, group = conflict_resolution, fill = conflict_resolution)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(aes(ymax = pmin(proportion_upp, 1), ymin = pmax(proportion_low, 0)),
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill = guide_legend(title = NULL)) +
  scale_fill_manual(values = c("#A9CCE3",  "#2E86C1", "#F5B7B1", "#D091BB", "#BBD4A6", "#FAD7A0", "#DFDFDF", SEA_palette)) +
  labs(title = paste("Proportion of conflict resolutions by village"), x = "Village", y = "Proportion of Households") +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal()

######################################################################################################################
############# Fishing  ############
######################################################################################################################

# List of variables to plot
fishing_to_plot <- c("fisheries_resources", "rights_access", "security_rights", "relationship_officer", "decision_making", 
                     "satisfaction_involvement", "awareness_reserves", "purpose_reserves", "sustainability_population", "sufficiency_fish")

# Define plotting function
fishing_survey_variable <- function(variable_name) {
  strat_design <- tanganyika_clean %>% as_survey_design(strata = stype, fpc = fpc, variables = c(stype, fpc, village, !!sym(variable_name)))
  
  # Stratify by village and variable, calculate proportions
  village_data <- strat_design %>%
    group_by(village, !!sym(variable_name)) %>%
    summarise(
      proportion = survey_mean(vartype = "ci", na.rm = TRUE), 
      total = survey_total(vartype = "ci", na.rm = TRUE),
      n = unweighted(n())) %>%
    filter(!is.na(!!sym(variable_name)))
  
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
plots_and_tables <- lapply(fishing_to_plot, function(var) fishing_survey_variable(var))
for (i in seq_along(fishing_to_plot)) {
  plot_file_name <- here::here("images", paste0(fishing_to_plot[i], ".png"))
  ggsave(filename = plot_file_name, plot = plots_and_tables[[i]]$plot, width = 10, height = 7)}

## boat_type --------------
boat_type_expanded <- tanganyika_clean %>% select(hh_code, village, boat_type, stype, fpc) %>%
  separate_rows(boat_type, sep = "\\|") %>%
  mutate(boat_type = trimws(boat_type)) %>% drop_na()  

boat_type_design <- boat_type_expanded %>%
  as_survey_design(strata = stype, fpc = fpc)

boat_type_data <- boat_type_design %>%
  group_by(village, boat_type) %>%
  summarise(proportion = survey_mean(vartype = "ci", na.rm = TRUE), total = survey_total(vartype = "ci", na.rm = TRUE), n = unweighted(n())) %>% ungroup()

ggplot(boat_type_data, aes(x = village, y = proportion, group = boat_type, fill = boat_type)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(aes(ymax = pmin(proportion_upp, 1), ymin = pmax(proportion_low, 0)),
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill = guide_legend(title = NULL)) +
  scale_fill_manual(values = c("#A9CCE3",  "#2E86C1", "#F5B7B1", "#D091BB", "#BBD4A6", "#FAD7A0", "#DFDFDF", SEA_palette)) +
  labs(title = paste("Proportion of boat types used for fishing by village"), x = "Village", y = "Proportion of Households") +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal()

## fishing_gear ---------------

fishing_gear_expanded <- tanganyika_clean %>% select(hh_code, village, fishing_gear, stype, fpc) %>%
  separate_rows(fishing_gear, sep = "\\|") %>%
  mutate(fishing_gear = trimws(fishing_gear)) %>% drop_na()  

fishing_gear_design <- fishing_gear_expanded %>%
  as_survey_design(strata = stype, fpc = fpc)

fishing_gear_data <- fishing_gear_design %>%
  group_by(village, fishing_gear) %>%
  summarise(proportion = survey_mean(vartype = "ci", na.rm = TRUE), total = survey_total(vartype = "ci", na.rm = TRUE), n = unweighted(n())) %>% ungroup()

ggplot(fishing_gear_data, aes(x = village, y = proportion, group = fishing_gear, fill = fishing_gear)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(aes(ymax = pmin(proportion_upp, 1), ymin = pmax(proportion_low, 0)),
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill = guide_legend(title = NULL)) +
  scale_fill_manual(values = c("#A9CCE3",  "#2E86C1", "#F5B7B1", "#D091BB", "#BBD4A6", "#FAD7A0", "#DFDFDF", SEA_palette)) +
  labs(title = paste("Proportion of fishing gear used by village"), x = "Village", y = "Proportion of Households") +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal()

######################################################################################################################
############# Fishing Practices per Village ############
######################################################################################################################

# List of variables to plot
fish_village_to_plot <- c("time_input", "time_comparison", "catch_comparison", "bmu_helpfulness", "group_membership", "cooperative", "cocoba_group", "other_group", "tnc_support", 
                          "dagaa_season", "migebuka_season", "kungura_season", "ngege_season", "kuhe_season", "sangara_season",
                          "satisfaction_skills", "tools_used", "catch_gained", "market_supply", "satisfaction_purchase", "satisfaction_market", "satisfaction_income", "satisfaction_capital", "business_skills", "organization_support",
                          "other_agency_support", "name_agency", "group_helpfulness", "group_challenges", "group_improvements", "activity_long_term")

# Define plotting function
fish_village_survey_variable <- function(variable_name) {
  strat_design <- tanganyika_clean %>% 
    as_survey_design(strata = stype, fpc = fpc, variables = c(stype, fpc, village, !!sym(variable_name)))
  
  # Stratify by village and variable, calculate proportions
  village_data <- strat_design %>%
    group_by(village, !!sym(variable_name)) %>%
    summarise(
      proportion = survey_mean(vartype = "ci", na.rm = TRUE), 
      total = survey_total(vartype = "ci", na.rm = TRUE),
      n = unweighted(n())) %>%
    filter(!is.na(!!sym(variable_name)))
  
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
plots_and_tables <- lapply(fish_village_to_plot, function(var) fish_village_survey_variable(var))
for (i in seq_along(fish_village_to_plot)) {
  plot_file_name <- here::here("images", paste0(fish_village_to_plot[i], ".png"))
  ggsave(filename = plot_file_name, plot = plots_and_tables[[i]]$plot, width = 10, height = 7)}

## fish_importance -----------

fish_importance_vars <- c("dagaa_importance", "migebuka_importance", "kungura_importance", "ngege_importance", "kuhe_importance", "sangara_importance")

fish_importance_long <- tanganyika_clean %>%
  pivot_longer(cols = all_of(fish_importance_vars), names_to = "Fish_Species", values_to = "Importance") %>%
  mutate(Fish_Species = str_replace(Fish_Species, "_importance", "") %>% str_to_title())

strat_design <- fish_importance_long %>%
  as_survey_design(strata = stype, fpc = fpc, variables = c(stype, fpc, village, Fish_Species, Importance))

# Summarize and plot function
aggregate_fish_importance_plot <- function() {
  # Calculate mean importance scores stratified by village and fish species
  village_data <- strat_design %>%
    group_by(village, Fish_Species) %>%
    summarise(
      mean_importance = survey_mean(Importance, vartype = "ci", na.rm = TRUE),
      n = unweighted(n()))
  
  # Create a summary table
  summary_table <- village_data %>%
    select(village, Fish_Species, n, mean_importance, mean_importance_low, mean_importance_upp) %>%
    rename(
      Total_Household_Responses = n,
      Mean_Importance = mean_importance,
      Lower_CI = mean_importance_low,
      Upper_CI = mean_importance_upp)
  
  # Save summary table as CSV
  table_file_name <- here::here("images", "fish_species_importance_summary_table.csv")
  write.csv(summary_table, table_file_name, row.names = FALSE)
  
  # Create the combined plot for all fish species
  plot <- ggplot(village_data, aes(x = village, y = mean_importance, fill = Fish_Species)) +
    geom_bar(stat = "identity", position = position_dodge(), width = 0.8) +
    geom_errorbar(aes(ymin = mean_importance_low, ymax = mean_importance_upp),
                  position = position_dodge(width = 0.8), width = 0.2) +
    labs(title = "Average Importance of Fish Species by Village", x = "Village", y = "Mean Importance Score", fill = "Fish Species") + 
    scale_y_continuous(limits = c(0, 6)) +
    theme_minimal() + scale_fill_manual(values = c("#A9CCE3", "#2E86C1", "#F5B7B1", "#D091BB", "#BBD4A6", "#FAD7A0"))  # Customize colors
  
  return(list(plot = plot, table = summary_table))}

# Generate and save the plot and summary table
fish_importance_results <- aggregate_fish_importance_plot()
ggsave(filename = here::here("images", "aggregate_fish_importance.png"), 
       plot = fish_importance_results$plot, width = 12, height = 8)

## sale_price_best -----------

best_price_vars <- c("dagaa_best", "migebuka_best", "kungura_best", "ngege_best", "kuhe_best", "sangara_best")

tanganyika_long_best <- tanganyika_clean %>%
  pivot_longer(cols = all_of(best_price_vars), names_to = "Fish_Species", values_to = "Best_Price") %>%
  mutate(Fish_Species = str_replace(Fish_Species, "_best", "") %>% str_to_title())

strat_design_best <- tanganyika_long_best %>%
  as_survey_design(strata = stype, fpc = fpc, variables = c(stype, fpc, village, Fish_Species, Best_Price))

aggregate_fish_best_price_plot <- function() {
  # Calculate mean best prices stratified by village and fish species
  village_data_best <- strat_design_best %>%
    group_by(village, Fish_Species) %>%
    summarise(
      mean_best_price = survey_mean(Best_Price, vartype = "ci", na.rm = TRUE),
      n = unweighted(n()))

  summary_table_best <- village_data_best %>%
    select(village, Fish_Species, n, mean_best_price, mean_best_price_low, mean_best_price_upp) %>%
    rename(
      Total_Household_Responses = n,
      Mean_Best_Price = mean_best_price,
      Lower_CI = mean_best_price_low,
      Upper_CI = mean_best_price_upp)
  
  # Save summary table as CSV
  table_file_name_best <- here::here("images", "fish_species_best_price_summary_table.csv")
  write.csv(summary_table_best, table_file_name_best, row.names = FALSE)
  
  # Create the combined plot for all fish species' best sale prices
  plot <- ggplot(village_data_best, aes(x = village, y = mean_best_price, fill = Fish_Species)) +
    geom_bar(stat = "identity", position = position_dodge(), width = 0.8) +
    geom_errorbar(aes(ymin = mean_best_price_low, ymax = mean_best_price_upp),
                  position = position_dodge(width = 0.8), width = 0.2) +
    labs(title = "Average Best Sale Price of Fish Species by Village", x = "Village", y = "Mean Best Price", fill = "Fish Species") +
    theme_minimal() +
    scale_fill_manual(values = c("#A9CCE3", "#2E86C1", "#F5B7B1", "#D091BB", "#BBD4A6", "#FAD7A0"))  
  return(list(plot = plot, table = summary_table_best))}

# Generate and save the plot and summary table
fish_best_price_results <- aggregate_fish_best_price_plot()
ggsave(filename = here::here("images", "aggregate_fish_best_price.png"), 
       plot = fish_best_price_results$plot, width = 12, height = 8)


## sale_price_worst -----------

worst_price_vars <- c("dagaa_worst", "migembuka_worst", "kungura_worst", "ngege_worst", "kuhe_worst", "sangara_worst")

tanganyika_long_worst <- tanganyika_clean %>%
  pivot_longer(cols = all_of(worst_price_vars), names_to = "Fish_Species", values_to = "Worst_Price") %>%
  mutate(Fish_Species = str_replace(Fish_Species, "_worst", "") %>% str_to_title())

strat_design_worst <- tanganyika_long_worst %>%
  as_survey_design(strata = stype, fpc = fpc, variables = c(stype, fpc, village, Fish_Species, Worst_Price))

# Summarize and plot function for worst sale prices
aggregate_fish_worst_price_plot <- function() {
  # Calculate mean worst prices stratified by village and fish species
  village_data_worst <- strat_design_worst %>%
    group_by(village, Fish_Species) %>%
    summarise(
      mean_worst_price = survey_mean(Worst_Price, vartype = "ci", na.rm = TRUE),
      n = unweighted(n()))

  summary_table_worst <- village_data_worst %>%
    select(village, Fish_Species, n, mean_worst_price, mean_worst_price_low, mean_worst_price_upp) %>%
    rename(
      Total_Household_Responses = n,
      Mean_Worst_Price = mean_worst_price,
      Lower_CI = mean_worst_price_low,
      Upper_CI = mean_worst_price_upp)

  table_file_name_worst <- here::here("images", "fish_species_worst_price_summary_table.csv")
  write.csv(summary_table_worst, table_file_name_worst, row.names = FALSE)

  plot <- ggplot(village_data_worst, aes(x = village, y = mean_worst_price, fill = Fish_Species)) +
    geom_bar(stat = "identity", position = position_dodge(), width = 0.8) +
    geom_errorbar(aes(ymin = mean_worst_price_low, ymax = mean_worst_price_upp),
                  position = position_dodge(width = 0.8), width = 0.2) +
    labs(title = "Average Worst Sale Price of Fish Species by Village", x = "Village", y = "Mean Worst Price", fill = "Fish Species") + 
    theme_minimal() + scale_fill_manual(values = c("#A9CCE3", "#2E86C1", "#F5B7B1", "#D091BB", "#BBD4A6", "#FAD7A0")) 
  return(list(plot = plot, table = summary_table_worst))}

# Generate and save the plot and summary table
fish_worst_price_results <- aggregate_fish_worst_price_plot()
ggsave(filename = here::here("images", "aggregate_fish_worst_price.png"), 
       plot = fish_worst_price_results$plot, width = 12, height = 8)

######################################################################################################################
############# Fish Trading Practices per Village ############
######################################################################################################################

# List of variables to plot
fish_traders_to_plot <- c("trader_present", "trader_code", "trading_form", "trading_form_other", "supply_chain", "fish_sell", "trade_target_type", "dagaa_trade_season", "migebuka_trade_season", "kungura_trade_season", "ngege_trade_season", "kuhe_trade_season", "sangara_trade_season", 
                          "trade_best", "dagaa_trade_best", "migebuka_trade_best", "kungura_trade_best", "ngege_trade_best", "kuhe_trade_best", "sangara_trade_best",
                          "trade_worst", "dagaa_trade_worst", "migebuka_trade_worst", "kungura_trade_worst", "ngege_trade_worst", "kuhe_trade_worst", "sangara_trade_worst",
                          "satisfaction_trade", "satisfaction_trade_skills", "trade_tools_used", "trade_productivity", "trade_market_supply", "satisfaction_purchase", "trade_market", "trade_income", "trade_capital", "trade_business_skills", "_trade_organization_support",
                          "trading_challenges", "trading_opportunities", "business_group_membership", "cooperative_fico", "cooperative_fico_name", "cocoba_savings", "cocoba_savings_name", "other_trading_group", "other_trading_group_name", "trading_group_helpfulness", "trading_tnc_supported", "trading_other_agency", "trading_other_agency_name", "trading_long_term")

# Define plotting function
fish_traders_survey_variable <- function(variable_name) {
  strat_design <- tanganyika_clean %>% 
    as_survey_design(strata = stype, fpc = fpc, variables = c(stype, fpc, village, !!sym(variable_name)))
  
  # Stratify by village and variable, calculate proportions
  village_data <- strat_design %>%
    group_by(village, !!sym(variable_name)) %>%
    summarise(
      proportion = survey_mean(vartype = "ci", na.rm = TRUE), 
      total = survey_total(vartype = "ci", na.rm = TRUE),
      n = unweighted(n())) %>%
    filter(!is.na(!!sym(variable_name)))
  
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
plots_and_tables <- lapply(fish_traders_to_plot, function(var) fish_traders_survey_variable(var))
for (i in seq_along(fish_traders_to_plot)) {
  plot_file_name <- here::here("images", paste0(fish_traders_to_plot[i], ".png"))
  ggsave(filename = plot_file_name, plot = plots_and_tables[[i]]$plot, width = 10, height = 7)}
