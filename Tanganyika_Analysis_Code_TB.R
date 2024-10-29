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

# Define plotting function
hh_info_survey_variable <- function(variable_name) {

# Set survey design
strat_design <- tanganyika_clean %>%
  as_survey_design(strata = stype, fpc = fpc, variables = c(stype, fpc, village, !!sym(variable_name)))
  
# Group by village and the current variable, then calculate proportions
village_data <- strat_design %>%
  group_by(village, !!sym(variable_name)) %>%
  summarise(proportion = survey_mean(vartype = "ci", na.rm = TRUE), n = unweighted(n()))
  
ggplot(village_data, aes(x = village, y = proportion, group = !!sym(variable_name), fill = !!sym(variable_name))) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(aes(ymax = pmin(proportion_upp, 1), ymin = pmax(proportion_low, 0)),
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill = guide_legend(title = NULL)) +
  scale_fill_manual(values = c("#A9CCE3",  "#2E86C1", "#F5B7B1", "#D091BB", "#BBD4A6", "#FAD7A0", "#DFDFDF", SEA_palette)) +
  labs(title = paste("Proportion of", variable_name, "by Village"), x = "Village", y = "Proportion of Households") +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal()}

# Generate and save plots
plots <- lapply(hh_info_to_plot, function(var) hh_info_survey_variable(var))
for (i in seq_along(hh_info_to_plot)) {
  file_name <- here::here("images", paste0(hh_info_to_plot[i], ".png"))
  ggsave(filename = file_name, plot = plots[[i]], width = 10, height = 7)}

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

# treatment_method_wet ----------------

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

# handwashing_show ----------------

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

######################################################################################################################
############# Livelihood and Credit Aspects of Households  ############
######################################################################################################################

# List of variables to plot
livelihood_credit_to_plot <- c() # add in the column titles 

# Define plotting function
livelihood_credit_survey_variable <- function(variable_name) {
  
  # Set survey design
  strat_design <- tanganyika_clean %>%
    as_survey_design(strata = stype, fpc = fpc, variables = c(stype, fpc, village, !!sym(variable_name)))
  
  # Group by village and the current variable, then calculate proportions
  village_data <- strat_design %>%
    group_by(village, !!sym(variable_name)) %>%
    summarise(proportion = survey_mean(vartype = "ci", na.rm = TRUE), n = unweighted(n()))
  
  # Generate the plot
  ggplot(village_data, aes(x = village, y = proportion, group = !!sym(variable_name), fill = !!sym(variable_name))) +
    geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
    geom_errorbar(aes(ymax = pmin(proportion_upp, 1), ymin = pmax(proportion_low, 0)),
                  position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
    guides(fill = guide_legend(title = NULL)) +
    scale_fill_manual(values = c("#A9CCE3",  "#2E86C1", "#F5B7B1", "#D091BB", "#BBD4A6", "#FAD7A0", "#DFDFDF", SEA_palette)) +
    labs(title = paste("Proportion of", variable_name, "by Village"), x = "Village", y = "Proportion of Households") +
    scale_y_continuous(limits = c(0, 1)) +
    theme_minimal()}

# Generate plots for each variable
plots <- lapply(livelihood_credit_to_plot, function(var) livelihood_credit_survey_variable(var))

# Save each plot in the "images" folder 
for (i in seq_along(livelihood_credit_to_plot)) {
  file_name <- here::here("images", paste0(livelihood_credit_to_plot[i], ".png"))
  ggsave(filename = file_name, plot = plots[[i]], width = 10, height = 7)}


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

# Relationship between age and familiarity with family planning, wanting more children and

# the ideal number of children 

# Antenatal care: number of visits 

# Assistance at birth 
