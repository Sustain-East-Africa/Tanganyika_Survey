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

# import cleaned Tangnayika household survey data with --------------------------------- 
rm(list=ls())

tanganyika_clean <- readRDS("tanganyika_clean.rds")
head(tanganyika_clean)

# Set up Survey Design
tanganyika_survey <- tanganyika_clean %>% 
  as_survey_design(strata=stype, fpc=fpc, variables = c(drinking_water_dry, other_drinking_water_dry, water_treatment_dry, treatment_method_dry, other_water_treatment_dry, drinking_water_wet, 
                                                        other_drinking_water_wet, water_treatment_wet, treatment_method_wet, toilet_facilities, other_toilet_facilities, 
                                                        shared_facilities, handwashing_place, handwashing_show)) 


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
