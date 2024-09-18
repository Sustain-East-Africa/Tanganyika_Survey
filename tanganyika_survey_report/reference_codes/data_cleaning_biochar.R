library(tidyverse)
library(lubridate)
library(scales)
library(googlesheets4)
library(stats)
library(stats4)
library(survey)
library(srvyr, warn.conflicts = FALSE)
library(sjPlot)
library(dplyr)
library(leaflet)

ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)

######################################################################################################################
####### run data cleaning code for biochar household survey
######################################################################################################################
#  -99 indicates don't know and these are converted to NA in continuous or "Don't know" in categorical   #######

rm(list=ls())
# bring in sheet ----------------------------------------------------------
gs4_deauth()

hhs<-googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1OO413RIG-Nxvc1ZxT9Rdjxb_kCIA9Yhu5JmyNWgcPno/edit#gid=0" , sheet = 1) %>%
  as.data.frame()

hhs_codes <-googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1OO413RIG-Nxvc1ZxT9Rdjxb_kCIA9Yhu5JmyNWgcPno/edit#gid=0" , sheet = 2) %>%
  as.data.frame() %>%
  select(column_titles)
 
# transpose the column of codes to replace the header row
new_header <- as.character(hhs_codes$column_titles)
colnames(hhs) <- new_header
 
hhs <- hhs %>% 
select(-c("intro", "over","issues"))

 
saveRDS(hhs, "hhs_raw.rds")

########################################################
# set all data types correctly before importation
########################################################
hhs <- readRDS("hhs_raw.rds")

hhs$date_1 <- lubridate::ymd(hhs$date_1) #convert to date

#Reduce length of options 
 hhs_clean <- hhs %>% 
 mutate(where_from = case_when(
   where_from == "I come from this county" ~ "This county",
   where_from == "I come from another county but have moved here permanently because of work/family" 
                 ~ "Another but moved here permanently due to work",
   where_from == "I come from another county but have moved here temporarily for work/family reasons" 
                 ~ "Another but moved here temporarily due to work"
  )) %>%
  mutate(employment_status_before = case_when(
    employment_status_before == "Before this job I had better income than the job I have on this farm (more income, more stable)" 
                               ~ "more income, more stable",
    employment_status_before == "Before this job I had fewer source of income (Underemployed)"
                               ~ "Underemployed",
    employment_status_before == "Before this job I had no source of income from a job/business (Unemployed)" 
                               ~ "Unemployed",
    employment_status_before == "Before this job I had similar income to this current job (pay and hours)" 
                              ~ "Similar pay and hours"
   )) %>%
   mutate(subsid_food_improv_hunger = case_when(
     subsid_food_improv_hunger == "It has improved my hunger/nutritional status greatly" 
     ~ "It has improved my nutritional status greatly",
     subsid_food_improv_hunger == "It has improved my hunger/nutritional status to some extent"
     ~ "It has improved my nutritional status to some extent",
     subsid_food_improv_hunger == "It has not changed my hunger/nutritional status at all" 
     ~ "It has not changed my nutritional status at all",
     subsid_food_improv_hunger == "NA/I don’t use the canteen" 
     ~ "I don't use the canteen"
   )) %>%
   mutate(no_dependents = ifelse(no_dependents == "NA - Single person", 0, no_dependents
   )) %>%
   mutate(hh_hunger_changed = ifelse(hh_hunger_changed == "Greatly reduced", "Greatly decreased", hh_hunger_changed
   )) %>%
   mutate(youth = ifelse(age %in% c("18-25", "26-35"), "Youth", "Older"
   ))%>%
   mutate(hh_gender = ifelse(hh_head == "Yes", paste(gender, "household head"), NA))
 

#rename("elec" = "main-elec")
saveRDS(hhs_clean, "hhs_cleaned.rds")

########################################################################################################################################
#########################################################################################################################################
# can end data cleaning here
######################################################################################################################################### 
########################################################################################################################################

