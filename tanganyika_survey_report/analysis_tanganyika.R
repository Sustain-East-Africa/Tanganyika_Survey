library(tidyverse)
library(lubridate)
library(openxlsx)
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

# import cleaned household survey data with wealth index --------------------------------- 
rm(list=ls())

#hhs_wealth <- readRDS("hhs_cleaned_wealth.rds")
hhs_clean <- readRDS("hhs_cleaned.rds")
head(hhs_clean)

#Set survey design
hhs_survey <- as_survey_design(hhs_clean)

#Sustain EA colour pallette
my_palette <- c("#d77e5e", "#a4b792", "#e6e7e2", "#3d5919", "#202C39", "#381D2A", "#000000","#202C39", "#d77e5e")

#Another attempt at looping through the columns - sort of working --------------------------------- 
variables <- colnames(hhs_clean)[c(5:7, 9, 11:13, 15:16, 19:25, 27:62)] #Select columns of interest (excluding NA & numeric)

#Initialize list object to store plots
plots <- list()

#Loop over variables to create summaries and plots
for (variable in variables) {
  by_variable <- hhs_survey %>% 
    filter(!is.na(!!sym(variable))) %>%
    group_by_at(vars(!!sym(variable))) %>%  
    summarise(
      proportion = survey_mean(vartype = "ci", na.rm = TRUE),
      total = survey_total(vartype = "ci", na.rm = TRUE),
      n = unweighted(n())) %>%
    ungroup()
  
  #Write summary statistics
  write.xlsx(by_variable, here::here("summary_stats", paste0(variable, "_summary_stats.xlsx")))
  
  p <- ggplot(by_variable, aes_string(x = variable, y = "proportion")) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = my_palette) +
    labs(title = paste("Proportion by", variable),
         x = variable,
         y = "Proportion") +
    theme_sjplot() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    theme(legend.position = c(0.85, 0.85))
  
  plots[[variable]] <- p #Store the plot in the list
  
  ggsave(filename = here::here("images/plots_raw", paste0(variable, ".png")), width = 1000, height = 1000, dpi = 140, units = "px")
}


# Display all the plots
for (variable in variables) {
  print(plots[[variable]])
}

data <- readRDS("hhs_cleaned.rds")

--------------------------------------------------------------------------------
# HOUSEHOLD AND GENERAL INFO

# Q2. relationship to HH head ------------------------------------------------------ 
by_relation <- hhs_survey %>% 
  group_by(relation_hh_head) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_relation <- by_relation %>%
  select(relation_hh_head, percentage, total)

ggplot(by_relation, aes(x = relation_hh_head, y = percentage, group = relation_hh_head, fill = relation_hh_head)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Relationship of respondent to the household head", x = "Relationship", y = "Percentage") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "relation_hh_head.png"), width = 1000, height = 600, dpi = 140, units = "px")
write.xlsx(by_relation, here::here("images", "relation_hh_head.xlsx"))


# Q3. gender ----------------------------------------------------------------------- 
by_gender <- hhs_survey %>% 
  group_by(gender) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_gender <- by_gender %>%
  select(gender, percentage, total)

ggplot(by_gender, aes(x = gender, y = percentage, group = gender, fill = gender)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Gender of respondents", x = "Gender of Respondents", y = "Percentage") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "gender.png"), width = 1000, height = 600, dpi = 140, units = "px")
write.xlsx(by_gender, here::here("images", "gender.xlsx"))

# Q4. age -------------------------------------------------------------------------- 
by_age <- hhs_survey %>% 
  group_by(age, education) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n()))  %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_age <- by_age %>%
  select(age, education, percentage, total)

ggplot(by_age, aes(x = age, y = percentage, group = education, fill = education)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Level of Education", x = "Age of Respondents", y = "Percentage") +
  theme_sjplot() + 
  theme(legend.position = "bottom") 

ggsave(filename = here::here("images", "education_age.png"), width = 1800, height = 900, dpi = 140, units = "px")
write.xlsx(by_age, here::here("images", "education_age.xlsx"))


# Q5. highest education completed by respondent age >= 5 yrs -----------------------------------
  ### all gender ###
  by_edu <- hhs_survey %>%
  filter(age >=5) %>%  #will have to test this with real data if it works
  group_by(education) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_edu <- by_edu %>%
  select(education, percentage, total)

ggplot(by_edu, aes(x = education, y = percentage, group = education, fill = education)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Gender of respondents", x = "Gender of Respondents", y = "Percentage") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "education.png"), width = 1000, height = 600, dpi = 140, units = "px")
write.xlsx(by_edu, here::here("images", "education.xlsx"))
  
  ### by gender ###
  by_gender_edu <- hhs_survey %>% 
  group_by(gender, education) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_gender_edu <- by_gender_edu %>%
  select(gender, education, percentage, total)

ggplot(by_gender_edu, aes(x = gender, y = percentage, group = education, fill = education)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Level of Education", x = "Gender", y = "Percentage") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "education_gender.png"), width = 1200, height = 700, dpi = 140, units = "px")
write.xlsx(by_gender_edu, here::here("images", "education_gender.xlsx"))

# Q6. School attendance, age 5-25 ---------------------------------------------------
         ### all gender###
by_sch_att <- hhs_survey %>%
  filter(age >=5 & age <= 25) %>%  #will have to test this with real data if it works
  group_by(school_att) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_sch_att <- by_sch_att %>%
  select(school_att, percentage, total)

ggplot(by_sch_att, aes(x = school_att, y = percentage, group = school_att, fill = school_att)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Respondents attending school this year", x = "Respondents", y = "Percentage") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "school_att.png"), width = 1000, height = 600, dpi = 140, units = "px")
write.xlsx(by_sch_att, here::here("images", "school_att.xlsx"))

        ### by gender###
by_gender_sch_att <- hhs_survey %>%
  filter(age >=5 & age <= 25) %>%  #will have to test this with real data if it works
  group_by(gender, school_att) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_gender_sch_att <- by_gender_sch_att %>%
  select(gender, school_att, percentage, total)

ggplot(by_gender_sch_att, aes(x = gender, y = percentage, group = school_att, fill = school_att)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Respondents attending school this year", x = "Gender of Respondents", y = "Percentage") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "gender_school_att.png"), width = 1000, height = 600, dpi = 140, units = "px")
write.xlsx(by_gender_sch_att, here::here("images", "gender_school_att.xlsx"))

# Q7. Present marital status of repondents (age  >= 15)-----------------------------------------

by_marital_stat <- hhs_survey %>% 
  filter(age >= 15) %>%
  group_by(marital_stat) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm = TRUE),
            total = survey_total(vartype = "ci", na.rm = TRUE),
            n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_marital_stat <- by_marital_stat %>%
  select(marital_stat, percentage, total)

ggplot(by_marital_stat, aes(x = marital_stat, y = percentage, group = marital_stat, fill = marital_stat)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Present Marital Status", x = "Status", y = "Percentage") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "marital_stat.png"), width = 900, height = 700, dpi = 140, units = "px")
write.xlsx(by_marital_stat, here::here("images", "marital_stat.xlsx"))

# Q8. Main economic activity of repondents (age  >= 15)-----------------------------------------

by_activity <- hhs_survey %>% 
  filter(age >= 15) %>%
  group_by(activity) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm = TRUE),
            total = survey_total(vartype = "ci", na.rm = TRUE),
            n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_activity <- by_activity %>%
  select(activity, percentage, total)

ggplot(by_activity, aes(x = activity, y = percentage, group = activity, fill = activity)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Main economic activity", x = "Activity", y = "Percentage") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "activity.png"), width = 900, height = 700, dpi = 140, units = "px")
write.xlsx(by_activity, here::here("images", "activity.xlsx"))


# Q9. Is the respondent engaged in fishing full-time or part-time? (age  >= 15)-----------------------------------------
by_fishing <- hhs_survey %>% 
  filter(age >= 15) %>%
  group_by(fishing) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm = TRUE),
            total = survey_total(vartype = "ci", na.rm = TRUE),
            n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_fishing <- by_fishing %>%
  select(fishing, percentage, total)

ggplot(by_fishing, aes(x = fishing, y = percentage, group = fishing, fill = fishing)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Respondents engaged in fishing activity", x = "Fishing", y = "Percentage") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "fishing.png"), width = 900, height = 700, dpi = 140, units = "px")
write.xlsx(by_fishing, here::here("images", "fishing.xlsx"))

# Q12. Was the household head born in Kirando, Itete,Kipili or Mkinga ward?-----------------------------------------
by_hh_head_born <- hhs_survey %>% 
  group_by(hh_head_born) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm = TRUE),
            total = survey_total(vartype = "ci", na.rm = TRUE),
            n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_hh_head_born <- by_hh_head_born %>%
  select(hh_head_born, percentage, total)

ggplot(by_hh_head_born, aes(x = hh_head_born, y = percentage, group = hh_head_born, fill = hh_head_born)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Was the household head born in Kirando, Itete, Kipili or Mkinga Ward?", x = " ", y = "Percentage") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "hh_head_born.png"), width = 900, height = 700, dpi = 140, units = "px")
write.xlsx(by_hh_head_born, here::here("images", "hh_head_born.xlsx"))

# Q13. How many years has the household head lived here?-----------------------------------------
by_years_lived <- hhs_survey %>% 
  group_by(years_lived) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm = TRUE),
            total = survey_total(vartype = "ci", na.rm = TRUE),
            n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_years_lived <- by_years_lived %>%
  select(years_lived, percentage, total)

ggplot(by_years_lived, aes(x = years_lived, y = percentage, group = years_lived, fill = years_lived)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Years lived by household head in the area", x = "Years", y = "Percentage") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "years_lived.png"), width = 900, height = 700, dpi = 140, units = "px")
write.xlsx(by_years_lived, here::here("images", "years_lived.xlsx"))


# Q14. Main source of drinking water------------------------------------------------
by_water_source_dry <- hhs_survey %>% 
  group_by(water_source_dry) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm = TRUE),
            total = survey_total(vartype = "ci", na.rm = TRUE),
            n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_water_source_dry <- by_water_source_dry %>%
  select(water_source_dry, percentage, total)

ggplot(by_water_source_dry, aes(x = water_source_dry, y = percentage, group = water_source_dry, fill = water_source_dry)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Main source of drinking water in dry season", x = "Source", y = "Percentage") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "water_source_dry.png"), width = 900, height = 700, dpi = 140, units = "px")
write.xlsx(by_water_source_dry, here::here("images", "water_source_dry.xlsx"))

# Q15. Do you do anything to the water to make safer to drink in the dry season?------------------------------------------------
by_water_safe_dry <- hhs_survey %>% 
  group_by(water_safe_dry) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm = TRUE),
            total = survey_total(vartype = "ci", na.rm = TRUE),
            n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_water_safe_dry <- by_water_safe_dry %>%
  select(water_safe_dry, percentage, total)

ggplot(by_water_safe_dry, aes(x = water_safe_dry, y = percentage, group = water_safe_dry, fill = water_safe_dry)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Do you make water safe to drink in dry season?", x = " ", y = "Percentage") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "water_safe_dry.png"), width = 900, height = 700, dpi = 140, units = "px")
write.xlsx(by_water_safe_dry, here::here("images", "water_safe_dry.xlsx"))

# Q16. What do you usually do to make the water safer to drink?------------------------------------------------
by_water_safe_action_dry <- hhs_survey %>% 
  group_by(water_safe_action_dry) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm = TRUE),
            total = survey_total(vartype = "ci", na.rm = TRUE),
            n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_water_safe_action_dry <- by_water_safe_action_dry %>%
  select(water_safe_action_dry, percentage, total)

ggplot(by_water_safe_action_dry, aes(x = water_safe_action_dry, y = percentage, group = water_safe_action_dry, fill = water_safe_action_dry)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Do you make water safe to drink in dry season?", x = " ", y = "Percentage") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "water_safe_action_dry.png"), width = 900, height = 700, dpi = 140, units = "px")
write.xlsx(by_water_safe_action_dry, here::here("images", "water_safe_action_dry.xlsx"))

# Q17. Main source of drinking water during wet season
by_water_source_wet <- hhs_survey %>% 
  group_by(water_source_wet) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm = TRUE),
            total = survey_total(vartype = "ci", na.rm = TRUE),
            n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_water_source_wet <- by_water_source_wet %>%
  select(water_source_wet, percentage, total)

ggplot(by_water_source_wet, aes(x = water_source_wet, y = percentage, group = water_source_wet, fill = water_source_wet)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Main source of drinking water in dry season", x = "Source", y = "Percentage") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "water_source_wet.png"), width = 900, height = 700, dpi = 140, units = "px")
write.xlsx(by_water_source_wet, here::here("images", "water_source_wet.xlsx"))

# Q18. Do you do anything to the water to make it safer to drink in the wet season? 
by_water_safe_wet <- hhs_survey %>% 
  group_by(water_safe_wet) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm = TRUE),
            total = survey_total(vartype = "ci", na.rm = TRUE),
            n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_water_safe_wet <- by_water_safe_wet %>%
  select(water_safe_wet, percentage, total)

ggplot(by_water_safe_wet, aes(x = water_safe_wet, y = percentage, group = water_safe_wet, fill = water_safe_wet)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Do you make water safe to drink in dry season?", x = " ", y = "Percentage") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "water_safe_wet.png"), width = 900, height = 700, dpi = 140, units = "px")
write.xlsx(by_water_safe_wet, here::here("images", "water_safe_wet.xlsx"))

# Q19. What do you usually do to make the water safer to drink? 
by_water_safe_action_wet <- hhs_survey %>% 
  group_by(water_safe_action_wet) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm = TRUE),
            total = survey_total(vartype = "ci", na.rm = TRUE),
            n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_water_safe_action_wet <- by_water_safe_action_wet %>%
  select(water_safe_action_wet, percentage, total)

ggplot(by_water_safe_action_wet, aes(x = water_safe_action_wet, y = percentage, group = water_safe_action_wet, fill = water_safe_action_wet)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "How do you make water safe to drink in dry season?", x = " ", y = "Percentage") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "water_safe_action_wet.png"), width = 900, height = 700, dpi = 140, units = "px")
write.xlsx(by_water_safe_action_wet, here::here("images", "water_safe_action_wet.xlsx"))

#Q20. most advanced toilet facility used by members of your household?
by_toilet_facility <- hhs_survey %>% 
  group_by(toilet_facility) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm = TRUE),
            total = survey_total(vartype = "ci", na.rm = TRUE),
            n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_toilet_facility <- by_toilet_facility %>%
  select(toilet_facility, percentage, total)

ggplot(by_toilet_facility, aes(x = toilet_facility, y = percentage, group = toilet_facility, fill = toilet_facility)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Most advanced toilet facility?", x = "Type of toilet", y = "Percentage") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "toilet_facility.png"), width = 900, height = 700, dpi = 140, units = "px")
write.xlsx(by_toilet_facility, here::here("images", "toilet_facility.xlsx"))




#PPI – average PPI of women, youth, compared to men. Are women more likely to be poor than men, and same for young people?

#I am also wondering if by combining responses to gender and household head (q2 + q4), 
#we can compare female headed households to male headed households quickly and easily on 
#hunger, ability to send kids to school, ability to pay for health etc?

#At the moment, how often does your household have to skip meals because of a lack of income?
by_hh_gender <- hhs_survey %>%
  filter(!is.na(hh_gender)) %>%
  group_by(hh_gender, hh_skip_meals) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) 

ggplot(by_hh_gender, aes(x = hh_gender, y = proportion, group = fct_relevel(hh_skip_meals, c("Some days in a week", "Some days in every month", "A few days in the worst months", "Never")), fill = fct_relevel(hh_skip_meals, c("Some days in a week", "Some days in every month", "A few days in the worst months", "Never")))) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "At the moment, how often does your household have to skip meals 
because of a lack of income?", x = "Gender of Household Head", y = "Proportion of Respondents") +
  theme_sjplot() + 
  theme(legend.position = c(0.85, 0.85))

ggsave(filename = here::here("images/hh_head", "hh_skip_meals_hh_gender.png"), width = 1200, height = 600, dpi = 140, units = "px")
write.xlsx(by_hh_gender, here::here("images/hh_head", "hh_skip_meals_hh_gender.xlsx"))

