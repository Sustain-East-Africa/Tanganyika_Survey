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
variables <- colnames(hhs_clean)[c(5:7, 9, 11:13, 15:16, 19:25, 27:62)] #Select columns (excluding NA & numeric)

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

# gender --------------------------------- 
by_location <- hhs_survey %>% 
  group_by(gender) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_location <- by_location %>%
  select(gender, percentage, total)

ggplot(by_location, aes(x = gender, y = percentage, group = gender, fill = gender)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Gender of respondents", x = "Gender of Respondents", y = "Percentage of Households") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "gender.png"), width = 1000, height = 600, dpi = 140, units = "px")
write.xlsx(by_location, here::here("images", "gender.xlsx"))

# age --------------------------------- 
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
  labs(title = "Level of Education", x = "Age of Respondents", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "bottom") 

ggsave(filename = here::here("images", "education_age.png"), width = 1800, height = 900, dpi = 140, units = "px")
write.xlsx(by_age, here::here("images", "education_age.xlsx"))

### Disaggregate by Gender and Youth ###

#Q4. Household head 
by_gender <- hhs_survey %>% 
  group_by(gender, hh_head) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm = TRUE),
            total = survey_total(vartype = "ci", na.rm = TRUE),
            n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_gender <- by_gender %>%
  select(gender, hh_head, percentage, total)

ggplot(by_gender, aes(x = gender, y = percentage, group = hh_head, fill = hh_head)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Are you the household head?", x = "Gender", y = "Percentage of Households") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "hh_head_gender.png"), width = 900, height = 700, dpi = 140, units = "px")
write.xlsx(by_gender, here::here("images", "hh_head_gender.xlsx"))

#Q5. Number of dependents
# by gender (total no. dependents)

hhs_survey <- hhs_survey %>%
  mutate(no_dependents = as.numeric(no_dependents))

# Summarize the total number of dependents by gender using survey design
by_gender_dependents <- hhs_survey %>%
  group_by(gender) %>%
  summarise(
    total_dependents = survey_total(no_dependents, vartype = "ci", na.rm = TRUE)
  )

# Create a dataframe from the summarised survey data
by_gender_dependents_total <- data.frame(
  gender = by_gender_dependents$gender,
  total_dependents = by_gender_dependents$total_dependents
)

ggplot(by_gender_dependents_total, aes(x = gender, y = total_dependents, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.95) +
  scale_fill_manual(values = my_palette) +
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Total Number of Dependents by Gender", x = "Gender", y = "Total Number of Dependents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "no_dependents_gender.png"), width = 1200, height = 700, dpi = 140, units = "px")
write.xlsx(by_gender_dependents_total, here::here("images", "no_dependents_gender.xlsx"))

# by gender (average no. dependents)
hhs_survey <- hhs_survey %>%
  mutate(no_dependents = as.numeric(no_dependents))

# Summarize the average number of dependents by gender using survey design
by_gender_dependents_m <- hhs_survey %>%
  group_by(gender) %>%
  summarise(
    mean_dependents = survey_mean(no_dependents, vartype = "ci", na.rm = TRUE)
  )

# Create a dataframe from the summarised survey data
by_gender_dependents_average <- data.frame(
  gender = by_gender_dependents_m$gender,
  mean_dependents = by_gender_dependents_m$mean_dependents
)

ggplot(by_gender_dependents_average, aes(x = gender, y = mean_dependents, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.95) +
  scale_fill_manual(values = my_palette) +
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Average Number of Dependents by Gender", x = "Gender", y = "Average Number of Dependents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "mean_dependents_gender.png"), width = 1200, height = 700, dpi = 140, units = "px")
write.xlsx(by_gender_dependents_average, here::here("images", "mean_dependents_gender.xlsx"))

#Q6. Level of education – disaggregate by gender and youth (18-25+26-35).
# by gender 
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
  labs(title = "Level of Education", x = "Gender", y = "Proportion of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "education_gender.png"), width = 1200, height = 700, dpi = 140, units = "px")
write.xlsx(by_gender_edu, here::here("images", "education_gender.xlsx"))

#by youth
by_youth_edu <- hhs_survey %>%
  group_by(youth, education) %>%
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_youth_edu <- by_youth_edu %>%
  select(youth, education, percentage, total)

ggplot(by_youth_edu, aes(x = youth, y = percentage, group = education, fill = education)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Level of Education", x = "Age: Youth (Aged 18-35) or Older", y = "Proportion of Respondents") +
  theme_sjplot() +
  theme(legend.position = "right")

ggsave(filename = here::here("images", "education_youth.png"), width = 1200, height = 700, dpi = 140, units = "px")
write.xlsx(by_youth_edu, here::here("images", "education_youth.xlsx"))

#Q7. Where do you come from?
# by gender 
by_gender_where_from <- hhs_survey %>% 
  group_by(gender, where_from) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_gender_where_from <- by_gender_where_from %>%
  select(gender, where_from, percentage, total)

ggplot(by_gender_where_from, aes(x = gender, y = percentage, group = where_from, fill = where_from)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Where Respondents Come From", x = "Gender", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "where_from_gender.png"), width = 1200, height = 600, dpi = 140, units = "px")
write.xlsx(by_gender_where_from, here::here("images", "where_from_gender.xlsx"))

#by youth
by_youth_where_from <- hhs_survey %>% 
  group_by(youth, where_from) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_youth_where_from <- by_youth_where_from %>%
  select(youth, where_from, percentage, total)

ggplot(by_youth_where_from, aes(x = youth, y = percentage, group = where_from, fill = where_from)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Where Respondents Come From", x = "Age: Youth (Aged 18-35) or Older", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "where_from_youth.png"), width = 1200, height = 700, dpi = 140, units = "px")
write.xlsx(by_youth_where_from, here::here("images", "where_from_youth.xlsx"))

#Q8. Main job on the farm 
#by gender
by_gender_main_job <- hhs_survey %>% 
  group_by(gender, main_job) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_gender_main_job <- by_gender_main_job %>%
  select(gender, main_job, percentage, total)

ggplot(by_gender_main_job, aes(x = gender, y = percentage, group = main_job, fill = main_job)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  #scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Main Job on the Farm", x = "Gender", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "main_job_gender.png"), width = 1200, height = 700, dpi = 140, units = "px")
write.xlsx(by_gender_main_job, here::here("images", "main_job_gender.xlsx"))

#by youth
by_youth_main_job <- hhs_survey %>% 
  group_by(youth, main_job) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_youth_main_job <- by_youth_main_job %>%
  select(youth, main_job, percentage, total)

ggplot(by_youth_main_job, aes(x = youth, y = percentage, group = main_job, fill = main_job)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  #scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Main Job on the Farm", x = "Age: Youth (Aged 18-35) or Older", y = "Proportion of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "main_job_youth.png"), width = 1200, height = 700, dpi = 140, units = "px")
write.xlsx(by_youth_main_job, here::here("images", "main_job_youth.xlsx"))

#Q9. How long have you been employed in this job?
# by gender 
by_gender_length_employed <- hhs_survey %>% 
  group_by(gender, length_employed) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_gender_length_employed <- by_gender_length_employed %>%
  select(gender, length_employed, percentage, total)

ggplot(by_gender_length_employed, aes(x = gender, y = percentage, group = length_employed, fill = length_employed)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  #scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Length of Employment", x = "Gender", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "length_employed_gender.png"), width = 1200, height = 700, dpi = 140, units = "px")
write.xlsx(by_gender_length_employed, here::here("images", "length_employed_gender.xlsx"))

#by youth
by_youth_length_employed <- hhs_survey %>% 
  group_by(youth, length_employed) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_youth_length_employed <- by_youth_length_employed %>%
  select(youth, length_employed, percentage, total)

ggplot(by_youth_length_employed, aes(x = youth, y = percentage, group = length_employed, fill = length_employed)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  #scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Length of Employment", x = "Youth", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "length_employed_youth.png"), width = 1200, height = 700, dpi = 140, units = "px")
write.xlsx(by_youth_length_employed, here::here("images", "length_employed_youth.xlsx"))

# Q10.What is your employment contract/status?
#by gender
by_gender_employment_contract <- hhs_survey %>% 
  group_by(gender, employment_contract) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_gender_employment_contract <- by_gender_employment_contract %>%
  select(gender, employment_contract, percentage, total) 

ggplot(by_gender_employment_contract, aes(x = gender, y = percentage, group = employment_contract, fill = employment_contract)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "What is your employment contract/status?", x = "Gender", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "bottom")

ggsave(filename = here::here("images", "employment_contract_gender.png"), width = 1200, height = 600, dpi = 140, units = "px")
write.xlsx(by_gender_employment_contract, here::here("images", "employment_contract_gender.xlsx"))

#by youth
by_youth_employment_contract <- hhs_survey %>% 
  group_by(youth, employment_contract) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_youth_employment_contract <- by_youth_employment_contract %>%
  select(youth, employment_contract, percentage, total) 

ggplot(by_youth_employment_contract, aes(x = youth, y = percentage, group = employment_contract, fill = employment_contract)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "What is your employment contract/status?", x = "Age: Youth (Aged 18-35) or Older", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "bottom")

ggsave(filename = here::here("images", "employment_contract_youth.png"), width = 1200, height = 600, dpi = 140, units = "px")
write.xlsx(by_youth_employment_contract, here::here("images", "employment_contract_youth.xlsx"))

# Q11. How many hours a week do you work on the farm, on average? 
#by gender
by_gender_hours_per_week_worked <- hhs_survey %>% 
  group_by(gender, hours_per_week_worked) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1),
    hours_per_week_worked = factor(hours_per_week_worked)  
  )
  

# Remove unwanted columns
by_gender_hours_per_week_worked <- by_gender_hours_per_week_worked %>%
  select(gender, hours_per_week_worked, percentage, total) 

ggplot(by_gender_hours_per_week_worked, aes(x = gender, y = percentage, group = hours_per_week_worked, fill = hours_per_week_worked)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "How many hours a week do you work on average?", x = "Gender", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "hours_per_week_worked_gender.png"), width = 1200, height = 600, dpi = 140, units = "px")
write.xlsx(by_gender_hours_per_week_worked, here::here("images", "hours_per_week_worked_gender.xlsx"))

#by youth
by_youth_hours_per_week_worked <- hhs_survey %>% 
  group_by(youth, hours_per_week_worked) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1),
    hours_per_week_worked = factor(hours_per_week_worked)  
  )

# Remove unwanted columns
by_youth_hours_per_week_worked <- by_youth_hours_per_week_worked %>%
  select(youth, hours_per_week_worked, percentage, total) 

ggplot(by_youth_hours_per_week_worked, aes(x = youth, y = percentage, group = hours_per_week_worked, fill = hours_per_week_worked)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "How many hours a week do you work on average?", x = "Age: Youth (Aged 18-35) or Older", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "hours_per_week_worked_youth.png"), width = 1200, height = 600, dpi = 140, units = "px")
write.xlsx(by_youth_hours_per_week_worked, here::here("images", "hours_per_week_worked_youth.xlsx"))

# Q13 What was your employment status before this job?
#by gender
by_gender_employment_status_before <- hhs_survey %>% 
  group_by(gender, employment_status_before) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_gender_employment_status_before <- by_gender_employment_status_before %>%
  select(gender, employment_status_before, percentage, total) 

ggplot(by_gender_employment_status_before, aes(x = gender, y = percentage, group = employment_status_before, fill = employment_status_before)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "What was your employment status before this job?", x = "Gender", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "employment_status_before_gender.png"), width = 1200, height = 900, dpi = 140, units = "px")
write.xlsx(by_gender_employment_status_before, here::here("images", "employment_status_before_gender.xlsx"))

#by youth
by_youth_employment_status_before <- hhs_survey %>% 
  group_by(youth, employment_status_before) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_youth_employment_status_before <- by_youth_employment_status_before %>%
  select(youth, employment_status_before, percentage, total) 

ggplot(by_youth_employment_status_before, aes(x = youth, y = percentage, group = employment_status_before, fill = employment_status_before)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "What was your employment status before this job?", x = "Age: Youth (Aged 18-35) or Older", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "employment_status_before_youth.png"), width = 1200, height = 900, dpi = 140, units = "px")
write.xlsx(by_youth_employment_status_before, here::here("images", "employment_status_before_youth.xlsx"))

#Q14. Reliance on job (job_supporting_hh_income_perc)
#by gender
by_gender_job_supporting_hh_income_perc <- hhs_survey %>% 
  group_by(gender, job_supporting_hh_income_perc) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_gender_job_supporting_hh_income_perc <- by_gender_job_supporting_hh_income_perc %>%
  select(gender, job_supporting_hh_income_perc, percentage, total) 

ggplot(by_gender_job_supporting_hh_income_perc, aes(x = gender, y = percentage, group = job_supporting_hh_income_perc, fill = job_supporting_hh_income_perc)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "How important is your job in terms of supporting the household, 
as a % of total household income (i.e. that of you and your spouse), on average?", x = "Gender", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "job_supporting_hh_income_perc_gender.png"), width = 1200, height = 700, dpi = 140, units = "px")
write.xlsx(by_gender_job_supporting_hh_income_perc, here::here("images", "job_supporting_hh_income_perc_gender.xlsx"))

#by youth
by_youth_job_supporting_hh_income_perc <- hhs_survey %>% 
  group_by(youth, job_supporting_hh_income_perc) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_youth_job_supporting_hh_income_perc <- by_youth_job_supporting_hh_income_perc %>%
  select(youth, job_supporting_hh_income_perc, percentage, total) 

ggplot(by_youth_job_supporting_hh_income_perc, aes(x = youth, y = percentage, group = job_supporting_hh_income_perc, fill = job_supporting_hh_income_perc)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "How important is your job in terms of supporting the household, 
as a % of total household income (i.e. that of you and your spouse), on average?", x = "Age: Youth (Aged 18-35) or Older", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "job_supporting_hh_income_perc_youth.png"), width = 1200, height = 700, dpi = 140, units = "px")
write.xlsx(by_youth_job_supporting_hh_income_perc, here::here("images", "job_supporting_hh_income_perc_youth.xlsx"))

#Q15. What do you earn on average on a fortnightly basis? 
#by gender
by_gender_earnings_fortnight <- hhs_survey %>% 
  group_by(gender, earnings_fortnight) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_gender_earnings_fortnight <- by_gender_earnings_fortnight %>%
  select(gender, earnings_fortnight, percentage, total) 

ggplot(by_gender_earnings_fortnight, aes(x = gender, y = percentage, group = earnings_fortnight, fill = earnings_fortnight)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "What do you earn on average on a fortnightly basis?", x = "Gender", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "earnings_fortnight_gender.png"), width = 1200, height = 700, dpi = 140, units = "px")
write.xlsx(by_gender_earnings_fortnight, here::here("images", "earnings_fortnight_gender.xlsx"))

#by youth
by_youth_earnings_fortnight <- hhs_survey %>% 
  group_by(youth, earnings_fortnight) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_youth_earnings_fortnight <- by_youth_earnings_fortnight %>%
  select(youth, earnings_fortnight, percentage, total) 

ggplot(by_youth_earnings_fortnight, aes(x = youth, y = percentage, group = earnings_fortnight, fill = earnings_fortnight)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "What do you earn on average on a fortnightly basis?", x = "Age: Youth (Aged 18-35) or Older", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "earnings_fortnight_youth.png"), width = 1200, height = 700, dpi = 140, units = "px")
write.xlsx(by_youth_earnings_fortnight, here::here("images", "earnings_fortnight_youth.xlsx"))

#Q17. What is the fortnightly income you derive from those other activities or jobs in total, on average?
#by gender
by_gender_other_job_earnings_fortnight <- hhs_survey %>% 
  group_by(gender, other_job_earnings_fortnight) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_gender_other_job_earnings_fortnight <- by_gender_other_job_earnings_fortnight %>%
  select(gender, other_job_earnings_fortnight, percentage, total) 

ggplot(by_gender_other_job_earnings_fortnight, aes(x = gender, y = percentage, group = other_job_earnings_fortnight, fill = other_job_earnings_fortnight)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "fortnightly income you derive from those other activities or jobs", x = "Gender", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "other_job_earnings_fortnight_gender.png"), width = 1200, height = 700, dpi = 140, units = "px")
write.xlsx(by_gender_other_job_earnings_fortnight, here::here("images", "other_job_earnings_fortnight_gender.xlsx"))

#by youth
by_youth_other_job_earnings_fortnight <- hhs_survey %>% 
  group_by(youth, other_job_earnings_fortnight) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_youth_other_job_earnings_fortnight <- by_youth_other_job_earnings_fortnight %>%
  select(youth, other_job_earnings_fortnight, percentage, total) 

ggplot(by_youth_other_job_earnings_fortnight, aes(x = youth, y = percentage, group = other_job_earnings_fortnight, fill = other_job_earnings_fortnight)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "fortnightly income you derive from those other activities or jobs", x = "Age: Youth (Aged 18-35) or Older", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "other_job_earnings_fortnight_youth.png"), width = 1200, height = 700, dpi = 140, units = "px")
write.xlsx(by_youth_other_job_earnings_fortnight, here::here("images", "other_job_earnings_fortnight_youth.xlsx"))

#Q18. Importance of job
#by gender
by_gender_job_importance_hh <- hhs_survey %>% 
  group_by(gender, job_importance_hh) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_gender_job_importance_hh <- by_gender_job_importance_hh %>%
  select(gender, job_importance_hh, percentage, total) 

ggplot(by_gender_job_importance_hh, aes(x = gender, y = percentage, group = job_importance_hh, fill = job_importance_hh)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "How important is this job to you and your household in terms of income?", x = "Gender", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "job_importance_hh_gender.png"), width = 1200, height = 700, dpi = 140, units = "px")
write.xlsx(by_gender_job_importance_hh, here::here("images", "job_importance_hh_gender.xlsx"))

#by youth
by_youth_job_importance_hh <- hhs_survey %>% 
  group_by(youth, job_importance_hh) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_youth_job_importance_hh <- by_youth_job_importance_hh %>%
  select(youth, job_importance_hh, percentage, total)

ggplot(by_youth_job_importance_hh, aes(x = youth, y = percentage, group = job_importance_hh, fill = job_importance_hh)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "How important is this job to you and your household in terms of income?", x = "Age: Youth (Aged 18-35) or Older", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "job_importance_hh_youth.png"), width = 1200, height = 700, dpi = 140, units = "px")
write.xlsx(by_youth_job_importance_hh, here::here("images", "job_importance_hh_youth.xlsx"))

#Q19 Often able to save 
#by gender
by_gender_savings <- hhs_survey %>% 
  group_by(gender, savings) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_gender_savings <- by_gender_savings %>%
  select(gender, savings, percentage, total)

ggplot(by_gender_savings, aes(x = gender, y = percentage, group = fct_relevel(savings, c("I am never able to save", "I am able to save at least monthly", "Other")), fill = fct_relevel(savings, c("I am never able to save", "I am able to save at least monthly", "Other")))) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "How often are you able to save the money you earn from this job on the farm?", x = "Gender", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "savings_gender.png"), width = 1200, height = 700, dpi = 140, units = "px")
write.xlsx(by_gender_savings, here::here("images", "savings_gender.xlsx"))

#by youth
by_youth_savings <- hhs_survey %>% 
  group_by(youth, savings) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_youth_savings <- by_youth_savings %>%
  select(youth, savings, percentage, total)

ggplot(by_youth_savings, aes(x = youth, y = percentage, group = fct_relevel(savings, c("I am never able to save", "I am able to save at least monthly", "Other")), fill = fct_relevel(savings, c("I am never able to save", "I am able to save at least monthly", "Other")))) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "How often are you able to save the money you earn from this job on the farm?", x = "Age: Youth (Aged 18-35) or Older", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "savings_youth.png"), width = 1200, height = 700, dpi = 140, units = "px")
write.xlsx(by_youth_savings, here::here("images", "savings_youth.xlsx"))

#Q20 Outstanding debt 
#by gender
by_gender_debts <- hhs_survey %>% 
  group_by(gender, debts) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n()))  %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_gender_debts <- by_gender_debts %>%
  select(gender, debts, percentage, total)


ggplot(by_gender_debts, aes(x = gender, y = percentage, group = debts, fill = debts)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Do you have any outstanding debts?", x = "Gender", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "debts_gender.png"), width = 800, height = 700, dpi = 140, units = "px")
write.xlsx(by_gender_debts, here::here("images", "debts_gender.xlsx"))

#by youth
by_youth_debts <- hhs_survey %>% 
  group_by(youth, debts) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n()))  %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_youth_debts <- by_youth_debts %>%
  select(youth, debts, percentage, total)

ggplot(by_youth_debts, aes(x = youth, y = percentage, group = debts, fill = debts)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Do you have any outstanding debts?", x = "Age: Youth (Aged 18-35) or Older", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "debts_youth.png"), width = 800, height = 700, dpi = 140, units = "px")
write.xlsx(by_youth_debts, here::here("images", "debts_youth.xlsx"))

#Q22. How has the total amount of debt changed since you started in this job?
# by gender
by_gender_debt_changed <- hhs_survey %>% 
  group_by(gender, debt_changed) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n()))  %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_gender_debt_changed <- by_gender_debt_changed %>%
  select(gender, debt_changed, percentage, total)

ggplot(by_gender_debt_changed, aes(x = gender, y = percentage, group = debt_changed, fill = debt_changed)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "How has the total amount of debt changed?", x = "Gender", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "debt_changed_gender.png"), width = 1200, height = 700, dpi = 140, units = "px")
write.xlsx(by_gender_debt_changed, here::here("images", "debt_changed_gender.xlsx"))

# by youth
by_youth_debt_changed <- hhs_survey %>% 
  group_by(youth, debt_changed) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n()))  %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_youth_debt_changed <- by_youth_debt_changed %>%
  select(youth, debt_changed, percentage, total)

ggplot(by_youth_debt_changed, aes(x = youth, y = percentage, group = debt_changed, fill = debt_changed)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "How has the total amount of debt changed?", x = "Age: Youth (Aged 18-35) or Older", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "debt_changed_youth.png"), width = 1200, height = 600, dpi = 140, units = "px")
write.xlsx(by_youth_debt_changed, here::here("images", "debt_changed_youth.xlsx"))


#Q24 hunger 
# by gender
by_gender_hh_skip_meals <- hhs_survey %>% 
  group_by(gender, hh_skip_meals) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n()))  %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_gender_hh_skip_meals <- by_gender_hh_skip_meals %>%
  select(gender, hh_skip_meals, percentage, total)

ggplot(by_gender_hh_skip_meals, aes(x = gender, y = percentage, group = fct_relevel(hh_skip_meals, c("Some days in a week", "Some days in every month", "A few days in the worst months", "Never")), fill = fct_relevel(hh_skip_meals, c("Some days in a week", "Some days in every month", "A few days in the worst months", "Never")))) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "At the moment, how often does your household have to skip meals 
because of a lack of income?", x = "Gender", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "hh_skip_meals_gender.png"), width = 1200, height = 700, dpi = 140, units = "px")
write.xlsx(by_gender_hh_skip_meals, here::here("images", "hh_skip_meals_gender.xlsx"))

# by youth
by_youth_hh_skip_meals <- hhs_survey %>% 
  group_by(youth, hh_skip_meals) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n()))  %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_youth_hh_skip_meals <- by_youth_hh_skip_meals %>%
  select(youth, hh_skip_meals, percentage, total)

ggplot(by_youth_hh_skip_meals, aes(x = youth, y = percentage, group = fct_relevel(hh_skip_meals, c("Some days in a week", "Some days in every month", "A few days in the worst months", "Never")), fill = fct_relevel(hh_skip_meals, c("Some days in a week", "Some days in every month", "A few days in the worst months", "Never")))) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "At the moment, how often does your household have to skip meals 
because of a lack of income?", x = "Age: Youth (Aged 18-35) or Older", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "hh_skip_meals_youth.png"), width = 1200, height = 700, dpi = 140, units = "px")
write.xlsx(by_youth_hh_skip_meals, here::here("images", "hh_skip_meals_youth.xlsx"))

#Q25 – change in hunger status -- disaggregate by gender and youth (18-25+26-35).
#by gender
by_gender_hh_hunger_changed <- hhs_survey %>% 
  group_by(gender, hh_hunger_changed) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_gender_hh_hunger_changed <- by_gender_hh_hunger_changed %>%
  select(gender, hh_hunger_changed, percentage, total) 

ggplot(by_gender_hh_hunger_changed, aes(x = gender, y = percentage, group = fct_relevel(hh_hunger_changed, c("Greatly decreased", "Somewhat decreased", "Remained stable", "Somewhat increased", "Greatly increased")), fill = fct_relevel(hh_hunger_changed, c("Greatly decreased", "Somewhat decreased", "Remained stable", "Somewhat increased", "Greatly increased")))) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "How has your household’s hunger changed since you started this job?", x = "Gender", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = c(0.85, 0.85))

ggsave(filename = here::here("images", "hh_hunger_changed_gender.png"), width = 1200, height = 600, dpi = 140, units = "px")
write.xlsx(by_gender_hh_hunger_changed, here::here("images", "hh_hunger_changed_gender.xlsx"))

#by youth
by_youth_hh_hunger_changed <- hhs_survey %>% 
  group_by(youth, hh_hunger_changed) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_youth_hh_hunger_changed <- by_youth_hh_hunger_changed %>%
  select(youth, hh_hunger_changed, percentage, total) 

ggplot(by_youth_hh_hunger_changed, aes(x = youth, y = percentage, group = fct_relevel(hh_hunger_changed, c("Greatly decreased", "Somewhat decreased", "Remained stable", "Somewhat increased", "Greatly increased")), fill = fct_relevel(hh_hunger_changed, c("Greatly decreased", "Somewhat decreased", "Remained stable", "Somewhat increased", "Greatly increased")))) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "How has your household’s hunger changed since you started this job?", x = "Age: Youth (Aged 18-35) or Older", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = c(0.85, 0.85))

ggsave(filename = here::here("images", "hh_hunger_changed_youth.png"), width = 1200, height = 600, dpi = 140, units = "px")
write.xlsx(by_youth_hh_hunger_changed, here::here("images", "hh_hunger_changed_youth.xlsx"))

#Q27 – subsidised food impact 
#by gender
by_gender_subsid_food_improv_hunger <- hhs_survey %>% 
  group_by(gender, subsid_food_improv_hunger) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_gender_subsid_food_improv_hunger <- by_gender_subsid_food_improv_hunger %>%
  select(gender, subsid_food_improv_hunger, percentage, total)  

ggplot(by_gender_subsid_food_improv_hunger, aes(x = gender, y = percentage, group = subsid_food_improv_hunger, fill = subsid_food_improv_hunger)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "To what extent has the subsidised food from the canteen improved your 
hunger or nutritional status?", x = "Gender", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "subsid_food_improv_hunger_gender.png"), width = 1200, height = 700, dpi = 140, units = "px")
write.xlsx(by_gender_subsid_food_improv_hunger, here::here("images", "subsid_food_improv_hunger_gender.xlsx"))

#by youth
by_youth_subsid_food_improv_hunger <- hhs_survey %>% 
  group_by(youth, subsid_food_improv_hunger) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_youth_subsid_food_improv_hunger <- by_youth_subsid_food_improv_hunger %>%
  select(youth, subsid_food_improv_hunger, percentage, total)  

ggplot(by_youth_subsid_food_improv_hunger, aes(x = youth, y = percentage, group = subsid_food_improv_hunger, fill = subsid_food_improv_hunger)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "To what extent has the subsidised food from the canteen improved your 
hunger or nutritional status?", x = "Age: Youth (Aged 18-35) or Older", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "subsid_food_improv_hunger_youth.png"), width = 1200, height = 700, dpi = 140, units = "px")
write.xlsx(by_youth_subsid_food_improv_hunger, here::here("images", "subsid_food_improv_hunger_youth.xlsx"))

#28. To what extent has the uji you receive improved your hunger or nutritional status? 
#by gender
by_gender_uji_improv_hunger <- hhs_survey %>% 
  group_by(gender, uji_improv_hunger) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_gender_uji_improv_hunger <- by_gender_uji_improv_hunger %>%
  select(gender, uji_improv_hunger, percentage, total) 

ggplot(by_gender_uji_improv_hunger, aes(x = gender, y = percentage, group = uji_improv_hunger, fill = uji_improv_hunger)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "To what extent has the uji you receive improved your hunger?", x = "Gender", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = c(0.85, 1.0))

ggsave(filename = here::here("images", "uji_improv_hunger_gender.png"), width = 1200, height = 700, dpi = 140, units = "px")
write.xlsx(by_gender_uji_improv_hunger, here::here("images", "uji_improv_hunger_gender.xlsx"))

#by youth
by_youth_uji_improv_hunger <- hhs_survey %>% 
  group_by(youth, uji_improv_hunger) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_youth_uji_improv_hunger <- by_youth_uji_improv_hunger %>%
  select(youth, uji_improv_hunger, percentage, total) 

ggplot(by_youth_uji_improv_hunger, aes(x = youth, y = percentage, group = uji_improv_hunger, fill = uji_improv_hunger)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "To what extent has the uji you receive improved your hunger?", x = "Youth", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = c(0.85, 1.0))

ggsave(filename = here::here("images", "uji_improv_hunger_youth.png"), width = 1200, height = 700, dpi = 140, units = "px")
write.xlsx(by_youth_uji_improv_hunger, here::here("images", "uji_improv_hunger_youth.xlsx"))

#Q29 – able to pay for education
#by gender
by_gender_pay_education_without_loan <- hhs_survey %>% 
  group_by(gender, pay_education_without_loan) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_gender_pay_education_without_loan <- by_gender_pay_education_without_loan %>%
  select(gender, pay_education_without_loan, percentage, total) 

ggplot(by_gender_pay_education_without_loan, aes(x = gender, y = percentage, group = fct_relevel(pay_education_without_loan, c("NA – no children at school", "No, none of the children", "For some children", "Yes, for all children")), fill = fct_relevel(pay_education_without_loan, c("NA – no children at school", "No, none of the children", "For some children", "Yes, for all children")))) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Are you able to pay for the education of all your children throughout the school year 
without taking a loan?", x = "Gender", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "pay_education_without_loan_gender.png"), width = 1200, height = 700, dpi = 140, units = "px")
write.xlsx(by_gender_pay_education_without_loan, here::here("images", "pay_education_without_loan_gender.xlsx"))

#by youth
by_youth_pay_education_without_loan <- hhs_survey %>% 
  group_by(youth, pay_education_without_loan) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_youth_pay_education_without_loan <- by_youth_pay_education_without_loan %>%
  select(youth, pay_education_without_loan, percentage, total) 

ggplot(by_youth_pay_education_without_loan, aes(x = youth, y = percentage, group = fct_relevel(pay_education_without_loan, c("NA – no children at school", "No, none of the children", "For some children", "Yes, for all children")), fill = fct_relevel(pay_education_without_loan, c("NA – no children at school", "No, none of the children", "For some children", "Yes, for all children")))) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Are you able to pay for the education of all your children throughout the school year 
without taking a loan?", x = "Age: Youth (Aged 18-35) or Older", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "pay_education_without_loan_youth.png"), width = 1200, height = 700, dpi = 140, units = "px")
write.xlsx(by_youth_pay_education_without_loan, here::here("images", "pay_education_without_loan_youth.xlsx"))

#Q30. How has your ability to pay for education changed since you started this job?
#by gender
by_gender_pay_education_change <- hhs_survey %>% 
  group_by(gender, pay_education_change) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_gender_pay_education_change <- by_gender_pay_education_change %>%
  select(gender, pay_education_change, percentage, total) 

ggplot(by_gender_pay_education_change, aes(x = gender, y = percentage, group = pay_education_change, fill = pay_education_change)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "How has your ability to pay for education changed?", x = "Gender", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "pay_education_change_gender.png"), width = 1200, height = 700, dpi = 140, units = "px")
write.xlsx(by_gender_pay_education_change, here::here("images", "pay_education_change_gender.xlsx"))

#by youth
by_youth_pay_education_change <- hhs_survey %>% 
  group_by(youth, pay_education_change) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_youth_pay_education_change <- by_youth_pay_education_change %>%
  select(youth, pay_education_change, percentage, total) 

ggplot(by_youth_pay_education_change, aes(x = youth, y = percentage, group = pay_education_change, fill = pay_education_change)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "How has your ability to pay for education changed?", x = "Age: Youth (Aged 18-35) or Older", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "pay_education_change_youth.png"), width = 1200, height = 700, dpi = 140, units = "px")
write.xlsx(by_youth_pay_education_change, here::here("images", "pay_education_change_youth.xlsx"))

#Q32 – change in ability to pay for healthcare
#by gender
by_gender_pay_health_change <- hhs_survey %>% 
  group_by(gender, pay_health_change) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_gender_pay_health_change <- by_gender_pay_health_change %>%
  select(gender, pay_health_change, percentage, total) 

ggplot(by_gender_pay_health_change, aes(x = gender, y = percentage, group = fct_relevel(pay_health_change, c("Somewhat got worse", "Remained stable", "Somewhat improved", "Greatly improved")), fill = fct_relevel(pay_health_change, c("Somewhat got worse", "Remained stable", "Somewhat improved", "Greatly improved")))) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "How has your ability to pay for healthcare changed since you started this job?", x = "Gender", y = "percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "pay_health_change_gender.png"), width = 1200, height = 700, dpi = 140, units = "px")
write.xlsx(by_gender_pay_health_change, here::here("images", "pay_health_change_gender.xlsx"))

#by youth
by_youth_pay_health_change <- hhs_survey %>% 
  group_by(youth, pay_health_change) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_youth_pay_health_change <- by_youth_pay_health_change %>%
  select(youth, pay_health_change, percentage, total)

ggplot(by_youth_pay_health_change, aes(x = youth, y = percentage, group = fct_relevel(pay_health_change, c("Somewhat got worse", "Remained stable", "Somewhat improved", "Greatly improved")), fill = fct_relevel(pay_health_change, c("Somewhat got worse", "Remained stable", "Somewhat improved", "Greatly improved")))) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "How has your ability to pay for healthcare changed since you started this job?", x = "Age: Youth (Aged 18-35) or Older", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "pay_health_change_youth.png"), width = 1200, height = 700, dpi = 140, units = "px")
write.xlsx(by_youth_pay_health_change, here::here("images", "pay_health_change_youth.xlsx"))

#Q34 -- Rating of health 
#by gender
by_gender_health <- hhs_survey %>% 
  group_by(gender, health) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_gender_health <- by_gender_health %>%
  select(gender, health, percentage, total)

ggplot(by_gender_health, aes(x = gender, y = percentage, group = fct_relevel(health, c("Quite poor", "Neither poor health nor excellent health", "Good health", "Excellent health")), fill = fct_relevel(health, c("Quite poor", "Neither poor health nor excellent health", "Good health", "Excellent health")))) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "How would you rate your health?", x = "Gender", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "health_gender.png"), width = 1200, height = 700, dpi = 140, units = "px")
write.xlsx(by_gender_health, here::here("images", "health_gender.xlsx"))

#by youth
by_youth_health <- hhs_survey %>% 
  group_by(youth, health) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_youth_health <- by_youth_health %>%
  select(youth, health, percentage, total)

ggplot(by_youth_health, aes(x = youth, y = percentage, group = fct_relevel(health, c("Quite poor", "Neither poor health nor excellent health", "Good health", "Excellent health")), fill = fct_relevel(health, c("Quite poor", "Neither poor health nor excellent health", "Good health", "Excellent health")))) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "How would you rate your health?", x = "Age: Youth (Aged 18-35) or Older", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "health_youth.png"), width = 1200, height = 700, dpi = 140, units = "px")
write.xlsx(by_youth_health, here::here("images", "health_youth.xlsx"))

#Q38 – safe working conditions
#by gender
by_gender_work_conditions_safe <- hhs_survey %>% 
  group_by(gender, work_conditions_safe) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_gender_work_conditions_safe <- by_gender_work_conditions_safe %>%
  select(gender, work_conditions_safe, percentage, total)

ggplot(by_gender_work_conditions_safe, aes(x = gender, y = percentage, group = fct_relevel(work_conditions_safe, c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree", "I do not want to answer")), fill = fct_relevel(work_conditions_safe, c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree", "I do not want to answer")))) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "My physical working conditions are safe and there are 
good safety precautions in place at work", x = "Gender", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "work_conditions_safe_gender.png"), width = 1200, height = 700, dpi = 140, units = "px")
write.xlsx(by_gender_work_conditions_safe, here::here("images", "work_conditions_safe_gender.xlsx"))

#by youth
by_youth_work_conditions_safe <- hhs_survey %>% 
  group_by(youth, work_conditions_safe) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_youth_work_conditions_safe <- by_youth_work_conditions_safe %>%
  select(youth, work_conditions_safe, percentage, total)

ggplot(by_youth_work_conditions_safe, aes(x = youth, y = percentage, group = fct_relevel(work_conditions_safe, c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree", "I do not want to answer")), fill = fct_relevel(work_conditions_safe, c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree", "I do not want to answer")))) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "My physical working conditions are safe and there are 
good safety precautions in place at work", x = "Age: Youth (Aged 18-35) or Older", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "work_conditions_safe_youth.png"), width = 1200, height = 700, dpi = 140, units = "px")
write.xlsx(by_youth_work_conditions_safe, here::here("images", "work_conditions_safe_youth.xlsx"))

#Q40. Are you exposed to heat and/or smoke in your job?  
#by gender
by_gender_exposed_heat_smoke <- hhs_survey %>% 
  group_by(gender, exposed_heat_smoke) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_gender_exposed_heat_smoke <- by_gender_exposed_heat_smoke %>%
  select(gender, exposed_heat_smoke, percentage, total)

ggplot(by_gender_exposed_heat_smoke, aes(x = gender, y = percentage, group = exposed_heat_smoke, fill = exposed_heat_smoke)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Are you exposed to heat and/or smoke in your job?", x = "Gender", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "exposed_heat_smoke_gender.png"), width = 1200, height = 600, dpi = 140, units = "px")
write.xlsx(by_gender_exposed_heat_smoke, here::here("images", "exposed_heat_smoke_gender.xlsx"))

#by youth
by_youth_exposed_heat_smoke <- hhs_survey %>% 
  group_by(youth, exposed_heat_smoke) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_youth_exposed_heat_smoke <- by_youth_exposed_heat_smoke %>%
  select(youth, exposed_heat_smoke, percentage, total)

ggplot(by_youth_exposed_heat_smoke, aes(x = youth, y = percentage, group = exposed_heat_smoke, fill = exposed_heat_smoke)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Are you exposed to heat and/or smoke in your job?", x = "Age: Youth (Aged 18-35) or Older", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "exposed_heat_smoke_youth.png"), width = 1200, height = 600, dpi = 140, units = "px")
write.xlsx(by_youth_exposed_heat_smoke, here::here("images", "exposed_heat_smoke_youth.xlsx"))

#Q41. If yes, are you given adequate personal protective equipment (PPE) to protect you from heat and/or smoke? 
#by gender
by_gender_adequate_ppe <- hhs_survey %>% 
  group_by(gender, adequate_ppe) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_gender_adequate_ppe <- by_gender_adequate_ppe %>%
  select(gender, adequate_ppe, percentage, total)

ggplot(by_gender_adequate_ppe, aes(x = gender, y = percentage, group = adequate_ppe, fill = adequate_ppe)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Are you given adequate personal protective equipment (PPE) to protect you from heat and/or smoke?", x = "Gender", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "adequate_ppe_gender.png"), width = 1200, height = 600, dpi = 140, units = "px")
write.xlsx(by_gender_adequate_ppe, here::here("images", "adequate_ppe_gender.xlsx"))

#by youth
by_youth_adequate_ppe <- hhs_survey %>% 
  group_by(youth, adequate_ppe) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_youth_adequate_ppe <- by_youth_adequate_ppe %>%
  select(youth, adequate_ppe, percentage, total)

ggplot(by_youth_adequate_ppe, aes(x = youth, y = percentage, group = adequate_ppe, fill = adequate_ppe)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Are you given adequate personal protective equipment (PPE) to protect you from heat and/or smoke?", x = "Youth", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "adequate_ppe_youth.png"), width = 1200, height = 600, dpi = 140, units = "px")
write.xlsx(by_youth_adequate_ppe, here::here("images", "adequate_ppe_youth.xlsx"))

#Q42. My employer treats me with decency and respect
#by gender
by_gender_decent_employer_treatment <- hhs_survey %>% 
  group_by(gender, decent_employer_treatment) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_gender_decent_employer_treatment <- by_gender_decent_employer_treatment %>%
  select(gender, decent_employer_treatment, percentage, total)

ggplot(by_gender_decent_employer_treatment, aes(x = gender, y = percentage, group = decent_employer_treatment, fill = decent_employer_treatment)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "My employer treats me with decency and respect", x = "Gender", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "decent_employer_treatment_gender.png"), width = 1200, height = 600, dpi = 140, units = "px")
write.xlsx(by_gender_decent_employer_treatment, here::here("images", "decent_employer_treatment_gender.xlsx"))

#by youth
by_youth_decent_employer_treatment <- hhs_survey %>% 
  group_by(youth, decent_employer_treatment) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_youth_decent_employer_treatment <- by_youth_decent_employer_treatment %>%
  select(youth, decent_employer_treatment, percentage, total)

ggplot(by_youth_decent_employer_treatment, aes(x = youth, y = percentage, group = decent_employer_treatment, fill = decent_employer_treatment)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "My employer treats me with decency and respect", x = "Youth", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "decent_employer_treatment_youth.png"), width = 1200, height = 600, dpi = 140, units = "px")
write.xlsx(by_youth_decent_employer_treatment, here::here("images", "decent_employer_treatment_youth.xlsx"))

#Q43. I am given sufficient breaks in the day 
#by gender
by_gender_suff_breaks_gven <- hhs_survey %>% 
  group_by(gender, suff_breaks_gven) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_gender_suff_breaks_gven <- by_gender_suff_breaks_gven %>%
  select(gender, suff_breaks_gven, percentage, total)

ggplot(by_gender_suff_breaks_gven, aes(x = gender, y = percentage, group = suff_breaks_gven, fill = suff_breaks_gven)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "I am given sufficient breaks in the day", x = "Gender", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "suff_breaks_gven_gender.png"), width = 1200, height = 600, dpi = 140, units = "px")
write.xlsx(by_gender_suff_breaks_gven, here::here("images", "suff_breaks_gven_gender.xlsx"))

#by youth
by_youth_suff_breaks_gven <- hhs_survey %>% 
  group_by(youth, suff_breaks_gven) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_youth_suff_breaks_gven <- by_youth_suff_breaks_gven %>%
  select(youth, suff_breaks_gven, percentage, total)

ggplot(by_youth_suff_breaks_gven, aes(x = youth, y = percentage, group = suff_breaks_gven, fill = suff_breaks_gven)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "I am given sufficient breaks in the day", x = "Age: Youth (Aged 18-35) or Older", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "suff_breaks_gven_youth.png"), width = 1200, height = 600, dpi = 140, units = "px")
write.xlsx(by_youth_suff_breaks_gven, here::here("images", "suff_breaks_gven_youth.xlsx"))

#Q44. I am given adequate opportunities for training to be able to do my job properly 
#by gender
by_gender_suff_training_opp <- hhs_survey %>% 
  group_by(gender, suff_training_opp) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_gender_suff_training_opp <- by_gender_suff_training_opp %>%
  select(gender, suff_training_opp, percentage, total)

ggplot(by_gender_suff_training_opp, aes(x = gender, y = percentage, group = suff_training_opp, fill = suff_training_opp)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "I am given adequate opportunities for training", x = "Gender", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "suff_training_opp_gender.png"), width = 1200, height = 600, dpi = 140, units = "px")
write.xlsx(by_gender_suff_training_opp, here::here("images", "suff_training_opp_gender.xlsx"))

#by youth
by_youth_suff_training_opp <- hhs_survey %>% 
  group_by(youth, suff_training_opp) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_youth_suff_training_opp <- by_youth_suff_training_opp %>%
  select(youth, suff_training_opp, percentage, total)

ggplot(by_youth_suff_training_opp, aes(x = youth, y = percentage, group = suff_training_opp, fill = suff_training_opp)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "I am given adequate opportunities for training", x = "Age: Youth (Aged 18-35) or Older", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "suff_training_opp_youth.png"), width = 1200, height = 600, dpi = 140, units = "px")
write.xlsx(by_youth_suff_training_opp, here::here("images", "suff_training_opp_youth.xlsx"))

#Q45. I am not subject to mental abuse by my employer or those I work with 
#by gender
by_gender_not_mentally_abused <- hhs_survey %>% 
  group_by(gender, not_mentally_abused) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_gender_not_mentally_abused <- by_gender_not_mentally_abused %>%
  select(gender, not_mentally_abused, percentage, total)

ggplot(by_gender_not_mentally_abused, aes(x = gender, y = percentage, group = not_mentally_abused, fill = not_mentally_abused)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "I am not subject to mental abuse by my employer or those I work with", x = "Gender", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "not_mentally_abused_gender.png"), width = 1200, height = 600, dpi = 140, units = "px")
write.xlsx(by_gender_not_mentally_abused, here::here("images", "not_mentally_abused_gender.xlsx"))

#by youth
by_youth_not_mentally_abused <- hhs_survey %>% 
  group_by(youth, not_mentally_abused) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_youth_not_mentally_abused <- by_youth_not_mentally_abused %>%
  select(youth, not_mentally_abused, percentage, total)

ggplot(by_youth_not_mentally_abused, aes(x = youth, y = percentage, group = not_mentally_abused, fill = not_mentally_abused)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "I am not subject to mental abuse by my employer or those I work with", x = "Age: Youth (Aged 18-35) or Older", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "not_mentally_abused_youth.png"), width = 1200, height = 600, dpi = 140, units = "px")
write.xlsx(by_youth_not_mentally_abused, here::here("images", "not_mentally_abused_youth.xlsx"))

#Q46. I am not subject to physical abuse by my employer or those I work with
#by gender
by_gender_not_physically_abused <- hhs_survey %>% 
  group_by(gender, not_physically_abused) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_gender_not_physically_abused <- by_gender_not_physically_abused %>%
  select(gender, not_physically_abused, percentage, total)

ggplot(by_gender_not_physically_abused, aes(x = gender, y = percentage, group = not_physically_abused, fill = not_physically_abused)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "I am not subject to physical abuse by my employer or those I work with", x = "Gender", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "not_physically_abused_gender.png"), width = 1200, height = 600, dpi = 140, units = "px")
write.xlsx(by_gender_not_physically_abused, here::here("images", "not_physically_abused_gender.xlsx"))

#by youth
by_youth_not_physically_abused <- hhs_survey %>% 
  group_by(youth, not_physically_abused) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_youth_not_physically_abused <- by_youth_not_physically_abused %>%
  select(youth, not_physically_abused, percentage, total)

ggplot(by_youth_not_physically_abused, aes(x = youth, y = percentage, group = not_physically_abused, fill = not_physically_abused)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "I am not subject to physical abuse by my employer or those I work with", x = "Age: Youth (Aged 18-35) or Older", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "not_physically_abused_youth.png"), width = 1200, height = 600, dpi = 140, units = "px")
write.xlsx(by_youth_not_physically_abused, here::here("images", "not_physically_abused_youth.xlsx"))

#Q47. My mental or physical health is not negatively affected by my job
#by gender
by_gender_job_not_affect_mental_physical <- hhs_survey %>% 
  group_by(gender, job_not_affect_mental_physical_neg) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_gender_job_not_affect_mental_physical <- by_gender_job_not_affect_mental_physical %>%
  select(gender, job_not_affect_mental_physical_neg, percentage, total)

ggplot(by_gender_job_not_affect_mental_physical, aes(x = gender, y = percentage, group = job_not_affect_mental_physical_neg, fill = job_not_affect_mental_physical_neg)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "My mental or physical health is not negatively affected by my job", x = "Gender", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "job_not_affect_mental_physical_gender.png"), width = 1200, height = 600, dpi = 140, units = "px")
write.xlsx(by_gender_job_not_affect_mental_physical, here::here("images", "job_not_affect_mental_physical_gender.xlsx"))

#by youth
by_youth_job_not_affect_mental_physical <- hhs_survey %>% 
  group_by(youth, job_not_affect_mental_physical_neg) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_youth_job_not_affect_mental_physical <- by_youth_job_not_affect_mental_physical %>%
  select(youth, job_not_affect_mental_physical_neg, percentage, total)

ggplot(by_youth_job_not_affect_mental_physical, aes(x = youth, y = percentage, group = job_not_affect_mental_physical_neg, fill = job_not_affect_mental_physical_neg)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "My mental or physical health is not negatively affected by my job", x = "Age: Youth (Aged 18-35) or Older", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "job_not_affect_mental_physical_youth.png"), width = 1200, height = 600, dpi = 140, units = "px")
write.xlsx(by_youth_job_not_affect_mental_physical, here::here("images", "job_not_affect_mental_physical_youth.xlsx"))

#Q48 – discrimination 
#by gender
by_gender_not_discrmn_socially <- hhs_survey %>% 
  group_by(gender, not_discrmn_socially) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_gender_not_discrmn_socially <- by_gender_not_discrmn_socially %>%
  select(gender, not_discrmn_socially, percentage, total)

ggplot(by_gender_not_discrmn_socially, aes(x = gender, y = percentage, group = fct_relevel(not_discrmn_socially, c("Strongly disagree", "Somewhat disagree", "Somewhat agree", "Strongly agree")), fill = fct_relevel(not_discrmn_socially, c("Strongly disagree", "Somewhat disagree", "Somewhat agree", "Strongly agree")))) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "I am never discriminated against in this job on the basis of age, gender, 
religion, race or ethnicity, economic background, or disability", x = "Gender", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "not_discrmn_socially_gender.png"), width = 1200, height = 600, dpi = 140, units = "px")
write.xlsx(by_gender_not_discrmn_socially, here::here("images", "not_discrmn_socially_gender.xlsx"))

#by youth
by_youth_not_discrmn_socially <- hhs_survey %>% 
  group_by(youth, not_discrmn_socially) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_youth_not_discrmn_socially <- by_youth_not_discrmn_socially %>%
  select(youth, not_discrmn_socially, percentage, total)

ggplot(by_youth_not_discrmn_socially, aes(x = youth, y = percentage, group = fct_relevel(not_discrmn_socially, c("Strongly disagree", "Somewhat disagree", "Somewhat agree", "Strongly agree")), fill = fct_relevel(not_discrmn_socially, c("Strongly disagree", "Somewhat disagree", "Somewhat agree", "Strongly agree")))) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "I am never discriminated against in this job on the basis of age, gender, 
religion, race or ethnicity, economic background, or disability", x = "Age: Youth (Aged 18-35) or Older", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "not_discrmn_socially_youth.png"), width = 1200, height = 600, dpi = 140, units = "px")
write.xlsx(by_youth_not_discrmn_socially, here::here("images", "not_discrmn_socially_youth.xlsx"))

#Q49. Have you ever sustained an injury as a result of your work here on the farm?  
#by gender
by_gender_injury <- hhs_survey %>% 
  group_by(gender, injury) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_gender_injury <- by_gender_injury %>%
  select(gender, injury, percentage, total)

ggplot(by_gender_injury, aes(x = gender, y = percentage, group = injury, fill = injury)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Have you ever sustained an injury as a result of your work here on the farm?", x = "Gender", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "injury_gender.png"), width = 1200, height = 600, dpi = 140, units = "px")
write.xlsx(by_gender_injury, here::here("images", "injury_gender.xlsx"))

#by youth
by_youth_injury <- hhs_survey %>% 
  group_by(youth, injury) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_youth_injury <- by_youth_injury %>%
  select(youth, injury, percentage, total)

ggplot(by_youth_injury, aes(x = youth, y = percentage, group = injury, fill = injury)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Have you ever sustained an injury as a result of your work here on the farm?", x = "Age: Youth (Aged 18-35) or Older", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "injury_youth.png"), width = 1200, height = 600, dpi = 140, units = "px")
write.xlsx(by_youth_injury, here::here("images", "injury_youth.xlsx"))

#Q51—raise concerns
#by gender
by_gender_can_raise_concerns <- hhs_survey %>% 
  group_by(gender, can_raise_concerns) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_gender_can_raise_concerns <- by_gender_can_raise_concerns %>%
  select(gender, can_raise_concerns, percentage, total)

ggplot(by_gender_can_raise_concerns, aes(x = gender, y = percentage, group = fct_relevel(can_raise_concerns, c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree")), fill = fct_relevel(can_raise_concerns, c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree")))) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "I can raise any concerns I have about the safety of my job or my wellbeing 
with management without fear of negative repercussions", x = "Gender", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = c(0.85, 0.85))

ggsave(filename = here::here("images", "can_raise_concerns_gender.png"), width = 1200, height = 600, dpi = 140, units = "px")
write.xlsx(by_gender_can_raise_concerns, here::here("images", "can_raise_concerns_gender.xlsx"))

#by youth
by_youth_can_raise_concerns <- hhs_survey %>% 
  group_by(youth, can_raise_concerns) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_youth_can_raise_concerns <- by_youth_can_raise_concerns %>%
  select(youth, can_raise_concerns, percentage, total)

ggplot(by_youth_can_raise_concerns, aes(x = youth, y = percentage, group = fct_relevel(can_raise_concerns, c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree")), fill = fct_relevel(can_raise_concerns, c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree")))) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "I can raise any concerns I have about the safety of my job or my wellbeing 
with management without fear of negative repercussions", x = "Age: Youth (Aged 18-35) or Older", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = c(0.875, 0.85))

ggsave(filename = here::here("images", "can_raise_concerns_youth.png"), width = 1200, height = 600, dpi = 140, units = "px")
write.xlsx(by_youth_can_raise_concerns, here::here("images", "can_raise_concerns_youth.xlsx"))

#Q50. If yes, how satisfied were you with how the farm leadership handled the incident? 
#by gender
by_gender_satisfied_injury_handled <- hhs_survey %>% 
  group_by(gender, satisfied_injury_handled) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_gender_satisfied_injury_handled <- by_gender_satisfied_injury_handled %>%
  select(gender, satisfied_injury_handled, percentage, total)

ggplot(by_gender_satisfied_injury_handled, aes(x = gender, y = percentage, group = satisfied_injury_handled, fill = satisfied_injury_handled)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "If yes, how satisfied were you with how the farm leadership handled the incident?", x = "Gender", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "satisfied_injury_handled_gender.png"), width = 1200, height = 600, dpi = 140, units = "px")
write.xlsx(by_gender_satisfied_injury_handled, here::here("images", "satisfied_injury_handled_gender.xlsx"))

#by youth
by_youth_satisfied_injury_handled <- hhs_survey %>% 
  group_by(youth, satisfied_injury_handled) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_youth_satisfied_injury_handled <- by_youth_satisfied_injury_handled %>%
  select(youth, satisfied_injury_handled, percentage, total)

ggplot(by_youth_satisfied_injury_handled, aes(x = youth, y = percentage, group = satisfied_injury_handled, fill = satisfied_injury_handled)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "If yes, how satisfied were you with how the farm leadership handled the incident?", x = "Age: Youth (Aged 18-35) or Older", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "satisfied_injury_handled_youth.png"), width = 1200, height = 600, dpi = 140, units = "px")
write.xlsx(by_youth_satisfied_injury_handled, here::here("images", "satisfied_injury_handled_youth.xlsx"))

#Q51. How satisfied are you that appropriate safety measures have been put in place to avoid the industry happening again?
#by gender
by_gender_satisfied_injury_measures <- hhs_survey %>% 
  group_by(gender, satisfied_injury_measures) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_gender_satisfied_injury_measures <- by_gender_satisfied_injury_measures %>%
  select(gender, satisfied_injury_measures, percentage, total)

ggplot(by_gender_satisfied_injury_measures, aes(x = gender, y = percentage, group = satisfied_injury_measures, fill = satisfied_injury_measures)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "How satisfied are you that appropriate safety measures have been put in place to avoid the industry happening again?", x = "Gender", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "satisfied_injury_measures_gender.png"), width = 1200, height = 600, dpi = 140, units = "px")
write.xlsx(by_gender_satisfied_injury_measures, here::here("images", "satisfied_injury_measures_gender.xlsx"))

#by youth
by_youth_satisfied_injury_measures <- hhs_survey %>% 
  group_by(youth, satisfied_injury_measures) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_youth_satisfied_injury_measures <- by_youth_satisfied_injury_measures %>%
  select(youth, satisfied_injury_measures, percentage, total)

ggplot(by_youth_satisfied_injury_measures, aes(x = youth, y = percentage, group = satisfied_injury_measures, fill = satisfied_injury_measures)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "How satisfied are you that appropriate safety measures have been put in place to avoid the industry happening again?", x = "Age: Youth (Aged 18-35) or Older", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "satisfied_injury_measures_youth.png"), width = 1200, height = 600, dpi = 140, units = "px")
write.xlsx(by_youth_satisfied_injury_measures, here::here("images", "satisfied_injury_measures_youth.xlsx"))

#Q52 safe on travel to work
#by gender
by_gender_safe_travel_work <- hhs_survey %>% 
  group_by(gender, safe_travel_work) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_gender_safe_travel_work <- by_gender_safe_travel_work %>%
  select(gender, safe_travel_work, percentage, total) 

ggplot(by_gender_safe_travel_work, aes(x = gender, y = percentage, group = fct_relevel(safe_travel_work, c("Strongly disagree", "Somewhat disagree", "Somewhat agree", "Strongly agree")), fill = fct_relevel(safe_travel_work, c("Strongly disagree", "Somewhat disagree", "Somewhat agree", "Strongly agree")))) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "I feel safe on my travel to and from work", x = "Gender", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "safe_travel_work_gender.png"), width = 1200, height = 600, dpi = 140, units = "px")
write.xlsx(by_gender_safe_travel_work, here::here("images", "safe_travel_work_gender.xlsx"))

# by youth
by_youth_safe_travel_work <- hhs_survey %>% 
  group_by(youth, safe_travel_work) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_youth_safe_travel_work <- by_youth_safe_travel_work %>%
  select(youth, safe_travel_work, percentage, total) 

ggplot(by_youth_safe_travel_work, aes(x = youth, y = percentage, group = fct_relevel(safe_travel_work, c("Strongly disagree", "Somewhat disagree", "Somewhat agree", "Strongly agree")), fill = fct_relevel(safe_travel_work, c("Strongly disagree", "Somewhat disagree", "Somewhat agree", "Strongly agree")))) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "I feel safe on my travel to and from work", x = "Age: Youth (Aged 18-35) or Older", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "safe_travel_work_youth.png"), width = 1200, height = 600, dpi = 140, units = "px")
write.xlsx(by_youth_safe_travel_work, here::here("images", "safe_travel_work_youth.xlsx"))

#Q53 – job satisfaction
#by gender
by_gender_satisfied_job <- hhs_survey %>% 
  group_by(gender, satisfied_job) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_gender_satisfied_job <- by_gender_satisfied_job %>%
  select(gender, satisfied_job, percentage, total) 

ggplot(by_gender_satisfied_job, aes(x = gender, y = percentage, group = fct_relevel(satisfied_job, c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree")), fill = fct_relevel(satisfied_job, c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree")))) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "I am satisfied with my job", x = "Gender", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "satisfied_job_gender.png"), width = 1200, height = 600, dpi = 140, units = "px")
write.xlsx(by_gender_satisfied_job, here::here("images", "satisfied_job_gender.xlsx"))

#by youth
by_youth_satisfied_job <- hhs_survey %>% 
  group_by(youth, satisfied_job) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_youth_satisfied_job <- by_youth_satisfied_job %>%
  select(youth, satisfied_job, percentage, total) 

ggplot(by_youth_satisfied_job, aes(x = youth, y = percentage, group = fct_relevel(satisfied_job, c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree")), fill = fct_relevel(satisfied_job, c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree")))) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "I am satisfied with my job", x = "Age: Youth (Aged 18-35) or Older", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "satisfied_job_youth.png"), width = 1200, height = 600, dpi = 140, units = "px")
write.xlsx(by_youth_satisfied_job, here::here("images", "satisfied_job_youth.xlsx"))

#Q54 – longer-term employment 
#by gender
by_gender_long_term_employmnt <- hhs_survey %>% 
  group_by(gender, long_term_employmnt) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_gender_long_term_employmnt <- by_gender_long_term_employmnt %>%
  select(gender, long_term_employmnt, percentage, total) 

ggplot(by_gender_long_term_employmnt, aes(x = gender, y = percentage, group = long_term_employmnt, fill = long_term_employmnt)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "I would like to be employed on this farm on a long-term basis", x = "Gender", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "long_term_employment_gender.png"), width = 1200, height = 600, dpi = 140, units = "px")
write.xlsx(by_gender_long_term_employmnt, here::here("images", "long_term_employment_gender.xlsx"))

#by youth
by_youth_long_term_employmnt <- hhs_survey %>% 
  group_by(youth, long_term_employmnt) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>%
  mutate(
    percentage = round(proportion * 100, 1)
  )

# Remove unwanted columns
by_youth_long_term_employmnt <- by_youth_long_term_employmnt %>%
  select(youth, long_term_employmnt, percentage, total) 

ggplot(by_youth_long_term_employmnt, aes(x = youth, y = percentage, group = long_term_employmnt, fill = long_term_employmnt)) +
  geom_bar(stat = "identity", position = "stack", width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "I would like to be employed on this farm on a long-term basis", x = "Age: Youth (Aged 18-35) or Older", y = "Percentage of Respondents") +
  theme_sjplot() + 
  theme(legend.position = "right")

ggsave(filename = here::here("images", "long_term_employment_youth.png"), width = 1200, height = 600, dpi = 140, units = "px")
write.xlsx(by_youth_long_term_employmnt, here::here("images", "long_term_employment_youth.xlsx"))

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

#How has your household’s hunger changed since you started this job?
by_hh_gender <- hhs_survey %>%
  filter(!is.na(hh_gender)) %>%
  group_by(hh_gender, hh_hunger_changed) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) 

ggplot(by_hh_gender, aes(x = hh_gender, y = proportion, group = fct_relevel(hh_hunger_changed, c("Greatly decreased", "Somewhat decreased", "Remained stable", "Somewhat increased", "Greatly increased")), fill = fct_relevel(hh_hunger_changed, c("Greatly decreased", "Somewhat decreased", "Remained stable", "Somewhat increased", "Greatly increased")))) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "How has your household’s hunger changed since you started this job?", x = "Gender of Household Head", y = "Proportion of Respondents") +
  theme_sjplot() + 
  theme(legend.position = c(0.85, 0.85))

ggsave(filename = here::here("images/hh_head", "hh_hunger_changed_hh_gender.png"), width = 1200, height = 600, dpi = 140, units = "px")
write.xlsx(by_hh_gender, here::here("images/hh_head", "hh_hunger_changed_hh_gender.xlsx"))

#Are you able to pay for the education of all your children throughout the school year without taking a loan?
by_hh_gender <- hhs_survey %>%
  filter(!is.na(hh_gender)) %>%
  group_by(hh_gender, pay_education_without_loan) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) 

ggplot(by_hh_gender, aes(x = hh_gender, y = proportion, group = fct_relevel(pay_education_without_loan, c("NA – no children at school", "No, none of the children", "For some children", "Yes, for all children")), fill = fct_relevel(pay_education_without_loan, c("NA – no children at school", "No, none of the children", "For some children", "Yes, for all children")))) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Are you able to pay for the education of all your children 
throughout the school year without taking a loan?", x = "Gender of Household Head", y = "Proportion of Respondents") +
  theme_sjplot() + 
  theme(legend.position = c(0.85, 0.85))

ggsave(filename = here::here("images/hh_head", "pay_education_without_loan_hh_gender.png"), width = 1200, height = 600, dpi = 140, units = "px")
write.xlsx(by_hh_gender, here::here("images/hh_head", "pay_education_without_loan_hh_gender.xlsx"))

#How has your ability to pay for education changed since you started this job?
by_hh_gender <- hhs_survey %>%
  filter(!is.na(hh_gender)) %>%
  group_by(hh_gender, pay_education_change) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) 

ggplot(by_hh_gender, aes(x = hh_gender, y = proportion, group = fct_relevel(pay_education_change, c("Greatly worsened", "Somewhat got worse", "Remained stable", "Somewhat improved", "Greatly improved")), fill = fct_relevel(pay_education_change, c("Greatly worsened", "Somewhat got worse", "Remained stable", "Somewhat improved", "Greatly improved")))) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "How has your ability to pay for education changed since you started this job?", x = "Gender of Household Head", y = "Proportion of Respondents") +
  theme_sjplot() + 
  theme(legend.position = c(0.85, 0.85))

ggsave(filename = here::here("images/hh_head", "pay_education_without_loan_hh_gender.png"), width = 1200, height = 600, dpi = 140, units = "px")
write.xlsx(by_hh_gender, here::here("images/hh_head", "pay_education_without_loan_hh_gender.xlsx"))

#Are you throughout the year able to pay for the family’s healthcare costs without taking a loan?
by_hh_gender <- hhs_survey %>%
  filter(!is.na(hh_gender)) %>%
  group_by(hh_gender, pay_health_without_loan) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) 

ggplot(by_hh_gender, aes(x = hh_gender, y = proportion, group = fct_relevel(pay_health_without_loan, c("No", "Partly", "Yes")), fill = fct_relevel(pay_health_without_loan, c("No", "Partly", "Yes")))) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Are you throughout the year able to pay for the family’s
healthcare costs without taking a loan?", x = "Gender of Household Head", y = "Proportion of Respondents") +
  theme_sjplot() + 
  theme(legend.position = c(0.85, 0.85))

ggsave(filename = here::here("images/hh_head", "pay_health_without_loan_hh_gender.png"), width = 1200, height = 600, dpi = 140, units = "px")
write.xlsx(by_hh_gender, here::here("images/hh_head", "pay_health_without_loan_hh_gender.xlsx"))
