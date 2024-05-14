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

#test with api data
#data(api)
rm(list=ls())

#hhs_wealth <- readRDS("hhs_cleaned_wealth.rds")
hhs_cleaned <- readRDS("Soysambu SAPA/hhs_cleaned.rds")
head(hhs_cleaned)

#Sustain EA colour pallette
my_palette <- c("#202C39", "#d77e5e", "#3d5919", "#e6e7e2","#a4b792", "#381D2A", "#000000","#202C39", "#d77e5e")

# Set survey design
dclus2 <- hhs_cleaned %>%
  as_survey_design(c(dnum, snum), fpc = c(fpc1, fpc2))

# Explanation of the terms above
# dnum is the ID of the village
# snum is the ID of the survey 
# FPC1 is the finite population correction of the total number of potential villages to sample
# FPC2 is the finite population correction of the total number of potential households in the cluster

# attempt at looping through - sort of working --------------------------------- 

variables <- c("gender", "age", "hh_head_born")

for (variable in variables) {
  by_location <- dclus2 %>% 
    group_by_at(vars(locat, {{variable}})) %>%  
    summarise(
      proportion = survey_mean(vartype = "ci", na.rm = TRUE),
      total = survey_total(vartype = "ci", na.rm = TRUE),
      n = unweighted(n())
    )
  
  ggplot(by_location, aes(x = locat, y = proportion, group = {{variable}}, fill = {{variable}})) +
    geom_bar(aes_string(fill = {{variable}}), stat = "identity", position = position_dodge(preserve = "single"), width = 0.8) +
    geom_errorbar(
          data = by_location,
          aes(
            ymax = ifelse(proportion_upp > 1, 1, proportion_upp),
            ymin = ifelse(proportion_low < 0, 0, proportion_low)
          ),
          position = position_dodge(preserve = "single", width = 0.95),
          width = 0.1
        ) +
      labs(title = variable, x = "Location", y = "Proportion of Households") +
    scale_y_continuous(limits = c(0, 1)) +
    theme_sjplot() +
    theme(legend.position = c(0.95, 0.95))
  
  ggsave(filename = here::here("images", paste0(variable, ".png")))
}

# gender --------------------------------- 

by_location <- dclus2 %>% 
  group_by(locat, gender) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())
  ) 

ggplot(by_location, aes(x = locat, y = n, group = gender, fill = gender)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Gender of respondents", x = "Location", y = "Number of Households") +
  theme_sjplot() + 
  theme(legend.position = c(0.95, 0.95))

ggsave(filename = here::here("images", "gender_respondents.png"))

# born in area --------------------------------- 

by_location <- dclus2 %>% 
  group_by(locat, hh_head_born) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n())) 

ggplot(by_location, aes(x=locat, y=proportion, group = hh_head_born, fill = hh_head_born)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=by_location, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  scale_fill_manual(values = my_palette) +  
  guides(fill=guide_legend(title=NULL)) +
  labs(title="hh_head_born", x="Location", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position=c(0.80,0.950))

ggsave(filename = here::here("images", "hh_head_born.png"))

# ppl in hhs --------------------------------- 

by_location <- dclus2 %>% 
  mutate(hh_total_numb_cat = cut(hh_total_numb, c(1, 2, 5, 10, 100), include.lowest = TRUE,
                           labels = c("<2", "2-5", "5-10", ">10"))) %>%
  group_by(locat, hh_total_numb_cat) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())
  ) 

ggplot(by_location, aes(x = locat, y = n, group = hh_total_numb_cat, fill = hh_total_numb_cat)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "hh_total_numb_respondents", x = "Location", y = "Number of Households") +
  theme_sjplot() + 
  theme(legend.position = c(0.95, 0.95))

ggsave(filename = here::here("images", "hh_total_numb_respondents.png"))

# age --------------------------------- 

by_location <- dclus2 %>% 
  mutate(age_cat = cut(age, c(18, 25, 35, 55, 100), include.lowest = TRUE,
                       labels = c("18-25", "25-35", "35-55", "55 +"))) %>% 
  group_by(locat, age_cat) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n())) 

by_location$age_cat <- by_location$age_cat %>% coalesce(by_location$age_cat, "Didn't answer")
  

ggplot(by_location, aes(x=locat, y=n, group = age_cat, fill = age_cat)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  #geom_errorbar(data=by_location, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
  #              position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  scale_fill_manual(values = my_palette) +  
  guides(fill=guide_legend(title=NULL)) +
  labs(title="age categories of respondents", x="Location", y = "Number of Households") +
  #scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position=c(0.90,0.950))

ggsave(filename = here::here("images", "age_respondents.png"))

# edu --------------------------------- 
by_location <- dclus2 %>% 
  group_by(locat, edu) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())
  ) 

ggplot(by_location, aes(x = locat, y = n, group = edu, fill = edu)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Completed education of respondents", x = "Location", y = "Number of Households") +
  theme_sjplot() + 
  theme(legend.position = c(0.90, 0.90))

ggsave(filename = here::here("images", "edu_respondents.png"))



# children in hhs --------------------------------- 

by_location <- dclus2 %>% 
  mutate(numb_child_cat = cut(numb_child, c(1, 2, 4, 6, 8), include.lowest = TRUE,
                              labels = c("1-2", "3-4", "5-6", "7 or more"))) %>%
  group_by(locat, numb_child_cat) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())
  ) %>% 
  drop_na(numb_child_cat)

ggplot(by_location, aes(x = locat, y = proportion, group = numb_child_cat, fill = numb_child_cat)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=by_location, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "numb_child_cat_proportions", x = "Location", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position = c(0.85, 0.85))

ggsave(filename = here::here("images", "numb_child_proportions.png"))

# Principal livelihood --------------------------------- 

dclus2 <- dclus2 %>% 
  mutate(   
    livelihood_activity1 = case_when(
      livelihood_activity1 == "Cultivation" ~ "Cultivation",
      livelihood_activity1 == "Kibarua or short-term employment (includes working for someone as bodaboda driver or herder)" ~ "Kibarua",
      livelihood_activity1 == "Own business" ~ "Other",
      livelihood_activity1 == "Food aid" ~ "Other",
      livelihood_activity1 == "Tourism related employment" ~ "Other",
      livelihood_activity1 == "Permanent Employment" ~ "Other",
      livelihood_activity1 == "Livestock and related products" ~ "Livestock",
      livelihood_activity1 == "I do not want to answer" ~ NA))

by_location <- dclus2 %>% 
  group_by(locat, livelihood_activity1) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())
  ) %>% 
  drop_na(livelihood_activity1)

ggplot(by_location, aes(x = locat, y = proportion, group = livelihood_activity1, fill = livelihood_activity1)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=by_location, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "livelihood_activity1", x = "Location", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position = c(0.93, 0.85))

ggsave(filename = here::here("images", "principal_livelihood.png"))

# skip meals --------------------------------- 

dclus2 <- dclus2 %>% 
  mutate(   
    skip_meal = case_when(
      skip_meal == "Some days in every month" ~ "Every month",
      skip_meal == "Only a few days in the worst months" ~ "Few days in worst months",
      skip_meal == "Never" ~ "Never",
      skip_meal == "I do not want to answer" ~ NA))

by_location <- dclus2 %>% 
  group_by(locat, skip_meal) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())
  ) %>% 
  drop_na(skip_meal)

ggplot(by_location, aes(x = locat, y = proportion, group = skip_meal, fill = skip_meal)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=by_location, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "skip_meal", x = "Location", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position = c(0.83, 0.85))

ggsave(filename = here::here("images", "skip_meal.png"))

# housing material --------------------------------- 

by_location <- dclus2 %>% 
  group_by(locat, wall) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())
  ) #%>% 
  drop_na(wall)

ggplot(by_location, aes(x = locat, y = proportion, group = wall, fill = wall)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=by_location, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "wall", x = "Location", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position = c(0.5, 0.85))

ggsave(filename = here::here("images", "wall.png"))

# influence over decision making --------------------------------- 

by_location <- dclus2 %>% 
  group_by(locat, influence_level) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())
  ) %>% 
filter(!influence_level == "I do not want to answer") %>%
  mutate(influence_level = fct_relevel(influence_level, "High", "Medium", "Low", "None"))

ggplot(by_location, aes(x = locat, y = proportion, group = influence_level, fill = influence_level)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=by_location, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "influence_level", x = "Location", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position = c(0.5, 0.85))

ggsave(filename = here::here("images", "influence_level.png"))

# secure feelings --------------------------------- 

by_location <- dclus2 %>% 
  group_by(locat, secure_feelings) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())
  ) %>% 
  filter(!secure_feelings == "I do not want to answer") %>%
  mutate(secure_feelings = fct_relevel(secure_feelings, "Very secure", "Secure", "Insecure", "Very Insecure"))

ggplot(by_location, aes(x = locat, y = proportion, group = secure_feelings, fill = secure_feelings)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=by_location, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "secure_feelings", x = "Location", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position = c(0.5, 0.85))

ggsave(filename = here::here("images", "secure_feelings.png"))

# wellbeing score --------------------------------- 

by_location <- dclus2 %>% 
  mutate(note_life_cat = cut(note_life, c(1, 3, 5, 8, 10), include.lowest = TRUE, labels = c("Very Bad", "Bad", "Good", "Very Good")
  )) %>% 
  group_by(locat, note_life_cat) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())
  ) %>% 
  drop_na(note_life_cat)

ggplot(by_location, aes(x = locat, y = proportion, group = note_life_cat, fill = note_life_cat)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=by_location, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "note_life_cat", x = "Location", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position = c(0.5, 0.85))

ggsave(filename = here::here("images", "wellbeing_cat.png"))

# wellbeing change over last 5 years --------------------------------- 

by_location <- dclus2 %>% 
  group_by(locat, change_wellbeing) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())
  ) %>% 
  filter(!change_wellbeing == "I do not want to answer") 

ggplot(by_location, aes(x = locat, y = proportion, group = change_wellbeing, fill = change_wellbeing)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=by_location, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "change_wellbeing over 5 years", x = "Location", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position = c(0.75, 0.85))

ggsave(filename = here::here("images", "change_wellbeing_5 years.png"))

# negative impact --------------------------------- 

# List of negative variables
variables <- c(
  "transmn_diseases",
  "conflict_wild",
  "compensation_wild",
  "access_restrictn",
  "priority_employ",
  "involved_projects",
  "soys_appreciate",
  "access_graze",
  "tree_cutting"
)

# Create empty frame to store the results 
all_by_location <- data.frame()

for (variable in variables) {
  by_location <- dclus2 %>% 
    group_by_at(vars({{variable}})) %>% 
    summarise(
      proportion = survey_mean(vartype = "ci", na.rm = TRUE),
      total = survey_total(vartype = "ci", na.rm = TRUE),
      n = unweighted(n())
    ) %>% 
    mutate(responses := fct_relevel({{variable}}, "High", "Medium", "Low", "Zero")) %>%
    # Pivot to long format
    pivot_longer(
      cols = starts_with("responses"),
      names_to = "variable",
      values_to = "value"
    )

  all_by_location <- bind_rows(all_by_location, by_location)
}

all_by_location <- all_by_location %>%
  mutate(variable = coalesce(transmn_diseases, conflict_wild, compensation_wild, access_restrictn, priority_employ,
                             involved_projects, soys_appreciate, access_graze, tree_cutting)) %>% 
  select(!c("transmn_diseases",
            "conflict_wild",
            "compensation_wild",
            "access_restrictn",
            "priority_employ",
            "involved_projects",
            "soys_appreciate",
            "access_graze",
            "tree_cutting")) %>% 
  filter(!variable == "I do not want to answer") %>% 
  mutate(variable = fct_relevel(variable, "High", "Medium", "Low", "Zero")) %>% 
  mutate(value = case_when(
    value == "tree_cutting" ~ "No cutting of trees allowed in Soysambu",
    value == "access_graze" ~ "Little access to grazing allowed in Soysambu",
    value == "soys_appreciate" ~ "Lack of appreciation when Soysambu receive help from community",
    value == "involved_projects" ~ "Community are not involved in development projects",
    value == "priority_employ" ~ "Community not prioritised in employment in Soysambu",
    value == "access_restrictn" ~ "Restriction on acccess to public utilities (e.g.roads)",
    value == "compensation_wild" ~ "No KWS compensation for damages by wildlife",
    value == "conflict_wild" ~ "Conflict with wildlife from Soysambu",
    value == "transmn_diseases" ~ "Transmission of disease from wildlife and livestock in Soysambu"
    ))
  
# Create a bar plot
ggplot(all_by_location, aes(x = reorder(value, proportion), y = proportion, fill = variable)) +
  geom_col(position = "stack", stat = "identity") +
  coord_flip() +
  scale_fill_brewer(palette = "PRGn") +
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Negative impacts", x = "", y = "Proportion of Households who feel impact is:") +
  theme_sjplot() + 
  theme(legend.position = "bottom") +
  scale_x_discrete(
    breaks = all_by_location$value,
    labels = str_wrap(all_by_location$value, width = 30)  # Adjust width as needed
  )

ggsave(filename = here::here("images", "negative impacts overall.png"))

# positive impact --------------------------------- 

# List of positive variables

variables <- c(
  "provide_water",
  "infrastr",
  "health_projects",
  "donations",
  "vaccin",
  "educate_comm",
  "sponsor",
  "educatn_trips",
  "envt_conservatn",
  "offer_firewd"
)

# Create empty frame to store the results 
all_by_location <- data.frame()

for (variable in variables) {
  by_location <- dclus2 %>% 
    group_by_at(vars({{variable}})) %>% 
    summarise(
      proportion = survey_mean(vartype = "ci", na.rm = TRUE),
      total = survey_total(vartype = "ci", na.rm = TRUE),
      n = unweighted(n())
    ) %>% 
    mutate(responses := fct_relevel({{variable}}, "High", "Medium", "Low", "Zero")) %>%
    # Pivot to long format
    pivot_longer(
      cols = starts_with("responses"),
      names_to = "variable",
      values_to = "value"
    )
  
  all_by_location <- bind_rows(all_by_location, by_location)
}

all_by_location <- all_by_location %>%
  mutate(variable = coalesce(provide_water,
                             infrastr,
                             health_projects,
                             donations,
                             vaccin,
                             educate_comm,
                             sponsor,
                             educatn_trips,
                             envt_conservatn,
                             offer_firewd)) %>% 
  select(!c("provide_water",
              "infrastr",
              "health_projects",
              "donations",
              "vaccin",
              "educate_comm",
              "sponsor",
              "educatn_trips",
              "envt_conservatn",
              "offer_firewd")) %>% 
  filter(!variable == "I do not want to answer") %>% 
  mutate(variable = fct_relevel(variable, "High", "Medium", "Low", "Zero")) %>% 
  mutate(value = case_when(
    value == "provide_water" ~ "Provide water to community",
    value == "infrastr" ~ "Build or maintain infrastructure (e.g. police post, school, roads)",
    value == "health_projects" ~ "Support health projects",
    value == "donations" ~ "Donate to schools (e.g. meals, desks, balls)",
    value == "vaccin" ~ "Provide anti-rabies vaccinations",
    value == "educate_comm" ~ "Education (e.g. livestock production, health talks, waste management)",
    value == "sponsor" ~ "Sponsorship opportunities for students",
    value == "educatn_trips" ~ "Guided educational trips to Soysambu",
    value == "envt_conservatn" ~ "Assist in environmental conservation e.g tree planting",
    value == "offer_firewd" ~ "Offer firewood to bush-clearing workers"
  ))

# Create a bar plot
ggplot(all_by_location, aes(x = reorder(value, proportion), y = proportion, fill = variable)) +
  geom_col(position = "stack", stat = "identity") +
  coord_flip() +
  scale_fill_brewer(palette = "PRGn") +
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Positive impacts", x = "", y = "Proportion of Households who feel impact is:") +
  theme_sjplot() + 
  theme(legend.position = "bottom") +
  scale_x_discrete(
    breaks = all_by_location$value,
    labels = str_wrap(all_by_location$value, width = 40)  # Adjust width as needed
  )

ggsave(filename = here::here("images", "positive impacts overall.png"))

# soysambu overall impact on wellbeing --------------------------------- 

by_location <- dclus2 %>% 
  group_by(locat, overall_impact) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())
  ) %>% 
  filter(!overall_impact == "I do not want to answer") %>% 
  mutate(overall_impact = fct_relevel(overall_impact, "Increased our wellbeing", "Neutral", "Reduced our wellbeing"))


ggplot(by_location, aes(x = locat, y = proportion, group = overall_impact, fill = overall_impact)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=by_location, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "overall_impact", x = "Location", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position = c(0.75, 0.85))

ggsave(filename = here::here("images", "summarise the overall impact of Soysambu on the well-being of your household.png"))

# soysambu contribution to change in wellbeing --------------------------------- 

by_location <- dclus2 %>% 
  group_by(locat, contribute_wellbeing) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())
  ) %>% 
  filter(!contribute_wellbeing == "I do not want to answer") %>% 
  mutate(contribute_wellbeing = fct_relevel(contribute_wellbeing, "Increased our wellbeing", "Neutral", "Reduced our wellbeing"))


ggplot(by_location, aes(x = locat, y = proportion, group = contribute_wellbeing, fill = contribute_wellbeing)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=by_location, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "contribute_wellbeing", x = "Location", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position = c(0.85, 0.85))

ggsave(filename = here::here("images", "Soysambu contribution to hhs well-being changed over past 5 years.png"))

# positive impact by location  --------------------------------- 

variables <- c(
  "provide_water",
  "infrastr",
  "health_projects",
  "donations",
  "vaccin",
  "educate_comm",
  "sponsor",
  "educatn_trips",
  "envt_conservatn",
  "offer_firewd"
)
# List of locations
locations <- c("Kiptangwanyi", "OlJorai", "Mbaruk", "Soysambu")

# Create an empty list to store results for each location
all_by_location_list <- list()

for (location in locations) {
  # Create empty frame to store the results for the current location
  all_by_location <- data.frame()
  
  for (variable in variables) {
    by_location <- dclus2 %>% 
      filter(locat == location) %>%  # Filter data for the specific location
      group_by_at(vars({{variable}})) %>% 
      summarise(
        proportion = survey_mean(vartype = "ci", na.rm = TRUE),
        total = survey_total(vartype = "ci", na.rm = TRUE),
        n = unweighted(n())
      ) %>% 
      mutate(responses := fct_relevel({{variable}}, "High", "Medium", "Low", "Zero")) %>%
      # Pivot to long format
      pivot_longer(
        cols = starts_with("responses"),
        names_to = "variable",
        values_to = "value"
      )
    
    all_by_location <- bind_rows(all_by_location, by_location)
  }
  
  all_by_location <- all_by_location %>%
    mutate(variable = coalesce(provide_water,
                               infrastr,
                               health_projects,
                               donations,
                               vaccin,
                               educate_comm,
                               sponsor,
                               educatn_trips,
                               envt_conservatn,
                               offer_firewd)) %>% 
    select(!c("provide_water",
              "infrastr",
              "health_projects",
              "donations",
              "vaccin",
              "educate_comm",
              "sponsor",
              "educatn_trips",
              "envt_conservatn",
              "offer_firewd")) %>% 
    filter(!variable == "I do not want to answer") %>% 
    mutate(variable = fct_relevel(variable, "High", "Medium", "Low", "Zero")) %>% 
    mutate(value = case_when(
      value == "provide_water" ~ "Provide water to community",
      value == "infrastr" ~ "Build or maintain infrastructure (e.g. police post, school, roads)",
      value == "health_projects" ~ "Support health projects",
      value == "donations" ~ "Donate to schools (e.g. meals, desks, balls)",
      value == "vaccin" ~ "Provide anti-rabies vaccinations",
      value == "educate_comm" ~ "Education (e.g. livestock production, health talks, waste management)",
      value == "sponsor" ~ "Sponsorship opportunities for students",
      value == "educatn_trips" ~ "Guided educational trips to Soysambu",
      value == "envt_conservatn" ~ "Assist in environmental conservation e.g tree planting",
      value == "offer_firewd" ~ "Offer firewood to bush-clearing workers"
    ))
  
  all_by_location_list[[location]] <- all_by_location
}

# Function to create ggplot for each location
create_ggplot <- function(df) {
  ggplot(df, aes(x = reorder(value, proportion), y = proportion, fill = variable)) +
    geom_col(position = "stack") +
    scale_fill_brewer(palette = "PRGn") +
    guides(fill = guide_legend(title = NULL)) +
    labs(x = "", y = "Proportion of Households who feel impact is:") +
    sea::theme_sea() + 
    theme(legend.position = "bottom") +
    scale_x_discrete(
      breaks = df$value,
      labels = str_wrap(df$value, width = 40)  # Adjust width as needed
    )
}
# Create the ggplots for each location with horizontal bars
p_kip <- create_ggplot(all_by_location_list$Kiptangwanyi) +
  coord_flip()
p_kip <- p_kip + labs(title="Kiptangwanyi")

p_ol <- create_ggplot(all_by_location_list$OlJorai) +
  coord_flip()
p_ol <- p_ol + labs(title="OlJorai")

p_mba <- create_ggplot(all_by_location_list$Mbaruk) +
  coord_flip()
p_mba <- p_mba + labs(title="Mbaruk")

p_soy <- create_ggplot(all_by_location_list$Soysambu) +
  coord_flip()
p_soy <- p_soy + labs(title="Soysambu")

pos <- ggarrange(p_kip + theme(legend.position="none", 
                 axis.title.y = element_blank(),
                 axis.title.x = element_blank()),
          p_ol + theme(legend.position="none", axis.text.y = element_blank(),
                       axis.ticks.y = element_blank(),
                       axis.title.y = element_blank(),
                       axis.title.x = element_blank()),
          p_mba + theme(legend.position="bottom", axis.text.y = element_blank(),
                        axis.ticks.y = element_blank()), 
          p_soy + theme(legend.position="none",axis.text.y = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.title.y = element_blank(),
                        axis.title.x = element_blank()),
          nrow = 1
          # labels = c("Kiptangwanyi", "OlJorai", "Mbaruk", "Soysambu"),
          # label.args = list(hjust = -0.8, vjust = 0.9)
          )
pos

ggsave(pos, filename = here::here("images", "positive impacts by location.png"))


# negative impacts by location --------------------------------- 

# List of negative variables
variables <- c(
  "transmn_diseases",
  "conflict_wild",
  "compensation_wild",
  "access_restrictn",
  "priority_employ",
  "involved_projects",
  "soys_appreciate",
  "access_graze",
  "tree_cutting"
)

# List of locations
locations <- c("Kiptangwanyi", "OlJorai", "Mbaruk", "Soysambu")

# Create an empty list to store results for each location
all_by_location_list <- list()

for (location in locations) {
  # Create empty frame to store the results for the current location
  all_by_location <- data.frame()
  
  for (variable in variables) {
    by_location <- dclus2 %>% 
      filter(locat == location) %>%  # Filter data for the specific location
      group_by_at(vars({{variable}})) %>% 
      summarise(
        proportion = survey_mean(vartype = "ci", na.rm = TRUE),
        total = survey_total(vartype = "ci", na.rm = TRUE),
        n = unweighted(n())
      ) %>% 
      mutate(responses := fct_relevel({{variable}}, "High", "Medium", "Low", "Zero")) %>%
      # Pivot to long format
      pivot_longer(
        cols = starts_with("responses"),
        names_to = "variable",
        values_to = "value"
      )
    
    all_by_location <- bind_rows(all_by_location, by_location)
  }
  
  all_by_location <- all_by_location %>%
    mutate(variable = coalesce(transmn_diseases, conflict_wild, compensation_wild, access_restrictn, priority_employ,
                               involved_projects, soys_appreciate, access_graze, tree_cutting)) %>%  
    select(!c("transmn_diseases",
              "conflict_wild",
              "compensation_wild",
              "access_restrictn",
              "priority_employ",
              "involved_projects",
              "soys_appreciate",
              "access_graze",
              "tree_cutting")) %>% 
    filter(!variable == "I do not want to answer") %>% 
    mutate(variable = fct_relevel(variable, "High", "Medium", "Low", "Zero")) %>% 
    mutate(value = case_when(
      value == "tree_cutting" ~ "No cutting of trees allowed in Soysambu",
      value == "access_graze" ~ "Little access to grazing allowed in Soysambu",
      value == "soys_appreciate" ~ "Lack of appreciation when Soysambu receive help from community",
      value == "involved_projects" ~ "Community are not involved in development projects",
      value == "priority_employ" ~ "Community not prioritised in employment in Soysambu",
      value == "access_restrictn" ~ "Restriction on acccess to public utilities (e.g.roads)",
      value == "compensation_wild" ~ "No KWS compensation for damages by wildlife",
      value == "conflict_wild" ~ "Conflict with wildlife from Soysambu",
      value == "transmn_diseases" ~ "Transmission of disease from wildlife and livestock in Soysambu"
    ))
  
  all_by_location_list[[location]] <- all_by_location
}

# Function to create ggplot for each location
create_ggplot <- function(df) {
  ggplot(df, aes(x = reorder(value, proportion), y = proportion, fill = variable)) +
    geom_col(position = "stack") +
    scale_fill_brewer(palette = "PRGn") +
    guides(fill = guide_legend(title = NULL)) +
    labs(x = "", y = "Proportion of Households who feel impact is:") +
    theme_sjplot() + 
    theme(legend.position = "bottom") +
    scale_x_discrete(
      breaks = df$value,
      labels = str_wrap(df$value, width = 40)  # Adjust width as needed
    )
}
# Create the ggplots for each location with horizontal bars
p_kip <- create_ggplot(all_by_location_list$Kiptangwanyi) +
  coord_flip()
p_kip <- p_kip + labs(title="Kiptangwanyi")

p_ol <- create_ggplot(all_by_location_list$OlJorai) +
  coord_flip()
p_ol <- p_ol + labs(title="OlJorai")

p_mba <- create_ggplot(all_by_location_list$Mbaruk) +
  coord_flip()
p_mba <- p_mba + labs(title="Mbaruk")

p_soy <- create_ggplot(all_by_location_list$Soysambu) +
  coord_flip()
p_soy <- p_soy + labs(title="Soysambu")

neg <- ggarrange(p_kip + theme(legend.position="none", 
                               axis.title.y = element_blank(),
                               axis.title.x = element_blank()),
                 p_ol + theme(legend.position="none", axis.text.y = element_blank(),
                              axis.ticks.y = element_blank(),
                              axis.title.y = element_blank(),
                              axis.title.x = element_blank()),
                 p_mba + theme(legend.position="bottom", axis.text.y = element_blank(),
                               axis.ticks.y = element_blank()), 
                 p_soy + theme(legend.position="none",axis.text.y = element_blank(),
                               axis.ticks.y = element_blank(),
                               axis.title.y = element_blank(),
                               axis.title.x = element_blank()),
                 nrow = 1
                 # labels = c("Kiptangwanyi", "OlJorai", "Mbaruk", "Soysambu"),
                 # label.args = list(hjust = -0.8, vjust = 0.9)
)
neg

ggsave(neg, filename = here::here("images", "negative impacts by location.png"))


# cultivated crop acreage  --------------------------------- 

by_location <- dclus2 %>% 
  mutate(crop_acre_cat = cut(crop_acre, c(0, 0.01, 1, 5, 10, Inf), include.lowest = TRUE,
                       labels = c("None", "Less than 1 acre", "1 - 5 acres", "5 - 10 acres", "more than 10 acres"))) %>% 
  group_by(locat, crop_acre_cat) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())
  )
ggplot(by_location, aes(x = locat, y = proportion, group = crop_acre_cat, fill = crop_acre_cat)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=by_location, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "cultivated crops in this location in the last year", x = "Location", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position = c(0.88, 0.85))

ggsave(filename = here::here("images", "acres cultivated in this location in the last year.png"))

# crop cultivated --------------------------------- 
by_location <- dclus2 %>% 
  group_by(locat, crop_cultivated) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())
  ) %>% 
  drop_na(crop_cultivated)

write.xlsx(by_location, here::here("images", "main crop cultivated.xlsx"))

# crop use --------------------------------- 

by_location <- dclus2 %>% 
  group_by(locat, crops_use) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())
  ) %>% 
  drop_na(crops_use)

write.xlsx(by_location, here::here("images", "main use of crop.xlsx"))

# crop damage --------------------------------- 

by_location <- dclus2 %>% 
  group_by(locat, crops_damg_yn) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())
  ) %>% 
  drop_na(crops_damg_yn)

ggplot(by_location, aes(x = locat, y = proportion, group = crops_damg_yn, fill = crops_damg_yn)) +
    geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
    geom_errorbar(data=by_location, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                  position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
    scale_fill_manual(values = my_palette) +  
    guides(fill = guide_legend(title = NULL)) +
    labs(title = "crops been damaged by wild animals in the last year", x = "Location", y = "Proportion of Households") +
    scale_y_continuous(limits=c(0, 1)) +
    theme_sjplot() + 
    theme(legend.position = c(0.88, 0.85))

ggsave(filename = here::here("images", "crops been damaged by wild animals in the last year.png"))

write.xlsx(by_location, here::here("images", "crops been damaged by wild animals in the last year.xlsx"))


# crop damage animal --------------------------- 

# Briefly looking at the problem animals - most are baboons and vervet monkeys with a few others (porcupine, buffalo)
# some pastoralists listed hyaenas

by_location <- dclus2 %>% 
  group_by(locat, damage_animal) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())
  ) %>% 
  drop_na(damage_animal)

by_location <- dclus2 %>% 
  group_by(locat, what_wild) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())
  ) 

# human injury or death --------------------------------- 

by_location <- dclus2 %>% 
  group_by(hwc_yn) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())
  )

write.xlsx(by_location, here::here("images", "ppl in hhs injured or killed by wildlife in the last year.xlsx"))


# livestock TLU --------------------------------- 

by_location <- dclus2 %>% 
  mutate(total_tlu_cat = cut(total_tlu, c(0, 0.01, 1, 5, 10, Inf), include.lowest = TRUE,
                                  labels = c("None", "Less than 1", "1 - 5", "5 - 10", "More than 10"))) %>% 
  group_by(locat, total_tlu_cat) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())
  ) %>% 
  filter(! total_tlu_cat == "NA") %>% 
  filter(! total_tlu_cat == "None")

ggplot(by_location, aes(x = locat, y = proportion, group = total_tlu_cat, fill = total_tlu_cat)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=by_location, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "total livestock tlu in hhs", x = "Location", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position = c(0.9, 0.85))

write.xlsx(by_location, here::here("images", "total livestock tlu in hhs.xlsx"))
ggsave(filename = here::here("images", "total livestock tlu in hhs.png"))

# livestock injury or death --------------------------------- 

by_location <- dclus2 %>% 
  group_by(locat, wild_conf_yn) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())
  ) %>% 
  drop_na(wild_conf_yn)

ggplot(by_location, aes(x = locat, y = proportion, group = wild_conf_yn, fill = wild_conf_yn)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=by_location, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "livestock been damaged by wild animals in the last year", x = "Location", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position = c(0.9, 0.85))

write.xlsx(by_location, here::here("images", "livestock been damaged by wild animals in the last year.xlsx"))
ggsave(filename = here::here("images", "livestock been damaged by wild animals in the last year.png"))

# agree/disagree statements rights --------------------------------- 

variables <- c(
  "men_wmen_rights",
  "harvest_firewd",
  "staff_violate"
)

# List of locations
locations <- c("OlJorai", "Kiptangwanyi", "Mbaruk", "Soysambu")

# Create an empty list to store results for each location
all_by_location_list <- list()

for (location in locations) {
  # Create empty frame to store the results for the current location
  all_by_location <- data.frame()
  
  for (variable in variables) {
    by_location <- dclus2 %>% 
      filter(locat == location) %>%  # Filter data for the specific location
      group_by_at(vars({{variable}})) %>% 
      summarise(
        proportion = survey_mean(vartype = "ci", na.rm = TRUE),
        total = survey_total(vartype = "ci", na.rm = TRUE),
        n = unweighted(n())) %>% 
      filter(!variable == "I do not want to answer") %>% 
      mutate(responses := fct_relevel({{variable}}, "Disagree", "Neutral", "Agree", "Dont Know")) %>%
      # Pivot to long format
      pivot_longer(
        cols = starts_with("responses"),
        names_to = "variable",
        values_to = "value"
      )
    
    all_by_location <- bind_rows(all_by_location, by_location)
  }
  
  all_by_location <- all_by_location %>%
    mutate(variable = coalesce(men_wmen_rights,
                               harvest_firewd,
                               staff_violate)) %>%  
    select(!c("men_wmen_rights",
              "harvest_firewd",
              "staff_violate")) %>%
    filter(!variable == "I do not want to answer") %>% 
    mutate(variable = fct_relevel(variable, "Disagree", "Neutral", "Agree", "Dont Know")) %>% 
    mutate(value = case_when(
      value == "men_wmen_rights" ~ "Soysambu recognises and respect the rights of local women and men",
      value == "harvest_firewd" ~ "Some community members have the right to harvest firewood",
      value == "staff_violate" ~ "Law enforcement staff violate the law or local people’s rights"))
 
  all_by_location_list[[location]] <- all_by_location
}

# Function to create ggplot for each location
create_ggplot <- function(df) {
  ggplot(df, aes(x = reorder(value, proportion), y = proportion, fill = variable)) +
    geom_col(position = "stack") +
    scale_fill_manual(values = c("Disagree" = "#7B3294", "Neutral" = "#C2A5CF", "Agree" = "#008837", "Dont Know" = "#EFEBF0")) +
    guides(fill = guide_legend(title = NULL)) +
    labs(x = "", y = "Governance theme: Rights") +
    theme_sjplot() + 
    theme(legend.position = "bottom") +
    scale_x_discrete(
      breaks = df$value,
      labels = str_wrap(df$value, width = 40)  # Adjust width as needed
    )
}
# Create the ggplots for each location with horizontal bars
p_kip <- create_ggplot(all_by_location_list$Kiptangwanyi) +
  coord_flip()
p_kip <- p_kip + labs(title="Kiptangwanyi")

p_ol <- create_ggplot(all_by_location_list$OlJorai) +
  coord_flip()
p_ol <- p_ol + labs(title="OlJorai")

p_mba <- create_ggplot(all_by_location_list$Mbaruk) +
  coord_flip()
p_mba <- p_mba + labs(title="Mbaruk")

p_soy <- create_ggplot(all_by_location_list$Soysambu) +
  coord_flip()
p_soy <- p_soy + labs(title="Soysambu")

agree <- ggarrange(p_kip + theme(legend.position="none", 
                               axis.title.y = element_blank(),
                               axis.title.x = element_blank()),
                 p_ol + theme(legend.position="none", axis.text.y = element_blank(),
                              axis.ticks.y = element_blank(),
                              axis.title.y = element_blank(),
                              axis.title.x = element_blank()),
                 p_mba + theme(legend.position="bottom", axis.text.y = element_blank(),
                               axis.ticks.y = element_blank()), 
                 p_soy + theme(legend.position="none",axis.text.y = element_blank(),
                               axis.ticks.y = element_blank(),
                               axis.title.y = element_blank(),
                               axis.title.x = element_blank()),
                 nrow = 1
                 # labels = c("Kiptangwanyi", "OlJorai", "Mbaruk", "Soysambu"),
                 # label.args = list(hjust = -0.8, vjust = 0.9)
)

ggsave(agree, filename = here::here("images", "agree disagree governance theme rights.png"), width = 1300, height = 500, dpi = 140, units = "px")


# agree/disagree statements participation --------------------------------- 

variables <- c("partcipt_decisions",
  "community_rep",
  "community_rep_select",
  "good_comms",
  "decision_influence"
)

# List of locations
locations <- c("OlJorai", "Kiptangwanyi", "Mbaruk", "Soysambu")

# Create an empty list to store results for each location
all_by_location_list <- list()

for (location in locations) {
  # Create empty frame to store the results for the current location
  all_by_location <- data.frame()
  
  for (variable in variables) {
    by_location <- dclus2 %>% 
      filter(locat == location) %>%  # Filter data for the specific location
      group_by_at(vars({{variable}})) %>% 
      summarise(
        proportion = survey_mean(vartype = "ci", na.rm = TRUE),
        total = survey_total(vartype = "ci", na.rm = TRUE),
        n = unweighted(n())) %>% 
      filter(!variable == "I do not want to answer") %>% 
      mutate(responses := fct_relevel({{variable}}, "Disagree", "Neutral", "Agree", "Dont Know")) %>%
      # Pivot to long format
      pivot_longer(
        cols = starts_with("responses"),
        names_to = "variable",
        values_to = "value"
      )
    
    all_by_location <- bind_rows(all_by_location, by_location)
  }
  
  all_by_location <- all_by_location %>%
    mutate(variable = coalesce(partcipt_decisions,
                               community_rep,
                               community_rep_select,
                               good_comms,
                               decision_influence)) %>%  
    select(!c("partcipt_decisions",
              "community_rep",
              "community_rep_select",
              "good_comms",
              "decision_influence")) %>%
    filter(!variable == "I do not want to answer") %>% 
    mutate(variable = fct_relevel(variable, "Disagree", "Neutral", "Agree", "Dont Know")) %>% 
    mutate(value = case_when(
     value == "partcipt_decisions" ~ "Local people participate in Soysambu's decision-making that impacts community",
      value == "community_rep" ~ "Know community representative for meetings with Soysambu",
      value == "community_rep_select" ~ "Community representative for meetings with Soysambu were properly selected",
      value == "good_comms" ~ "Communication between community rep and community members is good",
      value == "decision_influence" ~ "Soysambu decisions are often influenced by suggestions from local communities"
     ))
  
  all_by_location_list[[location]] <- all_by_location
}

# Function to create ggplot for each location
create_ggplot <- function(df) {
  ggplot(df, aes(x = reorder(value, proportion), y = proportion, fill = variable)) +
    geom_col(position = "stack") +
    scale_fill_manual(values = c("Disagree" = "#7B3294", "Neutral" = "#C2A5CF", "Agree" = "#008837", "Dont Know" = "#EFEBF0")) +
    guides(fill = guide_legend(title = NULL)) +
    labs(x = "", y = "Governance theme: Participation") +
    theme_sjplot() + 
    theme(legend.position = "bottom") +
    scale_x_discrete(
      breaks = df$value,
      labels = str_wrap(df$value, width = 40)  # Adjust width as needed
    )
}
# Create the ggplots for each location with horizontal bars
p_kip <- create_ggplot(all_by_location_list$Kiptangwanyi) +
  coord_flip()
p_kip <- p_kip + labs(title="Kiptangwanyi")

p_ol <- create_ggplot(all_by_location_list$OlJorai) +
  coord_flip()
p_ol <- p_ol + labs(title="OlJorai")

p_mba <- create_ggplot(all_by_location_list$Mbaruk) +
  coord_flip()
p_mba <- p_mba + labs(title="Mbaruk")

p_soy <- create_ggplot(all_by_location_list$Soysambu) +
  coord_flip()
p_soy <- p_soy + labs(title="Soysambu")

agree <- ggarrange(p_kip + theme(legend.position="none", 
                                 axis.title.y = element_blank(),
                                 axis.title.x = element_blank()),
                   p_ol + theme(legend.position="none", axis.text.y = element_blank(),
                                axis.ticks.y = element_blank(),
                                axis.title.y = element_blank(),
                                axis.title.x = element_blank()),
                   p_mba + theme(legend.position="bottom", axis.text.y = element_blank(),
                                 axis.ticks.y = element_blank()), 
                   p_soy + theme(legend.position="none",axis.text.y = element_blank(),
                                 axis.ticks.y = element_blank(),
                                 axis.title.y = element_blank(),
                                 axis.title.x = element_blank()),
                   nrow = 1
                   # labels = c("Kiptangwanyi", "OlJorai", "Mbaruk", "Soysambu"),
                   # label.args = list(hjust = -0.8, vjust = 0.9)
)

ggsave(agree, filename = here::here("images", "agree disagree governance theme participation.png"), width = 1300, height = 500, dpi = 140, units = "px")

# agree/disagree statements transparency --------------------------------- 

variables <- c("access_info",
  "annual_meetings",
  "info_sharing"
)

# List of locations
locations <- c("OlJorai", "Kiptangwanyi", "Mbaruk", "Soysambu")

# Create an empty list to store results for each location
all_by_location_list <- list()

for (location in locations) {
  # Create empty frame to store the results for the current location
  all_by_location <- data.frame()
  
  for (variable in variables) {
    by_location <- dclus2 %>% 
      filter(locat == location) %>%  # Filter data for the specific location
      group_by_at(vars({{variable}})) %>% 
      summarise(
        proportion = survey_mean(vartype = "ci", na.rm = TRUE),
        total = survey_total(vartype = "ci", na.rm = TRUE),
        n = unweighted(n())) %>% 
      filter(!variable == "I do not want to answer") %>% 
      mutate(responses := fct_relevel({{variable}}, "Disagree", "Neutral", "Agree", "Dont Know")) %>%
      # Pivot to long format
      pivot_longer(
        cols = starts_with("responses"),
        names_to = "variable",
        values_to = "value"
      )
    
    all_by_location <- bind_rows(all_by_location, by_location)
  }
  
  all_by_location <- all_by_location %>%
    mutate(variable = coalesce(access_info,
                               annual_meetings,
                               info_sharing)) %>%  
    select(!c("access_info",
              "annual_meetings",
              "info_sharing"
              )) %>%
    filter(!variable == "I do not want to answer") %>% 
    mutate(variable = fct_relevel(variable, "Disagree", "Neutral", "Agree", "Dont Know")) %>% 
    mutate(value = case_when(
      value == "access_info" ~ "People have timely access to information about decisions made by Soysambu which will affect them",
      value == "annual_meetings" ~ "There are meetings at least once a year between Soysambu and community to keep them well informed",
      value == "info_sharing" ~ "Community share information with Soysambu about key issues"
      ))
  
  all_by_location_list[[location]] <- all_by_location
}

# Function to create ggplot for each location
create_ggplot <- function(df) {
  ggplot(df, aes(x = reorder(value, proportion), y = proportion, fill = variable)) +
    geom_col(position = "stack") +
    scale_fill_manual(values = c("Disagree" = "#7B3294", "Neutral" = "#C2A5CF", "Agree" = "#008837", "Dont Know" = "#EFEBF0")) +
    guides(fill = guide_legend(title = NULL)) +
    labs(x = "", y = "Governance theme: Transparency") +
    theme_sjplot() + 
    theme(legend.position = "bottom") +
    scale_x_discrete(
      breaks = df$value,
      labels = str_wrap(df$value, width = 40)  # Adjust width as needed
    )
}
# Create the ggplots for each location with horizontal bars
p_kip <- create_ggplot(all_by_location_list$Kiptangwanyi) +
  coord_flip()
p_kip <- p_kip + labs(title="Kiptangwanyi")

p_ol <- create_ggplot(all_by_location_list$OlJorai) +
  coord_flip()
p_ol <- p_ol + labs(title="OlJorai")

p_mba <- create_ggplot(all_by_location_list$Mbaruk) +
  coord_flip()
p_mba <- p_mba + labs(title="Mbaruk")

p_soy <- create_ggplot(all_by_location_list$Soysambu) +
  coord_flip()
p_soy <- p_soy + labs(title="Soysambu")

agree <- ggarrange(p_kip + theme(legend.position="none", 
                                 axis.title.y = element_blank(),
                                 axis.title.x = element_blank()),
                   p_ol + theme(legend.position="none", axis.text.y = element_blank(),
                                axis.ticks.y = element_blank(),
                                axis.title.y = element_blank(),
                                axis.title.x = element_blank()),
                   p_mba + theme(legend.position="bottom", axis.text.y = element_blank(),
                                 axis.ticks.y = element_blank()), 
                   p_soy + theme(legend.position="none",axis.text.y = element_blank(),
                                 axis.ticks.y = element_blank(),
                                 axis.title.y = element_blank(),
                                 axis.title.x = element_blank()),
                   nrow = 1
                   # labels = c("Kiptangwanyi", "OlJorai", "Mbaruk", "Soysambu"),
                   # label.args = list(hjust = -0.8, vjust = 0.9)
)

ggsave(agree, filename = here::here("images", "agree disagree governance theme transparency.png"), width = 1300, height = 500, dpi = 140, units = "px")

# agree/disagree statements mitigation --------------------------------- 

variables <- c(
  "mitigate_negative",
  "system_wild_conf",
  "help_wild_conf",
  "measures_wild_conf"
)

# List of locations
locations <- c("OlJorai", "Kiptangwanyi", "Mbaruk", "Soysambu")

# Create an empty list to store results for each location
all_by_location_list <- list()

for (location in locations) {
  # Create empty frame to store the results for the current location
  all_by_location <- data.frame()
  
  for (variable in variables) {
    by_location <- dclus2 %>% 
      filter(locat == location) %>%  # Filter data for the specific location
      group_by_at(vars({{variable}})) %>% 
      summarise(
        proportion = survey_mean(vartype = "ci", na.rm = TRUE),
        total = survey_total(vartype = "ci", na.rm = TRUE),
        n = unweighted(n())) %>% 
      filter(!variable == "I do not want to answer") %>% 
      mutate(responses := fct_relevel({{variable}}, "Disagree", "Neutral", "Agree", "Dont Know")) %>%
      # Pivot to long format
      pivot_longer(
        cols = starts_with("responses"),
        names_to = "variable",
        values_to = "value"
      )
    
    all_by_location <- bind_rows(all_by_location, by_location)
  }
  
  all_by_location <- all_by_location %>%
    mutate(variable = coalesce(mitigate_negative,
                               system_wild_conf,
                               help_wild_conf,
                               measures_wild_conf)) %>%  
    select(!c("mitigate_negative",
              "system_wild_conf",
              "help_wild_conf",
              "measures_wild_conf")) %>%
    filter(!variable == "I do not want to answer") %>% 
    mutate(variable = fct_relevel(variable, "Disagree", "Neutral", "Agree", "Dont Know")) %>% 
    mutate(value = case_when(
      value == "mitigate_negative" ~ "Soysambu use effective measures to mitigate negative impacts of conservancy",
      value == "system_wild_conf" ~ "There's a good system for collecting information on damage by wild animals who spend most of their time on Soysambu",
      value == "help_wild_conf" ~ "Soysambu help when there are serious issues of damage by wild animals who spend most of their time on Soysambu",
      value == "measures_wild_conf" ~ "Measures to reduce crop damage by wild animals who spend most of their time on Soysambu work well" ))
  
  all_by_location_list[[location]] <- all_by_location
}

# Function to create ggplot for each location
create_ggplot <- function(df) {
  ggplot(df, aes(x = reorder(value, proportion), y = proportion, fill = variable)) +
    geom_col(position = "stack") +
    scale_fill_manual(values = c("Disagree" = "#7B3294", "Neutral" = "#C2A5CF", "Agree" = "#008837", "Dont Know" = "#EFEBF0")) +
    guides(fill = guide_legend(title = NULL)) +
    labs(x = "", y = "Governance theme: Mitigation of negative impacts") +
    theme_sjplot() + 
    theme(legend.position = "bottom") +
    scale_x_discrete(
      breaks = df$value,
      labels = str_wrap(df$value, width = 40)  # Adjust width as needed
    )
}
# Create the ggplots for each location with horizontal bars
p_kip <- create_ggplot(all_by_location_list$Kiptangwanyi) +
  coord_flip()
p_kip <- p_kip + labs(title="Kiptangwanyi")

p_ol <- create_ggplot(all_by_location_list$OlJorai) +
  coord_flip()
p_ol <- p_ol + labs(title="OlJorai")

p_mba <- create_ggplot(all_by_location_list$Mbaruk) +
  coord_flip()
p_mba <- p_mba + labs(title="Mbaruk")

p_soy <- create_ggplot(all_by_location_list$Soysambu) +
  coord_flip()
p_soy <- p_soy + labs(title="Soysambu")

agree <- ggarrange(p_kip + theme(legend.position="none", 
                                 axis.title.y = element_blank(),
                                 axis.title.x = element_blank()),
                   p_ol + theme(legend.position="none", axis.text.y = element_blank(),
                                axis.ticks.y = element_blank(),
                                axis.title.y = element_blank(),
                                axis.title.x = element_blank()),
                   p_mba + theme(legend.position="bottom", axis.text.y = element_blank(),
                                 axis.ticks.y = element_blank()), 
                   p_soy + theme(legend.position="none",axis.text.y = element_blank(),
                                 axis.ticks.y = element_blank(),
                                 axis.title.y = element_blank(),
                                 axis.title.x = element_blank()),
                   nrow = 1
                   # labels = c("Kiptangwanyi", "OlJorai", "Mbaruk", "Soysambu"),
                   # label.args = list(hjust = -0.8, vjust = 0.9)
)

ggsave(agree, filename = here::here("images", "agree disagree governance theme negative impact.png"), width = 1300, height = 500, dpi = 140, units = "px")

# agree/disagree statements benefit sharing --------------------------------- 

variables <- c("benefit_sharing",
  "women_influence"
)

# List of locations
locations <- c("OlJorai", "Kiptangwanyi", "Mbaruk", "Soysambu")

# Create an empty list to store results for each location
all_by_location_list <- list()

for (location in locations) {
  # Create empty frame to store the results for the current location
  all_by_location <- data.frame()
  
  for (variable in variables) {
    by_location <- dclus2 %>% 
      filter(locat == location) %>%  # Filter data for the specific location
      group_by_at(vars({{variable}})) %>% 
      summarise(
        proportion = survey_mean(vartype = "ci", na.rm = TRUE),
        total = survey_total(vartype = "ci", na.rm = TRUE),
        n = unweighted(n())) %>% 
      filter(!variable == "I do not want to answer") %>% 
      mutate(responses := fct_relevel({{variable}}, "Disagree", "Neutral", "Agree", "Dont Know")) %>%
      # Pivot to long format
      pivot_longer(
        cols = starts_with("responses"),
        names_to = "variable",
        values_to = "value"
      )
    
    all_by_location <- bind_rows(all_by_location, by_location)
  }
  
  all_by_location <- all_by_location %>%
    mutate(variable = coalesce(benefit_sharing,
                               women_influence)) %>%  
    select(!c("benefit_sharing",
              "women_influence")) %>%
    filter(!variable == "I do not want to answer") %>% 
    mutate(variable = fct_relevel(variable, "Disagree", "Neutral", "Agree", "Dont Know")) %>% 
    mutate(value = case_when(
      value == "benefit_sharing" ~ "Benefits from Soysambu are equitably shared within and between local communities",
      value == "women_influence" ~ "Women have as much influence as men in determining allocation of benefits from Soysambu"
    ))
  
  all_by_location_list[[location]] <- all_by_location
}

# Function to create ggplot for each location
create_ggplot <- function(df) {
  ggplot(df, aes(x = reorder(value, proportion), y = proportion, fill = variable)) +
    geom_col(position = "stack") +
    scale_fill_manual(values = c("Disagree" = "#7B3294", "Neutral" = "#C2A5CF", "Agree" = "#008837", "Dont Know" = "#EFEBF0")) +
    guides(fill = guide_legend(title = NULL)) +
    labs(x = "", y = "Governance theme: Benefit Sharing") +
    theme_sjplot() + 
    theme(legend.position = "bottom") +
    scale_x_discrete(
      breaks = df$value,
      labels = str_wrap(df$value, width = 40)  # Adjust width as needed
    )
}
# Create the ggplots for each location with horizontal bars
p_kip <- create_ggplot(all_by_location_list$Kiptangwanyi) +
  coord_flip()
p_kip <- p_kip + labs(title="Kiptangwanyi")

p_ol <- create_ggplot(all_by_location_list$OlJorai) +
  coord_flip()
p_ol <- p_ol + labs(title="OlJorai")

p_mba <- create_ggplot(all_by_location_list$Mbaruk) +
  coord_flip()
p_mba <- p_mba + labs(title="Mbaruk")

p_soy <- create_ggplot(all_by_location_list$Soysambu) +
  coord_flip()
p_soy <- p_soy + labs(title="Soysambu")

agree <- ggarrange(p_kip + theme(legend.position="none", 
                                 axis.title.y = element_blank(),
                                 axis.title.x = element_blank()),
                   p_ol + theme(legend.position="none", axis.text.y = element_blank(),
                                axis.ticks.y = element_blank(),
                                axis.title.y = element_blank(),
                                axis.title.x = element_blank()),
                   p_mba + theme(legend.position="bottom", axis.text.y = element_blank(),
                                 axis.ticks.y = element_blank()), 
                   p_soy + theme(legend.position="none",axis.text.y = element_blank(),
                                 axis.ticks.y = element_blank(),
                                 axis.title.y = element_blank(),
                                 axis.title.x = element_blank()),
                   nrow = 1
                   # labels = c("Kiptangwanyi", "OlJorai", "Mbaruk", "Soysambu"),
                   # label.args = list(hjust = -0.8, vjust = 0.9)
)

ggsave(agree, filename = here::here("images", "agree disagree governance theme benefit sharing.png"), width = 1300, height = 500, dpi = 140, units = "px")

