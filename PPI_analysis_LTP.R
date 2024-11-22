# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyverse)
library(sea)
library(writexl)

# Load the data
file_path <- "LTP_Baseline_2024_Clean.rds"
survey_data <- readRDS(file_path)

# Ensure the age column is numeric for filtering youths
survey_data <- survey_data %>%
  mutate(hh_members = as.numeric(hh_members))

# Define energy_source column and add in Rukwa region column
survey_data <- survey_data %>%
  mutate(
    energy_source = case_when(
      solar_panel == "YES" | generator == "YES" ~ "solar or generator",
      tanesco_power == "YES" ~ "electricity", TRUE ~ "other"), region = "Rukwa")

# Columns of interest
columns_of_interest <- c("region", "hh_members", "beef", "milk", "rice", "flour", "wall_material", "energy_source", "iron","table")

# Define the PPI scorecard
upper_ppi_scorecard <- list(
  region = c("Rukwa" = 2),
  hh_members = c("1" = 32, "2" =32, "3" = 32, "4" = 10, "5" = 10, "6" = 10, "7" = 0, "8" = 0, "9" = 0, "10" = 0, "11" = 0, "12" = 0, "13" = 0, "14" = 0),
  beef = c("Yes" = 7, "No" = 0),
  milk = c("Yes" = 7, "No" = 0),
  rice = c("Yes" = 6, "No" = 0),
  flour = c("Yes" = 7, "No" = 0),
  wall_material = c("BAKED BRICKS" = 0, "GRASS/REEDS" =0, "CEMENT BRICKS" = 2, "SUNDRIED BRICKS" = 0, "POLES AND MUD" = 0, "IRON SHEETS" = 0,  "STONES" =2),
  energy_source = c("electricity" = 11, "solar or generator" = 3, "other" = 0),
  iron = c("Yes" = 10, "No" = 0),
  table = c("Yes" = 3, "No" = 0)
)

lower_ppi_scorecard <- list(
  region = c("Rukwa" = 2),
  hh_members = c("1" = 27, "2" =27, "3" = 27, "4" = 10, "5" = 10, "6" = 10, "7" = 0, "8" = 0, "9" = 0, "10" = 0, "11" = 0, "12" = 0, "13" = 0, "14" = 0),
  beef = c("Yes" = 8, "No" = 0),
  milk = c("Yes" = 8, "No" = 0),
  rice = c("Yes" = 7, "No" = 0),
  flour = c("Yes" = 8, "No" = 0),
  wall_material = c("BAKED BRICKS" = 0, "GRASS/REEDS" =0, "CEMENT BRICKS" = 2, "SUNDRIED BRICKS" = 0, "POLES AND MUD" = 0, "IRON SHEETS" = 0,  "STONES" =2),
  energy_source = c("electricity" = 11, "solar or generator" = 4, "other" = 0),
  iron = c("Yes" = 9, "No" = 0),
  table = c("Yes" = 4, "No" = 0))

extreme_ppi_scorecard <- list(
  region = c("Rukwa" = 3),
  hh_members = c("1" = 22, "2" =22, "3" = 22, "4" = 9, "5" = 9, "6" = 9, "7" = 0, "8" = 0, "9" = 0, "10" = 0, "11" = 0, "12" = 0, "13" = 0, "14" = 0),
  beef = c("Yes" = 9, "No" = 0),
  milk = c("Yes" = 7, "No" = 0),
  rice = c("Yes" = 8, "No" = 0),
  flour = c("Yes" = 9, "No" = 0),
  wall_material = c("BAKED BRICKS" = 0, "GRASS/REEDS" =0, "CEMENT BRICKS" = 0, "SUNDRIED BRICKS" = 0, "POLES AND MUD" = 0, "IRON SHEETS" = 0,  "STONES" =0),
  energy_source = c("electricity" = 9, "solar or generator" = 6, "other" = 0),
  iron = c("Yes" = 11, "No" = 0),
  table = c("Yes" = 6, "No" = 0))

# Function to compute PPI score for a single row
compute_ppi_score <- function(row, scorecard) {
  score <- 0
  for (col in names(scorecard)) {
    response <- as.character(row[[col]])
    if (!is.na(response) && response %in% names(scorecard[[col]])) {
      score <- score + scorecard[[col]][response]
    }
  }
  return(score)
}

# Add PPI scores to the survey data
survey_data <- survey_data %>%
  rowwise() %>%
  mutate(upper_PPI_Score = compute_ppi_score(cur_data(), upper_ppi_scorecard))
survey_data <- survey_data %>%
  rowwise() %>%
  mutate(lower_PPI_Score = compute_ppi_score(cur_data(), lower_ppi_scorecard))
survey_data <- survey_data %>%
  rowwise() %>%
  mutate(extreme_PPI_Score = compute_ppi_score(cur_data(), extreme_ppi_scorecard))

# 2018 Tanzania PPI Look-Up Table – Upper National Poverty Line
upper_lookup_table <- list(
  "0" = 88.6, "1" = 87.8, "2" = 86.9, "3" = 86.0, "4" = 85.0,
  "5" = 83.9, "6" = 82.8, "7" = 81.7, "8" = 80.4, "9" = 79.2,
  "10" = 77.8, "11" = 76.4, "12" = 75.0, "13" = 73.4, "14" = 71.9,
  "15" = 70.2, "16" = 68.6, "17" = 66.8, "18" = 65.0, "19" = 63.2,
  "20" = 61.3, "21" = 59.4, "22" = 57.5, "23" = 55.6, "24" = 53.6,
  "25" = 51.6, "26" = 49.6, "27" = 47.6, "28" = 45.7, "29" = 43.7,
  "30" = 41.8, "31" = 39.8, "32" = 38.0, "33" = 36.1, "34" = 34.3,
  "35" = 32.5, "36" = 30.8, "37" = 29.1, "38" = 27.5, "39" = 26.0,
  "40" = 24.5, "41" = 23.0, "42" = 21.7, "43" = 20.3, "44" = 19.1,
  "45" = 17.9, "46" = 16.7, "47" = 15.7, "48" = 14.7, "49" = 13.7,
  "50" = 12.8, "51" = 11.9, "52" = 11.1, "53" = 10.3, "54" = 9.6,
  "55" = 9.0, "56" = 8.3, "57" = 7.7, "58" = 7.2, "59" = 6.7,
  "60" = 6.2, "61" = 5.8, "62" = 5.3, "63" = 5.0, "64" = 4.6,
  "65" = 4.3, "66" = 3.9, "67" = 3.7, "68" = 3.4, "69" = 3.1,
  "70" = 2.9, "71" = 2.7, "72" = 2.5, "73" = 2.3, "74" = 2.1,
  "75" = 2.0, "76" = 1.8, "77" = 1.7, "78" = 1.6, "79" = 1.4,
  "80" = 1.3, "81" = 1.2, "82" = 1.1, "83" = 1.1, "84" = 1.0,
  "85" = 0.9, "86" = 0.8, "87" = 0.8, "88" = 0.7, "89" = 0.7,
  "90" = 0.6, "91" = 0.6, "92" = 0.5, "93" = 0.5, "94" = 0.4,
  "95" = 0.4, "96" = 0.4, "97" = 0.3, "98" = 0.3, "99" = 0.3,
  "100" = 0.3)

# 2018 Tanzania PPI Look-Up Table – Lower National Poverty Line
lower_lookup_table <- list(
  "0" = 86.3, "1" = 85.3, "2" = 84.2, "3" = 83.0, "4" = 81.8,
  "5" = 80.5, "6" = 79.1, "7" = 77.7, "8" = 76.2, "9" = 74.6,
  "10" = 73.0, "11" = 71.3, "12" = 69.6, "13" = 67.8, "14" = 65.9,
  "15" = 64.0, "16" = 62.0, "17" = 60.0, "18" = 57.9, "19" = 55.9,
  "20" = 53.8, "21" = 51.7, "22" = 49.6, "23" = 47.5, "24" = 45.4,
  "25" = 43.3, "26" = 41.2, "27" = 39.2, "28" = 37.2, "29" = 35.3,
  "30" = 33.4, "31" = 31.5, "32" = 29.7, "33" = 28.0, "34" = 26.3,
  "35" = 24.7, "36" = 23.2, "37" = 21.7, "38" = 20.3, "39" = 19.0,
  "40" = 17.7, "41" = 16.5, "42" = 15.4, "43" = 14.3, "44" = 13.3,
  "45" = 12.4, "46" = 11.5, "47" = 10.7, "48" = 9.9, "49" = 9.2,
  "50" = 8.5, "51" = 7.8, "52" = 7.3, "53" = 6.7, "54" = 6.2,
  "55" = 5.7, "56" = 5.3, "57" = 4.9, "58" = 4.5, "59" = 4.2,
  "60" = 3.8, "61" = 3.5, "62" = 3.3, "63" = 3.0, "64" = 2.8,
  "65" = 2.5, "66" = 2.3, "67" = 2.2, "68" = 2.0, "69" = 1.8,
  "70" = 1.7, "71" = 1.6, "72" = 1.4, "73" = 1.3, "74" = 1.2,
  "75" = 1.1, "76" = 1.0, "77" = 0.9, "78" = 0.9, "79" = 0.8,
  "80" = 0.7, "81" = 0.7, "82" = 0.6, "83" = 0.6, "84" = 0.5,
  "85" = 0.5, "86" = 0.4, "87" = 0.4, "88" = 0.4, "89" = 0.3,
  "90" = 0.3, "91" = 0.3, "92" = 0.3, "93" = 0.2, "94" = 0.2,
  "95" = 0.2, "96" = 0.2, "97" = 0.2, "98" = 0.2, "99" = 0.1,
  "100" = 0.1)

# 2018 Tanzania PPI Look-Up Table – Extreme National Poverty Line
extreme_lookup_table <- list(
  "0" = 63.0, "1" = 60.7, "2" = 58.4, "3" = 56.1, "4" = 53.8,
  "5" = 51.4, "6" = 49.0, "7" = 46.6, "8" = 44.3, "9" = 41.9,
  "10" = 39.6, "11" = 37.4, "12" = 35.2, "13" = 33.0, "14" = 31.0,
  "15" = 29.0, "16" = 27.0, "17" = 25.2, "18" = 23.4, "19" = 21.8,
  "20" = 20.2, "21" = 18.7, "22" = 17.3, "23" = 16.0, "24" = 14.7,
  "25" = 13.6, "26" = 12.5, "27" = 11.5, "28" = 10.6, "29" = 9.7,
  "30" = 8.9, "31" = 8.2, "32" = 7.5, "33" = 6.8, "34" = 6.3,
  "35" = 5.7, "36" = 5.2, "37" = 4.8, "38" = 4.4, "39" = 4.0,
  "40" = 3.6, "41" = 3.3, "42" = 3.0, "43" = 2.8, "44" = 2.5,
  "45" = 2.3, "46" = 2.1, "47" = 1.9, "48" = 1.7, "49" = 1.6,
  "50" = 1.4, "51" = 1.3, "52" = 1.2, "53" = 1.1, "54" = 1.0,
  "55" = 0.9, "56" = 0.8, "57" = 0.7, "58" = 0.7, "59" = 0.6,
  "60" = 0.6, "61" = 0.5, "62" = 0.5, "63" = 0.4, "64" = 0.4,
  "65" = 0.3, "66" = 0.3, "67" = 0.3, "68" = 0.3, "69" = 0.2,
  "70" = 0.2, "71" = 0.2, "72" = 0.2, "73" = 0.2, "74" = 0.1,
  "75" = 0.1, "76" = 0.1, "77" = 0.1, "78" = 0.1, "79" = 0.1,
  "80" = 0.1, "81" = 0.1, "82" = 0.1, "83" = 0.1, "84" = 0.1,
  "85" = 0.1, "86" = 0.0, "87" = 0.0, "88" = 0.0, "89" = 0.0,
  "90" = 0.0, "91" = 0.0, "92" = 0.0, "93" = 0.0, "94" = 0.0,
  "95" = 0.0, "96" = 0.0, "97" = 0.0, "98" = 0.0, "99" = 0.0,
  "100" = 0.0)


# Function to compute the average poverty likelihood for a group
compute_poverty_likelihood <- function(ppi_scores, lookup_table) {
  poverty_likelihoods <- sapply(ppi_scores, function(score) {
    if (is.null(lookup_table[as.character(score)])) {
      return(NA)  # Handle unmatched scores
    }
    return(lookup_table[[as.character(score)]])
  })
  return(mean(as.numeric(poverty_likelihoods), na.rm = TRUE))
}

# Compute PPI score for the overall survey group
upper_overall_poverty_likelihood <- compute_poverty_likelihood(survey_data$upper_PPI_Score, upper_lookup_table)
lower_overall_poverty_likelihood <- compute_poverty_likelihood(survey_data$lower_PPI_Score, lower_lookup_table)
extreme_overall_poverty_likelihood <- compute_poverty_likelihood(survey_data$extreme_PPI_Score, extreme_lookup_table)

# Compute PPI score for males
upper_male_poverty_likelihood <- compute_poverty_likelihood(
  survey_data %>% filter(sex == "MALE") %>% pull(upper_PPI_Score),
  upper_lookup_table)
lower_male_poverty_likelihood <- compute_poverty_likelihood(
  survey_data %>% filter(sex == "MALE") %>% pull(lower_PPI_Score),
  lower_lookup_table)
extreme_male_poverty_likelihood <- compute_poverty_likelihood(
  survey_data %>% filter(sex == "MALE") %>% pull(extreme_PPI_Score),
  extreme_lookup_table)

# Compute PPI score for females
upper_female_poverty_likelihood <- compute_poverty_likelihood(
  survey_data %>% filter(sex == "FEMALE") %>% pull(upper_PPI_Score),
  upper_lookup_table)
lower_female_poverty_likelihood <- compute_poverty_likelihood(
  survey_data %>% filter(sex == "FEMALE") %>% pull(lower_PPI_Score),
  lower_lookup_table)
extreme_female_poverty_likelihood <- compute_poverty_likelihood(
  survey_data %>% filter(sex == "FEMALE") %>% pull(extreme_PPI_Score),
  extreme_lookup_table)

# Compute PPI score for youths (youth is defined as age < 35)
upper_youth_poverty_likelihood <- compute_poverty_likelihood(
  survey_data %>% filter(age < 35) %>% pull(upper_PPI_Score),
  upper_lookup_table)
lower_youth_poverty_likelihood <- compute_poverty_likelihood(
  survey_data %>% filter(age < 35) %>% pull(lower_PPI_Score),
  lower_lookup_table)
extreme_youth_poverty_likelihood <- compute_poverty_likelihood(
  survey_data %>% filter(age < 35) %>% pull(extreme_PPI_Score),
  extreme_lookup_table)

# Print results
cat("Upper Overall Poverty Likelihood:", upper_overall_poverty_likelihood, "\n")
cat("Upper Male Poverty Likelihood:", upper_male_poverty_likelihood, "\n")
cat("Upper Female Poverty Likelihood:", upper_female_poverty_likelihood, "\n")
cat("Upper Youth Poverty Likelihood:", upper_youth_poverty_likelihood, "\n")

cat("Lower Overall Poverty Likelihood:", lower_overall_poverty_likelihood, "\n")
cat("Lower Male Poverty Likelihood:", lower_male_poverty_likelihood, "\n")
cat("Lower Female Poverty Likelihood:", lower_female_poverty_likelihood, "\n")
cat("Lower Youth Poverty Likelihood:", lower_youth_poverty_likelihood, "\n")

cat("Extreme Overall Poverty Likelihood:", extreme_overall_poverty_likelihood, "\n")
cat("Extreme Male Poverty Likelihood:", extreme_male_poverty_likelihood, "\n")
cat("Extreme Female Poverty Likelihood:", extreme_female_poverty_likelihood, "\n")
cat("Extreme Youth Poverty Likelihood:", extreme_youth_poverty_likelihood, "\n")

results_df <- data.frame(
  Category = c("Overall", "Male", "Female", "Youth"),
  Upper = c(upper_overall_poverty_likelihood, upper_male_poverty_likelihood, upper_female_poverty_likelihood, upper_youth_poverty_likelihood),
  Lower = c(lower_overall_poverty_likelihood, lower_male_poverty_likelihood, lower_female_poverty_likelihood, lower_youth_poverty_likelihood),
  Extreme = c(extreme_overall_poverty_likelihood, extreme_male_poverty_likelihood, extreme_female_poverty_likelihood, extreme_youth_poverty_likelihood)
)

# Write the results to an .xlsx file
write_xlsx(results_df, "images/Poverty_Likelihood_Results.xlsx")

################################################################################

# Create village clusters for plotting
cluster_1_villages <- c("IZINGA", "KALA", "KATENGE", "KILAMBO CHA MKOLECHI", 
                        "KIZUMBI", "LYAPINDA", "MPASA", "MWINZA", "NG'ANGA", 
                        "TUNDU", "WAMPEMBE")
cluster_2_villages <- c("ISASA", "KALUNGU", "KICHANGANI", "KIPILI", 
                        "MAJENGO MAPYA", "MANDA KERENGE", "MANDA UHURU", 
                        "MKINGA", "MTAKUJA", "NTANGANYIKA")

# Define the clusters (Assume `cluster_1_villages` and `cluster_2_villages` are predefined)
survey_data <- survey_data %>%
  mutate(cluster = case_when(
    village %in% cluster_1_villages ~ "Cluster 1",
    village %in% cluster_2_villages ~ "Cluster 2",
    TRUE ~ "Other"))

# Compute poverty likelihood by village for upper, lower, and extreme levels
village_poverty_likelihood <- survey_data %>%
  group_by(village, cluster) %>%
  summarise(
    upper_poverty = compute_poverty_likelihood(upper_PPI_Score, upper_lookup_table),
    lower_poverty = compute_poverty_likelihood(lower_PPI_Score, lower_lookup_table),
    extreme_poverty = compute_poverty_likelihood(extreme_PPI_Score, extreme_lookup_table))

# Reshape the data for easier plotting
village_poverty_likelihood_long <- village_poverty_likelihood %>%
  pivot_longer(
    cols = c(upper_poverty, lower_poverty, extreme_poverty),
    names_to = "poverty_level",
    values_to = "poverty_likelihood")

# Create the plot
ggplot(village_poverty_likelihood_long, aes(x = village, y = poverty_likelihood, 
                                            group = poverty_level, fill = poverty_level)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  guides(fill = guide_legend(title = NULL)) +
  labs(x = "Village", y = "Percentage of Poverty Likelihood") +
  scale_fill_manual(values = SEA_palette,  labels = c(
    "extreme_poverty" = "Extreme Poverty Line", 
    "lower_poverty" = "Lower National Poverty Line", 
    "upper_poverty" = "Upper National Poverty Line")) +
  labs(x = "", y = "Percentage of Households") +
  sea::theme_sea() + 
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.justification = "center", 
        axis.text.x = element_text(size = 7), strip.text = element_blank(), legend.box = "horizontal") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  facet_wrap(~cluster, ncol = 1, scales = "free_x") + 
  theme(axis.text.x = element_text(size = 10))  
