### Redundant Flextables ###


```{r}
# Summarize the data by village and drinking_water_dry
summary_by_village <- tanganyika_survey %>%
  filter(!is.na(drinking_water_dry) & drinking_water_dry != "I DON'T KNOW") %>% 
  group_by(village, drinking_water_dry) %>%
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE)  # Weighted proportion
  ) %>%
  ungroup()

# Reshape the data to have "Yes" and "No" as columns
proportion_table <- summary_by_village %>%
  select(village, drinking_water_dry, proportion) %>%
  pivot_wider(
    names_from = drinking_water_dry,    # Use `drinking_water_dry` values as column names
    values_from = proportion   # Fill with the proportion values
  )

# Rename columns for clarity
colnames(proportion_table) <- c("Village", "Neighbour's Open Well", "Neighbour's Tap", "Open Public Well", "Piped into Plot", "Pond or Lake", 
                                "Protected Public Well", "Protected Well in Plot", "Public Tap", "River or Stream", "Spring", "Neighbour's Borehole", "Open Well", "Water Vendor", "Piped into Dwelling", "Other")

# Format the proportion table values as percentages with one decimal point and add % sign, leave NA as blank
proportion_table <- proportion_table %>%
  mutate(across(c("Neighbour's Open Well", "Neighbour's Tap", "Open Public Well", "Piped into Plot", "Pond or Lake", 
                  "Protected Public Well", "Protected Well in Plot", "Public Tap", "River or Stream", "Spring", "Neighbour's Borehole", "Open Well", "Water Vendor", "Piped into Dwelling", "Other"),
                ~ ifelse(is.na(.), "", paste0(round(. * 100, 1), "%"))))

add_lot_link(caption = 'Main water source in the dry season at village level (%)')

# Create the flextable
proportion_table %>%
  sea::sea_table(
    dark_color = sea_colors("gunmetal"),
    pale_color = sea_colors("grey"),
    third_color = "white"
  ) %>%
  flextable::autofit()  
```
################################################################################


```{r}
# Summarize the data by village and drinking_water_wet
summary_by_village <- tanganyika_survey %>%
  filter(!is.na(drinking_water_wet) & drinking_water_wet != "I DON'T KNOW") %>% 
  group_by(village, drinking_water_wet) %>%
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE)  # Weighted proportion
  ) %>%
  ungroup()

# Reshape the data to have "Yes" and "No" as columns
proportion_table <- summary_by_village %>%
  select(village, drinking_water_wet, proportion) %>%
  pivot_wider(
    names_from = drinking_water_wet,    # Use `drinking_water_dry` values as column names
    values_from = proportion   # Fill with the proportion values
  )

# Rename columns for clarity
colnames(proportion_table) <- c("Village", "Open Public Well", "Piped into Plot", "Protected Public Well", "Rainwater", "River or Stream", "Pond or Lake", "Spring", "Public Tap", "Neighbour's Open Well", "Piped into Dwelling","Water Vendor", "Open Well", "Neighbour's Borehole")

# Format the proportion table values as percentages with one decimal point and add % sign, leave NA as blank
proportion_table <- proportion_table %>%
  mutate(across(c("Open Public Well", "Piped into Plot", "Protected Public Well", "Rainwater", "River or Stream", "Pond or Lake", "Spring", "Public Tap", "Neighbour's Open Well", "Piped into Dwelling","Water Vendor", "Open Well", "Neighbour's Borehole"),
                ~ ifelse(is.na(.), "", paste0(round(. * 100, 1), "%"))))

add_lot_link(caption = 'Main water source in the wet season at village level (%)')

# Create the flextable
proportion_table %>%
  sea::sea_table(
    dark_color = sea_colors("gunmetal"),
    pale_color = sea_colors("grey"),
    third_color = "white"
  ) %>%
  flextable::autofit()  
```
################################################################################


```{r}
household_item_data <- household_item_design %>%
  group_by(village, household_item) %>%
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())) %>% ungroup()

proportion_table <- household_item_data %>%
  select(village, household_item, proportion) %>%
  pivot_wider(
    names_from = household_item,    
    values_from = proportion   # Fill with the proportion values
  )

# Rename columns for clarity
colnames(proportion_table) <- c("Village", "Bed or Mattress", "Clock", "Generator", "Iron", "Mobile Phone", "Radio", "Refrigerator", "Smartphone", "Sofa", "Solar Panel", "Table", "TANESCO Power", "TV")

proportion_table <- proportion_table %>%
  mutate(across(c("Bed or Mattress", "Clock", "Generator", "Iron", "Mobile Phone", "Radio", "Refrigerator", "Smartphone", "Sofa", "Solar Panel", "Table", "TANESCO Power", "TV"),
                ~ ifelse(is.na(.), "", paste0(round(. * 100, 1), "%"))))

add_lot_link(caption = 'Asset ownership at village level (%)')

# Create the flextable
proportion_table %>%
  sea::sea_table(
    dark_color = sea_colors("gunmetal"),
    pale_color = sea_colors("grey"),
    third_color = "white"
  ) %>%
  flextable::width(width = 0.5)  
```
################################################################################


```{r}
# Reshape the data to have "Yes" and "No" as columns
proportion_table <- livelihood_activities_data %>%
  select(village, livelihood_activities, proportion) %>%
  pivot_wider(
    names_from = livelihood_activities,    
    values_from = proportion   # Fill with the proportion values
  )

# Rename columns for clarity
colnames(proportion_table) <- c("Village", "Agriculture", "Business", "Day Labor", "Fich Processing", "Fish Trading", "Fishing", "Livestock Keeping", "Other", "Remittances", "Tailor", "Boat Builder", "Employee", "Pensions")

# Format the proportion table values as percentages with one decimal point and add % sign, leave NA as blank
proportion_table <- proportion_table %>%
  mutate(across(c("Agriculture", "Business", "Day Labor", "Fich Processing", "Fish Trading", "Fishing", "Livestock Keeping", "Other", "Remittances", "Tailor", "Boat Builder", "Employee", "Pensions"),
                ~ ifelse(is.na(.), "", paste0(round(. * 100, 1), "%"))))

add_lot_link(caption = 'Livelihood activities at village level (%)')

# Create the flextable
proportion_table %>%
  sea::sea_table(
    dark_color = sea_colors("gunmetal"),
    pale_color = sea_colors("grey"),
    third_color = "white"
  ) %>%
  flextable::autofit()  

```
