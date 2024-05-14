library(tidyverse)
library(lubridate)
library(scales)
library(googlesheets4)
library(stats)
library(stats4)
library(survey)
library(srvyr, warn.conflicts = FALSE)
library(sjPlot)
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)

######################################################################################################################
####### import cleaned household survey data
######################################################################################################################

rm(list=ls())



#######################################################################################################################
###### constructing wealth index #####
#######################################################################################################################
#### 

## this version has the wealth index already from data_cleaning.R
hhs2 <- readRDS("hhs_cleaned_wealth.rds")

# this version is without the wealth index
hhs2 <- readRDS("hhs_cleaned.rds")

#######################################################################################################################
###### constructing wealth index #####
#######################################################################################################################


# simplest way is just to sum all the assets ######

hhs_pca_eda <- hhs2 %>% 
  select(dnum, snum, fpc1, fpc2, stype, inter_phon, norm_phon, radio, torch,  tv, elec, gen, 
         solar_elec, piki, vehicle, table, sofa, pit_lat, tank, mpesa,skip_meal, wall,fuel,
         total_tlu, crop_acre, "_index") %>% 
  mutate(inter_phon = ifelse(inter_phon == "Yes", 1, 0)) %>% 
  mutate(norm_phon = ifelse(norm_phon == "Yes", 1, 0)) %>% 
  mutate(radio = ifelse(radio == "Yes", 1, 0)) %>% 
  mutate(torch = ifelse(torch == "Yes", 1, 0)) %>% 
  mutate(tv = ifelse(tv == "Yes", 1, 0)) %>% 
  mutate(elec = ifelse(elec == "Yes", 1, 0)) %>% 
  mutate(gen = ifelse(gen == "Yes", 1, 0)) %>% 
  mutate(solar_elec = ifelse(solar_elec == "Yes", 1, 0)) %>% 
  mutate(piki = ifelse(piki == "Yes", 1, 0)) %>% 
  mutate(vehicle = ifelse(vehicle == "Yes", 1, 0)) %>% 
  mutate(table = ifelse(table == "Yes", 1, 0)) %>% 
  mutate(sofa = ifelse(sofa == "Yes", 1, 0)) %>% 
  mutate(pit_lat = ifelse(pit_lat == "Yes", 1, 0)) %>% 
  mutate(tank = ifelse(tank == "Yes", 1, 0)) %>% 
  mutate(mpesa = ifelse(mpesa == "Yes", 1, 0)) %>% 
  mutate(gas_fuel = ifelse(fuel == "Gas", 1, 0)) %>% 
  mutate(skip_meal = ifelse(skip_meal == "Never", 1, 0)) %>% 
  mutate(brick_cement_wall = ifelse(wall == "Bricks (or mud bricks) with Cement (Durable)", 1, 0)) %>% 
  mutate(crop_acre= ifelse(crop_acre > 1, 1, 0)) %>% 
  mutate(total_tlu = coalesce(total_tlu, 0)) %>% 
  mutate(total_tlu= ifelse(total_tlu > 1, 1, 0)) %>% 
  mutate(any_elec = ifelse(elec == 1 | solar_elec == 1, 1, 0)) %>% 
  mutate(sum_values = rowSums(select(., -c(dnum, snum, fpc1, fpc2, stype, wall, fuel, "_index")))) %>% 
  mutate(category = cut_number(sum_values, n = 5, labels = FALSE)) %>%
  select(-c(elec, solar_elec, wall, fuel))

hhs_pca_eda <- hhs_pca_eda %>% 
  mutate("_index" = row_number())
hhs_wealth <- full_join(hhs2, hhs_pca_eda, by = "_index")

saveRDS(hhs_wealth, "hhs_cleaned_wealth.rds")

# 1. exploratory data analysis of variables to potentially use in PCA ######

hhs_pca_eda <- hhs2 %>% 
  select(dnum, snum, fpc1, fpc2, stype, inter_phon, norm_phon, radio, torch,  tv, elec, gen, 
         solar_elec, piki, vehicle, table, sofa, pit_lat, tank, mpesa,skip_meal, wall,fuel,
         total_tlu, crop_acre, "_index") %>% 
  mutate(inter_phon = ifelse(inter_phon == "Yes", 1, 0)) %>% 
  mutate(norm_phon = ifelse(norm_phon == "Yes", 1, 0)) %>% 
  mutate(radio = ifelse(radio == "Yes", 1, 0)) %>% 
  mutate(torch = ifelse(torch == "Yes", 1, 0)) %>% 
  mutate(tv = ifelse(tv == "Yes", 1, 0)) %>% 
  mutate(elec = ifelse(elec == "Yes", 1, 0)) %>% 
  mutate(gen = ifelse(gen == "Yes", 1, 0)) %>% 
  mutate(solar_elec = ifelse(solar_elec == "Yes", 1, 0)) %>% 
  mutate(piki = ifelse(piki == "Yes", 1, 0)) %>% 
  mutate(vehicle = ifelse(vehicle == "Yes", 1, 0)) %>% 
  mutate(table = ifelse(table == "Yes", 1, 0)) %>% 
  mutate(sofa = ifelse(sofa == "Yes", 1, 0)) %>% 
  mutate(pit_lat = ifelse(pit_lat == "Yes", 1, 0)) %>% 
  mutate(tank = ifelse(tank == "Yes", 1, 0)) %>% 
  mutate(mpesa = ifelse(mpesa == "Yes", 1, 0)) %>% 
  mutate(gas_fuel = ifelse(fuel == "Gas", 1, 0)) %>% 
  mutate(skip_meal = ifelse(skip_meal == "Never", 1, 0)) %>% 
  mutate(brick_cement_wall = ifelse(wall == "Bricks (or mud bricks) with Cement (Durable)", 1, 0)) %>% 
  mutate(crop_acre= ifelse(crop_acre > 1, 1, 0)) %>% 
  mutate(total_tlu = coalesce(total_tlu, 0)) %>% 
  mutate(total_tlu= ifelse(total_tlu > 1, 1, 0)) %>% 
  mutate(any_elec = ifelse(elec == 1 | solar_elec == 1, 1, 0)) %>% 
  select(-c(elec, solar_elec, wall, fuel))
# 2. Select all the subset of variables to be used to construct the PCA and omit all NA
# this one contains all the ones possible including some continuous variables
#removed tlu as this excluded too many people
hhs_pca_eda_subset_all <- hhs_pca_eda %>% 
  select(!c(dnum, snum, fpc1, fpc2, stype, "_index", sofa, gas_fuel)) %>%
  na.omit()

### for all, checked all scaling and centering and both scaling and centering are needed - means it is a correlation matrix
hhs_pca_all <- prcomp(hhs_pca_eda_subset_all, center = TRUE, scale = TRUE)
summary(hhs_pca_all)
biplot(hhs_pca_all, main = "All variables with centering and scaling")

#boxplot of all the variables with red cross at mean 
plot <- hhs_pca_eda_subset_all 
#### for binary variables, checked all scaling and centering and only centering needed - means it is a covariance matrix 
hhs_pca_binary <- prcomp(plot, center = TRUE, scale = FALSE)
summary(hhs_pca_binary)
biplot(hhs_pca_binary, main = "Binary variables with centering and scaling")

# check out the loading scores for this data
loading_scores <- hhs_pca_all$rotation[,1]
w_score <- abs(loading_scores)
w_score_ranked <- sort(w_score, decreasing = TRUE) # sort them so we can just take out the most influention ones
top_10_scores <- names(w_score_ranked[1:10]) # show the most influential ones
top_10_scores
hhs_pca_all$rotation[top_10_scores,1] # show the associated scores for the most valuable ones



# 3. check for correlations between wealth indicators and explore data #########
library(corrr)
# using here() create a new folder in the Here() path location, called images
dir.create(here::here("images/wealth_index"))

cor_asset <- hhs_pca_eda_subset_all %>% 
  #replace(is.na(.),0) %>% 
  glimpse
cor_asset %>% 
  corrr::correlate() %>% 
  corrr::shave() %>% 
  corrr::fashion() %>% 
  readr::write_excel_csv(here::here("correlation_matrix.csv"))
# based on these correlations and the results of the score rankings, we removed sofa, and combined solar electricitry 
# and mains electricity as there were the only ones with scores greater than 0.4. we then removed any-elec as tv was sufficient
# total_tlu and crop_acre are also highly correlated
cor_asset %>% 
  corrr::correlate() %>% 
  corrr::rearrange(method = "HC", absolute = FALSE) %>% 
  corrr:::shave() %>% 
  corrr::rplot(shape=19, colors = c("red", "green")) %>% 
  ggplot2::ggsave(
    filename = here::here("images", "correlation of assets.png"), 
    width = 20, 
    height = 5)

#boxplot of all the variables with red cross at mean 
plot <- hhs_pca_eda_subset_all
ggplot(stack(plot), aes(x = reorder(ind, values, FUN = mean), y = values)) + 
  geom_boxplot(notch = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  stat_summary(fun.y = "mean", geom = "point", size = 5, color = "red", shape = 3) +
  stat_summary(fun.y = "median", geom = "point", size = 5, color = "blue", shape = 3)

#### for binary variables, checked all scaling and centering and only centering needed - means it is a covariance matrix 
hhs_pca_binary <- prcomp(plot, center = TRUE, scale = FALSE)
summary(hhs_pca_binary)
biplot(hhs_pca_binary, main = "Binary variables with centering and NO scaling")
# after this we removed these as almost everyone had them: norm_phon, radio, table, mpesa, pit_lat
cor_asset_binary <- plot %>% 
  #replace(is.na(.),0) %>% 
  glimpse
cor_asset_binary %>% 
  corrr::correlate() %>% 
  corrr::shave() %>% 
  corrr::fashion() %>% 
  readr::write_excel_csv(here::here("correlation_matrix_binary.csv"))

cor_asset_binary %>% 
  corrr::correlate() %>% 
  corrr::rearrange(method = "HC", absolute = FALSE) %>% 
  corrr:::shave() %>% 
  corrr::rplot(shape=19, colors = c("red", "green")) %>% 
  ggplot2::ggsave(
    filename = here::here("images", "correlation of assets_binary.png"), 
    width = 20, 
    height = 5)

#boxplot of all the variables with red cross at mean 
plot <- hhs_pca_eda_subset_all #%>% 
  select(-id)
ggplot(stack(plot), aes(x = reorder(ind, values, FUN = mean), y = values)) + 
  geom_boxplot(notch = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  stat_summary(fun.y = "mean", geom = "point", size = 5, color = "red", shape = 3) +
  stat_summary(fun.y = "median", geom = "point", size = 5, color = "blue", shape = 3)
ggsave(filename = here::here("images", "wealth_index_boxplot.png"))
# from Vyas and Kumaranayake 2006 standard procedure is to select components where the associated eigenvalue is greater than 1
# only the first principal component is then used to measure wealth.

library(factoextra)
dev.off()
fviz_pca_var(hhs_pca_binary,

                          col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)     # Avoid text overlapping
fviz_screeplot(hhs_pca_binary)
ggsave(filename = here::here("images", "PCA_viz_binary.png"))

# 4. constructing PCA ######
# if the data have been standardised (e.g. all binary) then use a co-variance matrix for the PCA
# if the data have not been standardised (e.g. some quant data as well as binary), they use the correlation matrix

# construct index of principal components
index_all = hhs_pca_binary$x[,1]
nlab<-c(1,2,3,4,5)

# append the index, and the wealth quintiles from all (with tlu and crop area) onto the full hhs dataframe
hhs_pca_eda_subset_all <- hhs_pca_eda_subset_all %>% 
  mutate(quintiles = as.factor(cut(index_all, breaks=5, labels=nlab))) %>% 
  mutate(wealth_pca = index_all) 

hhs_pca_eda_subset_all <- hhs_pca_eda_subset_all %>% 
  mutate("_index" = row_number())
hhs_wealth <- full_join(hhs2, hhs_pca_eda_subset_all, by = "_index")

saveRDS(hhs_wealth, "hhs_cleaned_wealth.rds")

########################################################################################################################
######## Test the wealth index by comparing to other variables
########################################################################################################################

# compare to payments from conservancies, land elsewhere, other?

# bar chart of the quintiles against the variable of choice
ggplot(hhs_wealth, aes(x=total_now_tlu)) + 
  geom_point(aes(fill = quintiles), position = "fill",width = 0.4) +
  xlab("TLU") +
  ylab("Percentage") +
  ggtitle("Wealth Breakdown with crop and tlu")

# scatterplot with a linear model best fit line
ggplot(hhs_wealth, aes(x=land_size_fct, y=wealth_pca)) +
  geom_point() +
  geom_smooth(method='lm') +
  xlab("crop_acre") +
  ylab("Wealth PCA") +
  ggtitle("perc_child_in_edu vs wealth PCA")
