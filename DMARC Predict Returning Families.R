rm(list = ls())
source("cleancode.R")
display.brewer.all(colorblindFriendly = TRUE)


dim(all)
head(all)
str(all)

unique(all$served_month)

############################################################################


##### start here #####



# 04/17/2025

# code we'd like to use

#unique_ind_year_all <- all %>% 
#  group_by(individual_id, served_year) %>% 
#  summarise(count_ind = n(), .groups = "drop") %>% 
#  arrange(desc(count_ind))

# make binary columns
# help from chatgpt putting in the unique values 
binary_all <- all %>%
  mutate(
    # served_year 
    year_2018 = ifelse(served_year == "2018", 1, 0),
    year_2019 = ifelse(served_year == "2019", 1, 0),
    year_2020 = ifelse(served_year == "2020", 1, 0),
    year_2021 = ifelse(served_year == "2021", 1, 0),
    year_2022 = ifelse(served_year == "2022", 1, 0),
    year_2023 = ifelse(served_year == "2023", 1, 0),
    
    # ethnicity
    eth_hispanic = ifelse(ethnicity == "Hispanic or Latino", 1, 0),
    eth_nonhispanic = ifelse(ethnicity == "Not Hispanic or Latino", 1, 0),
    eth_unknown = ifelse(ethnicity == "Unknown", 1, 0),
    eth_other = ifelse(ethnicity == "Other", 1, 0),
    
    # gender
    gender_female = ifelse(gender == "Woman (girl)", 1, 0),
    gender_male = ifelse(gender == "Man (boy)", 1, 0),
    gender_other = ifelse(!(gender %in% c("Woman (girl)", "Man (boy)", "Not Selected")), 1, 0),
    
    # education
    edu_9_12 = ifelse(education == "9-12 (Currently)", 1, 0),
    edu_k_8 = ifelse(education == "K-8 (Currently)", 1, 0),
    edu_unknown = ifelse(education == "Unknown", 1, 0),
    edu_dropout = ifelse(education == "K-12 Drop Out", 1, 0),
    edu_hs_grad = ifelse(education == "HS Grad", 1, 0),
    edu_prek = ifelse(education == "Pre-K and younger (Currently)", 1, 0),
    edu_college_degree = ifelse(education == "College 2 or 4 yr  Degree", 1, 0),
    edu_some_college = ifelse(education == "HS Grad / Some College", 1, 0),
    edu_ged = ifelse(education == "HSED / GED Grad (or current)", 1, 0),
    edu_no_schooling = ifelse(education == "No Schooling", 1, 0),
    edu_advanced_degree = ifelse(education == "College Advanced Degree", 1, 0),
    edu_low_grade = ifelse(education == "Low Grade Grad", 1, 0),
    
    # family_type
    fam_adults_with_kids = ifelse(family_type == "Adults with Children", 1, 0),
    fam_other = ifelse(family_type == "Other", 1, 0),
    fam_adults_no_kids = ifelse(family_type == "Adults Without Children", 1, 0),
    fam_female_with_kids = ifelse(family_type == "Female Adult with Children", 1, 0),
    fam_single = ifelse(family_type == "Single Person", 1, 0),
    fam_male_with_kids = ifelse(family_type == "Male Adult with Children", 1, 0),
    
    # snap_household
    snap_yes = ifelse(snap_household == "Y", 1, 0),
    snap_no = ifelse(snap_household == "N", 1, 0),
    
    # housing
    house_renting = ifelse(housing == "Renting", 1, 0),
    house_owning = ifelse(housing == "Own/Buying", 1, 0),
    house_homeless = ifelse(housing == "Homeless", 1, 0),
    house_other = ifelse(housing == "Other", 1, 0),
    
    # housing_type
    housing_apartment = ifelse(housing_type == "Five or More Unit Apartments", 1, 0),
    housing_house = ifelse(housing_type == "House", 1, 0),
    housing_duplex = ifelse(housing_type == "Duplex/Triplex/Quadplex", 1, 0),
    housing_other_type = ifelse(housing_type == "Other", 1, 0),
    housing_mobile = ifelse(housing_type == "Mobile Home", 1, 0),
    housing_room = ifelse(housing_type == "Rent a room", 1, 0),
    
    # homeless
    homeless_stable = ifelse(homeless == "Stably Housed", 1, 0),
    homeless_unstable = ifelse(homeless == "Unstably Housed", 1, 0),
    homeless_literally = ifelse(homeless == "Literally Homeless", 1, 0),
    homeless_imminent = ifelse(homeless == "Imminently Homeless", 1, 0),
    
    # income_source
    income_unemployed = ifelse(income_source == "Unemployed", 1, 0),
    income_caregiver = ifelse(income_source == "Care Giver", 1, 0),
    income_part_time = ifelse(income_source == "Part Time", 1, 0),
    income_retired = ifelse(income_source == "Retired", 1, 0),
    income_full_time = ifelse(income_source == "Full Time", 1, 0),
    income_ss = ifelse(income_source == "Social Security", 1, 0),
    income_other = ifelse(income_source == "Other", 1, 0),
    income_stay_home = ifelse(income_source == "Stay at Home Parent", 1, 0),
    income_student = ifelse(income_source == "Student/ Workforce Training", 1, 0),
    income_pending_disability = ifelse(income_source == "Pending Disability", 1, 0),
    income_disability = ifelse(income_source == "Disability", 1, 0),
    income_child_support = ifelse(income_source == "Child Support", 1, 0),
    income_seasonal = ifelse(income_source == "Seasonal/Temporary", 1, 0),
    income_unemployment_benefits = ifelse(income_source == "Unemployment Benefits", 1, 0),
    income_fip = ifelse(income_source == "FIP (Family Investment Program)", 1, 0) 
  ) %>% 
  group_by(individual_id) %>% 
  summarise(
    year_2018 = max(year_2018),
    year_2019 = max(year_2019),
    year_2020 = max(year_2020),
    year_2021 = max(year_2021),
    year_2022 = max(year_2022),
    year_2023 = max(year_2023),
    .groups = "drop") 
#%>% 
# select(individual_id, starts_with("year_"), starts_with("eth_"), 
#        starts_with("edu_"), starts_with("gender_"), starts_with("fam_"), 
#       starts_with("snap_"), starts_with("house_"), starts_with("housing_"), 
#      starts_with("homeless_"), starts_with("income_"),
#     -income_source, -housing_type, -snap_household)

# heres your columns listed 
# individual_id, afn, year_2018, year_2019, year_2020, year_2021, year_2022, year_2023, eth_hispanic, eth_nonhispanic, eth_unknown, eth_other, edu_9_12, edu_k_8, edu_unknown, edu_dropout, edu_hs_grad, edu_prek, edu_college_degree, edu_some_college, edu_ged, edu_no_schooling, edu_advanced_degree, edu_low_grade, gender_female, gender_male, gender_other, fam_adults_with_kids, fam_other, fam_adults_no_kids, fam_female_with_kids, fam_single, fam_male_with_kids, snap_household, snap_yes, snap_no, house_renting, house_owning, house_homeless, house_other, housing_type, housing_apartment, housing_house, housing_duplex, housing_other_type, housing_mobile, housing_room, homeless_stable, homeless_unstable, homeless_literally, homeless_imminent, income_source, income_unemployed, income_caregiver, income_part_time, income_retired, income_full_time, income_ss, income_other, income_stay_home, income_student, income_pending_disability, income_disability, income_child_support, income_seasonal, income_unemployment_benefits, income_fip


#install.packages("randomForest")
#install.packages("RColorBrewer")  
#install.packages("reshape2")  
#install.packages("tidyverse")  
#install.packages("pROC")  
#install.packages("glmnet")  
#install.packages("lubridate")  
#install.packages("sf")  
#install.packages("tigris")  
#install.packages("leaflet")  
#install.packages("knitr")  
#install.packages("scales")  
#install.packages("selectiveInference")  
# only run this once
library(randomForest)
library(RColorBrewer)
library(reshape2)
library(tidyverse)
library(pROC)
library(glmnet) #fitting lasso, ridge, regressions GLMs)
library(lubridate)
library(sf)
library(tigris)
library(leaflet)
library(knitr)
library(scales)
#library(selectiveInference)


##### Model 8: Predict 2024 attendance 2 #####

# train a model to predict 2023 visits based on 
#   previous years 2018-2023 and apply logic to 
#   2024

# create training data to predict 2023
# group by family attendance in years
features_2023 <- all %>%
  filter(served_year %in% 2018:2022) %>%
  mutate(flag = 1) %>%
  pivot_wider(names_from = served_year, 
              names_prefix = "year_", 
              values_from = flag, 
              values_fill = 0) %>%
  group_by(afn, family_type, race, ethnicity) %>%
  summarise(across(starts_with("year_"), max),
            .groups = "drop")

# Add the label: did they show up in 2023?
label_2023 <- all %>%
  mutate(returned_2023 = ifelse(served_year == 2023, 1, 0)) %>%
  group_by(afn) %>% 
  summarise(returned_2023 = max(returned_2023), .groups = "drop")

# joining tables where 2023 data is added back?
model_data <- features_2023 %>%
  left_join(label_2023, by = "afn") %>%
  mutate(returned_2023 = ifelse(is.na(returned_2023), 0, returned_2023))
head(model_data)

# set training sets
set.seed(123)
train.idx <- sample(nrow(model_data), size = 0.8 * nrow(model_data))
train <- model_data[train.idx, ]
test <- model_data[-train.idx, ]

# run your model using the 2023 data
myforest8 <- randomForest(as.factor(returned_2023) ~ . -afn,
                          data = train,
                          ntree = 1000,
                          mtry = floor(sqrt(ncol(train) - 2)),
                          importance = TRUE)


# calculate accuracy for 2023
# Accuracy Statistics
Predicted = predict(myforest8, newdata = test)
Actual = test$returned_2023

conf_mat <- table(Predicted, Actual) 
# Assign entries for readability
TN <- conf_mat["0", "0"]
FP <- conf_mat["1", "0"]
FN <- conf_mat["0", "1"]
TP <- conf_mat["1", "1"]
# Calculate metrics
accuracy <- (TP + TN) / (TP + TN + FP + FN)
sensitivity <- TP / (TP + FN)
specificity <- TN / (TN + FP)
# Print results
cat("Accuracy:", round(accuracy * 100, 2), "%\n")
cat("Sensitivity:", round(sensitivity * 100, 2), "%\n")
cat("Specificity:", round(specificity * 100, 2), "%\n")

predictions_2023 <- features_2023 %>%
  mutate(returned_2023 = predict(myforest8, newdata = .))

# plot
ggplot(predictions_2023, aes(x = returned_2023)) +
  geom_bar(fill = "#5e2b97ff") +
  geom_text(stat = "count", aes(label = ..count..), 
            vjust = -0.5) +
  labs(title = "Predicted Visitor Return in 2023",
       x = "Will Return (1 = Yes, 0 = No)",
       y = "Number of Visitors") +
  theme_minimal()


# make prediction table for 2024
predictors_for_2024 <- all %>%
  filter(served_year %in% 2018:2023) %>%
  mutate(flag = 1) %>%
  pivot_wider(names_from = served_year, names_prefix = "year_", 
              values_from = flag, values_fill = 0) %>%
  group_by(afn, family_type, race, ethnicity) %>%
  summarise(across(starts_with("year_"), max), .groups = "drop")

# Apply model to predict 2024
predictions_2024 <- predictors_for_2024 %>%
  mutate(predicted_2024 = predict(myforest8, newdata = .))


# graph the returning values 
ggplot(predictions_2024, aes(x = predicted_2024)) +
  geom_bar(fill = "#5e2b97ff") +
  geom_text(stat = "count", aes(label = ..count..), 
            vjust = -0.5) +
  labs(title = "Predicted Visitor Return in 2024",
       x = "Will Return (1 = Yes, 0 = No)",
       y = "Number of Visitors") +
  theme_minimal()

# break out by family_type
ggplot(predictions_2024, 
       aes(x = predicted_2024, 
           fill = family_type)) +
  geom_bar() +
  labs(title = "Predicted Visitor Return in 2024 by Family Type",
       x = "Will Return (1 = Yes, 0 = No)",
       y = "Number of Visitors") +
  theme_minimal()

# hone in on returners 
returners_2024 <- predictions_2024 %>%
  filter(predicted_2024 == 1)
# Plot breakdown of family_type
ggplot(returners_2024, 
       aes(x = fct_infreq(family_type))) +
  geom_bar(fill = "#5e2b97ff") +
  geom_text(stat = "count", aes(label = ..count..), 
            vjust = -0.5) +
  labs(title = "2024 Predicted Returners by Family Type",
       x = "Family Type",
       y = "Number of Returners") +
  theme_minimal() +
  theme(axis.text.x = 
          element_text(angle = 45, hjust = 1))

return_rates_family_type <- predictions_2024 %>%
  group_by(family_type) %>%
  summarise(
    total = n(),
    returning = sum(predicted_2024 == 1),
    return_rate = round(returning / total, 3)
  ) %>% 
  arrange(desc(return_rate))



return_rates_race <- predictions_2024 %>%
  group_by(race) %>%
  summarise(
    total = n(),
    returning = sum(predicted_2024 == 1),
    return_rate = round(returning / total, 3)
  ) %>% 
  arrange(desc(return_rate))

# rate_1 is % more likely to return in 2024 than rate_2
rate_1 = 0.396
rate_2 = 0.352
percent_diff <- (rate_1 - rate_2) / rate_2 * 100
percent_diff
# Adults with Children have a 39.6% chance of returning to the 
#   pantry in 2024
# Female Adults with Children have a 35.5% chance of returning 
#   to the pantry in 2024.
# Adults with Children are 11.5% more likely to return to 
#   the pantry in 2024 than Female Adults with children


library(gridExtra)
plot.new()
grid.table(return_rates_family_type)

importance(myforest8)
varImpPlot(myforest8, type = 1)

vi <- as.data.frame(varImpPlot(myforest8, type = 1))
vi$Variable <- rownames(vi)
ggplot(data = vi) +
  geom_bar(aes(x = Variable, weight = MeanDecreaseAccuracy),
           position = "identity") +
  coord_flip() +
  labs(x = "Variable Name", y = "Mean Decrease Accuracy")



write.csv(predictions_2024, 
          "~/STAT 190/predictions_2024s.csv",
          row.names = TRUE)

# hone in on returners with children
returners_2024_children_1 <- predictions_2024 %>%
  filter(predicted_2024 == 1) %>% 
  mutate(family_type = ifelse(grepl("with Children", 
                                    family_type, 
                                    ignore.case = TRUE),
                              "Families with Children", 
                              family_type))
# Plot breakdown of family_type
ggplot(returners_2024_children_1, 
       aes(x = fct_infreq(family_type))) +
  geom_bar(fill = "#5e2b97ff") +
  geom_text(stat = "count", aes(label = ..count..), 
            vjust = -0.5) +
  labs(title = "2024 Predicted Returners by Family Type",
       x = "Family Type",
       y = "Number of Returners") +
  theme_minimal() +
  theme(axis.text.x = 
          element_text(angle = 45, hjust = 1))

# hone in on returners with children
returners_2024_children <- predictions_2024 %>%
  mutate(family_type = ifelse(grepl("with Children", 
                                    family_type, 
                                    ignore.case = TRUE),
                              "Families with Children", 
                              family_type))

return_rates_children <- returners_2024_children %>%
  group_by(family_type) %>%
  summarise(
    total = n(),
    returning = sum(predicted_2024 == 1),
    return_rate = round(returning / total, 3)
  ) %>% 
  arrange(desc(return_rate))

plot.new()
grid.table(return_rates_children)


# break out by family_type
ggplot(predictions_2024, 
       aes(x = year_2023)) +
  geom_text(stat = "count", aes(label = ..count..), 
            vjust = -0.5) +
  geom_bar() +
  labs(title = "Predicted Visitor Return in 2024 by Family Type",
       x = "Will Return (1 = Yes, 0 = No)",
       y = "Number of Visitors") +
  theme_minimal()

























#########   ######### 
######### ARCHIVED MODELS ######### 
##### start here #####



# 04/17/2025

# code we'd like to use

#unique_ind_year_all <- all %>% 
#  group_by(individual_id, served_year) %>% 
#  summarise(count_ind = n(), .groups = "drop") %>% 
#  arrange(desc(count_ind))

# make binary columns
# help from chatgpt putting in the unique values 
binary_all <- all %>%
  mutate(
    # served_year 
    year_2018 = ifelse(served_year == "2018", 1, 0),
    year_2019 = ifelse(served_year == "2019", 1, 0),
    year_2020 = ifelse(served_year == "2020", 1, 0),
    year_2021 = ifelse(served_year == "2021", 1, 0),
    year_2022 = ifelse(served_year == "2022", 1, 0),
    year_2023 = ifelse(served_year == "2023", 1, 0),
    
    # ethnicity
    eth_hispanic = ifelse(ethnicity == "Hispanic or Latino", 1, 0),
    eth_nonhispanic = ifelse(ethnicity == "Not Hispanic or Latino", 1, 0),
    eth_unknown = ifelse(ethnicity == "Unknown", 1, 0),
    eth_other = ifelse(ethnicity == "Other", 1, 0),
    
    # gender
    gender_female = ifelse(gender == "Woman (girl)", 1, 0),
    gender_male = ifelse(gender == "Man (boy)", 1, 0),
    gender_other = ifelse(!(gender %in% c("Woman (girl)", "Man (boy)", "Not Selected")), 1, 0),
    
    # education
    edu_9_12 = ifelse(education == "9-12 (Currently)", 1, 0),
    edu_k_8 = ifelse(education == "K-8 (Currently)", 1, 0),
    edu_unknown = ifelse(education == "Unknown", 1, 0),
    edu_dropout = ifelse(education == "K-12 Drop Out", 1, 0),
    edu_hs_grad = ifelse(education == "HS Grad", 1, 0),
    edu_prek = ifelse(education == "Pre-K and younger (Currently)", 1, 0),
    edu_college_degree = ifelse(education == "College 2 or 4 yr  Degree", 1, 0),
    edu_some_college = ifelse(education == "HS Grad / Some College", 1, 0),
    edu_ged = ifelse(education == "HSED / GED Grad (or current)", 1, 0),
    edu_no_schooling = ifelse(education == "No Schooling", 1, 0),
    edu_advanced_degree = ifelse(education == "College Advanced Degree", 1, 0),
    edu_low_grade = ifelse(education == "Low Grade Grad", 1, 0),
    
    # family_type
    fam_adults_with_kids = ifelse(family_type == "Adults with Children", 1, 0),
    fam_other = ifelse(family_type == "Other", 1, 0),
    fam_adults_no_kids = ifelse(family_type == "Adults Without Children", 1, 0),
    fam_female_with_kids = ifelse(family_type == "Female Adult with Children", 1, 0),
    fam_single = ifelse(family_type == "Single Person", 1, 0),
    fam_male_with_kids = ifelse(family_type == "Male Adult with Children", 1, 0),
    
    # snap_household
    snap_yes = ifelse(snap_household == "Y", 1, 0),
    snap_no = ifelse(snap_household == "N", 1, 0),
    
    # housing
    house_renting = ifelse(housing == "Renting", 1, 0),
    house_owning = ifelse(housing == "Own/Buying", 1, 0),
    house_homeless = ifelse(housing == "Homeless", 1, 0),
    house_other = ifelse(housing == "Other", 1, 0),
    
    # housing_type
    housing_apartment = ifelse(housing_type == "Five or More Unit Apartments", 1, 0),
    housing_house = ifelse(housing_type == "House", 1, 0),
    housing_duplex = ifelse(housing_type == "Duplex/Triplex/Quadplex", 1, 0),
    housing_other_type = ifelse(housing_type == "Other", 1, 0),
    housing_mobile = ifelse(housing_type == "Mobile Home", 1, 0),
    housing_room = ifelse(housing_type == "Rent a room", 1, 0),
    
    # homeless
    homeless_stable = ifelse(homeless == "Stably Housed", 1, 0),
    homeless_unstable = ifelse(homeless == "Unstably Housed", 1, 0),
    homeless_literally = ifelse(homeless == "Literally Homeless", 1, 0),
    homeless_imminent = ifelse(homeless == "Imminently Homeless", 1, 0),
    
    # income_source
    income_unemployed = ifelse(income_source == "Unemployed", 1, 0),
    income_caregiver = ifelse(income_source == "Care Giver", 1, 0),
    income_part_time = ifelse(income_source == "Part Time", 1, 0),
    income_retired = ifelse(income_source == "Retired", 1, 0),
    income_full_time = ifelse(income_source == "Full Time", 1, 0),
    income_ss = ifelse(income_source == "Social Security", 1, 0),
    income_other = ifelse(income_source == "Other", 1, 0),
    income_stay_home = ifelse(income_source == "Stay at Home Parent", 1, 0),
    income_student = ifelse(income_source == "Student/ Workforce Training", 1, 0),
    income_pending_disability = ifelse(income_source == "Pending Disability", 1, 0),
    income_disability = ifelse(income_source == "Disability", 1, 0),
    income_child_support = ifelse(income_source == "Child Support", 1, 0),
    income_seasonal = ifelse(income_source == "Seasonal/Temporary", 1, 0),
    income_unemployment_benefits = ifelse(income_source == "Unemployment Benefits", 1, 0),
    income_fip = ifelse(income_source == "FIP (Family Investment Program)", 1, 0) 
  ) %>% 
  group_by(individual_id) %>% 
  summarise(
    year_2018 = max(year_2018),
    year_2019 = max(year_2019),
    year_2020 = max(year_2020),
    year_2021 = max(year_2021),
    year_2022 = max(year_2022),
    year_2023 = max(year_2023),
    .groups = "drop") 
#%>% 
# select(individual_id, starts_with("year_"), starts_with("eth_"), 
#        starts_with("edu_"), starts_with("gender_"), starts_with("fam_"), 
#       starts_with("snap_"), starts_with("house_"), starts_with("housing_"), 
#      starts_with("homeless_"), starts_with("income_"),
#     -income_source, -housing_type, -snap_household)

# heres your columns listed 
# individual_id, afn, year_2018, year_2019, year_2020, year_2021, year_2022, year_2023, eth_hispanic, eth_nonhispanic, eth_unknown, eth_other, edu_9_12, edu_k_8, edu_unknown, edu_dropout, edu_hs_grad, edu_prek, edu_college_degree, edu_some_college, edu_ged, edu_no_schooling, edu_advanced_degree, edu_low_grade, gender_female, gender_male, gender_other, fam_adults_with_kids, fam_other, fam_adults_no_kids, fam_female_with_kids, fam_single, fam_male_with_kids, snap_household, snap_yes, snap_no, house_renting, house_owning, house_homeless, house_other, housing_type, housing_apartment, housing_house, housing_duplex, housing_other_type, housing_mobile, housing_room, homeless_stable, homeless_unstable, homeless_literally, homeless_imminent, income_source, income_unemployed, income_caregiver, income_part_time, income_retired, income_full_time, income_ss, income_other, income_stay_home, income_student, income_pending_disability, income_disability, income_child_support, income_seasonal, income_unemployment_benefits, income_fip


#install.packages("randomForest")
#install.packages("RColorBrewer")  
#install.packages("reshape2")  
#install.packages("tidyverse")  
#install.packages("pROC")  
#install.packages("glmnet")  
#install.packages("lubridate")  
#install.packages("sf")  
#install.packages("tigris")  
#install.packages("leaflet")  
#install.packages("knitr")  
#install.packages("scales")  
#install.packages("selectiveInference")  
# only run this once
library(randomForest)
library(RColorBrewer)
library(reshape2)
library(tidyverse)
library(pROC)
library(glmnet) #fitting lasso, ridge, regressions GLMs)
library(lubridate)
library(sf)
library(tigris)
library(leaflet)
library(knitr)
library(scales)
#library(selectiveInference)


##### Model 1: Random Forest on Individual Level #####

# INDIVIDUAL LEVEL
# run this code 
binary_all_indv <- all %>%
  mutate(
    # served_year 
    year_2018 = ifelse(served_year == "2018", 1, 0),
    year_2019 = ifelse(served_year == "2019", 1, 0),
    year_2020 = ifelse(served_year == "2020", 1, 0),
    year_2021 = ifelse(served_year == "2021", 1, 0),
    year_2022 = ifelse(served_year == "2022", 1, 0),
    year_2023 = ifelse(served_year == "2023", 1, 0)
  ) %>% 
  group_by(individual_id) %>% 
  summarise(
    year_2018 = max(year_2018),
    year_2019 = max(year_2019),
    year_2020 = max(year_2020),
    year_2021 = max(year_2021),
    year_2022 = max(year_2022),
    year_2023 = max(year_2023),
    .groups = "drop") 


# create models 

binary_all_indv <- binary_all_indv %>%
  mutate(predict_2023 = as.factor(year_2023)) %>% 
  filter((year_2018 + year_2019 + year_2020 + 
            year_2021 + year_2022 + year_2023) > 0) %>% 
  select(individual_id, predict_2023, starts_with("year_"))

dim(binary_all_indv)
(n_distinct(binary_all_indv$individual_id))

# can we use 2023 to predict and train for that? Then change to predict 2024 
# can i then predict further to 2025

# randomForest 
bin_indv_data_for_model <- binary_all_indv %>% select(-c(individual_id, year_2023))
RNGkind(sample.kind = "default")
set.seed(172172)
train.idx<- sample(x = 1:nrow(bin_indv_data_for_model), 
                   size =.8*nrow(bin_indv_data_for_model))
train.df <- bin_indv_data_for_model[train.idx,]
test.df <- bin_indv_data_for_model[-train.idx,]

myforest1 <- 
  randomForest(predict_2023 ~ .
               , data =train.df
               , ntree = 200
               , importance = TRUE)

# Predict
# Confusion matrix
table(Predicted = predict(myforest1, newdata = test.df), 
      Actual = test.df$predict_2023)

# Accuracy 0.761
# 9163	1375
# 3340	5881


##### Model 2: Random Forest on Family (afn) Level #####

# FAMILY LEVEL
# run this code 
binary_all_AFN <- all %>%
  mutate(
    # served_year 
    year_2018 = ifelse(served_year == "2018", 1, 0),
    year_2019 = ifelse(served_year == "2019", 1, 0),
    year_2020 = ifelse(served_year == "2020", 1, 0),
    year_2021 = ifelse(served_year == "2021", 1, 0),
    year_2022 = ifelse(served_year == "2022", 1, 0),
    year_2023 = ifelse(served_year == "2023", 1, 0)
  ) %>% 
  group_by(afn) %>% 
  summarise(
    year_2018 = max(year_2018),
    year_2019 = max(year_2019),
    year_2020 = max(year_2020),
    year_2021 = max(year_2021),
    year_2022 = max(year_2022),
    year_2023 = max(year_2023),
    .groups = "drop") 


# create models 

binary_all_AFN <- binary_all_AFN %>%
  mutate(predict_2023 = as.factor(year_2023)) %>% 
  filter((year_2018 + year_2019 + year_2020 + 
            year_2021 + year_2022 + year_2023) > 0) %>% 
  select(afn, predict_2023, starts_with("year_"))

dim(binary_all_AFN)
(n_distinct(binary_all_AFN$afn))

# can we use 2023 to predict and train for that? Then change to predict 2024 
# can i then predict further to 2025

# randomForest 
bin_afn_data_for_model <- binary_all_AFN %>% select(-c(afn, year_2023))
RNGkind(sample.kind = "default")
set.seed(172172)
train.idx<- sample(x = 1:nrow(bin_afn_data_for_model), 
                   size =.8*nrow(bin_afn_data_for_model))
train.df <- bin_afn_data_for_model[train.idx,]
test.df <- bin_afn_data_for_model[-train.idx,]

myforest2 <- 
  randomForest(predict_2023 ~ .
               , data =train.df
               , ntree = 200
               , importance = TRUE)

# Predict
# Confusion matrix
table(Predicted = predict(myforest2, newdata = test.df), 
      Actual = test.df$predict_2023)

# Accuracy 0.791
# 4721	1191
#  900	3185

##### Model 3: Random Forest on Individual & Family Level #####

# INDIVIDUAL AND FAMILY LEVEL
# run this code 
binary_all_INDV_AFN <- all %>%
  mutate(
    # served_year 
    year_2018 = ifelse(served_year == "2018", 1, 0),
    year_2019 = ifelse(served_year == "2019", 1, 0),
    year_2020 = ifelse(served_year == "2020", 1, 0),
    year_2021 = ifelse(served_year == "2021", 1, 0),
    year_2022 = ifelse(served_year == "2022", 1, 0),
    year_2023 = ifelse(served_year == "2023", 1, 0)
  ) %>% 
  group_by(individual_id, afn) %>% 
  summarise(
    year_2018 = max(year_2018),
    year_2019 = max(year_2019),
    year_2020 = max(year_2020),
    year_2021 = max(year_2021),
    year_2022 = max(year_2022),
    year_2023 = max(year_2023),
    .groups = "drop") 


# create models 

binary_all_INDV_AFN <- binary_all_INDV_AFN %>%
  mutate(predict_2023 = as.factor(year_2023)) %>% 
  filter((year_2018 + year_2019 + year_2020 + 
            year_2021 + year_2022 + year_2023) > 0) %>% 
  select(individual_id, afn, predict_2023, starts_with("year_"))

dim(binary_all_INDV_AFN)
(n_distinct(binary_all_INDV_AFN$afn))
(n_distinct(binary_all_INDV_AFN$individual_id))

# can we use 2023 to predict and train for that? Then change to predict 2024 
# can i then predict further to 2025

# randomForest 
bin_indv_afn_data_for_model <- binary_all_INDV_AFN %>% 
  select(-c(individual_id, afn, year_2023))
RNGkind(sample.kind = "default")
set.seed(172172)
train.idx<- sample(x = 1:nrow(bin_indv_afn_data_for_model), 
                   size =.8*nrow(bin_indv_afn_data_for_model))
train.df <- bin_indv_afn_data_for_model[train.idx,]
test.df <- bin_indv_afn_data_for_model[-train.idx,]

myforest3 <- 
  randomForest(predict_2023 ~ .
               , data =train.df
               , ntree = 200
               , importance = TRUE)

# Predict
# Confusion matrix
table(Predicted = predict(myforest3, newdata = test.df), 
      Actual = test.df$predict_2023)

# Accuracy 0.800 (0.79852)
# 9162	1436
# 3206	9235



##### Model 4: Random Forest on Individual & Family Level + Family Type #####

# INDIVIDUAL AND FAMILY LEVEL
# run this code 
binary_INDV_AFN_famtype <- all %>%
  mutate(
    # served_year 
    year_2018 = ifelse(served_year == "2018", 1, 0),
    year_2019 = ifelse(served_year == "2019", 1, 0),
    year_2020 = ifelse(served_year == "2020", 1, 0),
    year_2021 = ifelse(served_year == "2021", 1, 0),
    year_2022 = ifelse(served_year == "2022", 1, 0),
    year_2023 = ifelse(served_year == "2023", 1, 0)
  ) %>% 
  group_by(individual_id, afn, family_type) %>% 
  summarise(
    year_2018 = max(year_2018),
    year_2019 = max(year_2019),
    year_2020 = max(year_2020),
    year_2021 = max(year_2021),
    year_2022 = max(year_2022),
    year_2023 = max(year_2023),
    .groups = "drop") 


# create models 

binary_INDV_AFN_famtype <- binary_INDV_AFN_famtype %>%
  mutate(predict_2023 = as.factor(year_2023)) %>% 
  filter((year_2018 + year_2019 + year_2020 + 
            year_2021 + year_2022 + year_2023) > 0) %>% 
  select(individual_id, afn, family_type, predict_2023, 
         starts_with("year_"))

dim(binary_INDV_AFN_famtype)
(n_distinct(binary_INDV_AFN_famtype$afn))
(n_distinct(binary_INDV_AFN_famtype$individual_id))

# can we use 2023 to predict and train for that? Then change to predict 2024 
# can i then predict further to 2025

# randomForest 
bin_indv_afn_famtype_data_for_model <- 
  binary_INDV_AFN_famtype %>% 
  select(-c(individual_id, afn, year_2023))
RNGkind(sample.kind = "default")
set.seed(172172)
train.idx<- sample(x = 1:nrow(bin_indv_afn_famtype_data_for_model), 
                   size =.8*nrow(bin_indv_afn_famtype_data_for_model))
train.df <- bin_indv_afn_famtype_data_for_model[train.idx,]
test.df <- bin_indv_afn_famtype_data_for_model[-train.idx,]

myforest4 <- 
  randomForest(predict_2023 ~ .
               , data =train.df
               , ntree = 200
               , importance = TRUE)

# Predict
# Confusion matrix
table(Predicted = predict(myforest4, newdata = test.df), 
      Actual = test.df$predict_2023)

# Accuracy 0.808 (0.80775)
# 12266	1912
# 3240	9380


##### Model 5: Random Forest on Individual & Family Level + all variables #####

# INDIVIDUAL AND FAMILY LEVEL
# run this code 
binary_all_var <- all %>%
  mutate(
    # served_year 
    year_2018 = ifelse(served_year == "2018", 1, 0),
    year_2019 = ifelse(served_year == "2019", 1, 0),
    year_2020 = ifelse(served_year == "2020", 1, 0),
    year_2021 = ifelse(served_year == "2021", 1, 0),
    year_2022 = ifelse(served_year == "2022", 1, 0),
    year_2023 = ifelse(served_year == "2023", 1, 0)
  ) %>% 
  group_by(individual_id, afn, family_type, race, ethnicity
  ) %>% 
  summarise(
    year_2018 = max(year_2018),
    year_2019 = max(year_2019),
    year_2020 = max(year_2020),
    year_2021 = max(year_2021),
    year_2022 = max(year_2022),
    year_2023 = max(year_2023),
    .groups = "drop") 


# create models 

binary_all_var <- binary_all_var %>%
  mutate(predict_2023 = year_2023) %>% 
  filter((year_2018 + year_2019 + year_2020 + 
            year_2021 + year_2022 + year_2023) > 0) %>% 
  select(individual_id, afn, family_type, race, ethnicity,
         predict_2023, starts_with("year_"))

#dim(binary_all_var)
#(n_distinct(binary_all_var$afn))
#(n_distinct(binary_all_var$individual_id))

# can we use 2023 to predict and train for that? Then change to predict 2024 
# can i then predict further to 2025

bin_all_data_for_model <- binary_all_var %>% 
  select(-c(individual_id, afn, year_2023))
# randomForest 
RNGkind(sample.kind = "default")
set.seed(172172)
train.idx<- sample(x = 1:nrow(bin_all_data_for_model), 
                   size =.8*nrow(bin_all_data_for_model))
train.df <- bin_all_data_for_model[train.idx,]
test.df <- bin_all_data_for_model[-train.idx,]

myforest5 <- 
  randomForest(as.factor(predict_2023) ~ .
               , data = train.df
               , ntree = 200
               , mtry = floor(sqrt(ncol(train.df) - 2))
               , importance = TRUE)

# Predict 
Predicted = predict(myforest5, newdata = test.df)
Actual = test.df$predict_2023
# Confusion matrix 
table(Predicted, Actual) 
# Accuracy (0.809) 
# 12311    1916
#  3195    9376
# Total	                         26798
# Specificity - true negatives	 0.865 
# Sensitivity - true positives	 0.746 
# Accuracy                       0.809 

# give me a table with the predictor variables and the returned predicted 
#   value column, predict_2023
binary_all_var_output <- binary_all_var
test_original <- binary_all_var_output[-train.idx, ]
test_with_predictions <- test_original %>%
  mutate(predict_2023 = Predicted)

write.csv(test_with_predictions, 
          "~/STAT 190/test_with_predictions.csv",
          row.names = TRUE)

##### Model 6: Random Forest on Family Level + all variables #####

# FAMILY LEVEL
# run this code 
binary_all_fam_var <- all %>%
  mutate(
    # served_year 
    year_2018 = ifelse(served_year == "2018", 1, 0),
    year_2019 = ifelse(served_year == "2019", 1, 0),
    year_2020 = ifelse(served_year == "2020", 1, 0),
    year_2021 = ifelse(served_year == "2021", 1, 0),
    year_2022 = ifelse(served_year == "2022", 1, 0),
    year_2023 = ifelse(served_year == "2023", 1, 0)
  ) %>% 
  group_by(afn, family_type, race, ethnicity
  ) %>% 
  summarise(
    year_2018 = max(year_2018),
    year_2019 = max(year_2019),
    year_2020 = max(year_2020),
    year_2021 = max(year_2021),
    year_2022 = max(year_2022),
    year_2023 = max(year_2023),
    .groups = "drop") 


# create models 

binary_all_fam_var <- binary_all_fam_var %>%
  mutate(predict_2023 = year_2023) %>% 
  filter((year_2018 + year_2019 + year_2020 + 
            year_2021 + year_2022 + year_2023) > 0) %>% 
  select(afn, family_type, race, ethnicity,
         predict_2023, starts_with("year_"))

#dim(binary_all_fam_var)
#(n_distinct(binary_all_fam_var$afn))

# can we use 2023 to predict and train for that? Then change to predict 2024 
# can i then predict further to 2025

bin_all_fam_data_for_model <- binary_all_fam_var %>% 
  select(-c(afn, year_2023))
# randomForest 
RNGkind(sample.kind = "default")
set.seed(172172)
train.idx<- sample(x = 1:nrow(bin_all_fam_data_for_model), 
                   size =.8*nrow(bin_all_fam_data_for_model))
train.df <- bin_all_fam_data_for_model[train.idx,]
test.df <- bin_all_fam_data_for_model[-train.idx,]

myforest6 <- 
  randomForest(as.factor(predict_2023) ~ .
               , data = train.df
               , ntree = 200
               , mtry = floor(sqrt(ncol(train.df) - 2))
               , importance = TRUE)

# Accuracy Statistics
Predicted = predict(myforest6, newdata = test.df)
Actual = test.df$predict_2023

conf_mat <- table(Predicted, Actual) 
#          Actual
# Predicted    0    1
#         0 7234 1178
#         1 1405 4308
# Assign entries for readability
TN <- conf_mat["0", "0"]
FP <- conf_mat["1", "0"]
FN <- conf_mat["0", "1"]
TP <- conf_mat["1", "1"]
# Calculate metrics
accuracy <- (TP + TN) / (TP + TN + FP + FN)
sensitivity <- TP / (TP + FN)
specificity <- TN / (TN + FP)
# Print results
cat("Accuracy:", round(accuracy * 100, 2), "%\n")
cat("Sensitivity:", round(sensitivity * 100, 2), "%\n")
cat("Specificity:", round(specificity * 100, 2), "%\n")
# Accuracy: 81.71 %
# Sensitivity: 78.53 %
# Specificity: 83.74 %



# give me a table with the predictor variables and the returned predicted 
#   value column, predict_2023
binary_all_fam_var_output <- binary_all_fam_var
test_original <- binary_all_fam_var_output[-train.idx, ]
test_with_predictions <- test_original %>%
  mutate(predict_2023 = Predicted)

write.csv(test_with_predictions, 
          "~/STAT 190/test_with_predictions.csv",
          row.names = TRUE)



##### Model Decision #####
## answer the following questions 

## Why did you pick the model you did? 
# using Model 6 - highest accuracy (0.817)
# Random Forest 
#   large data set, discrete outcome of yes/no expected
# predictors: afn, family_type, race, ethnicity



##### Model 7: Predict 2024 attendance #####

predict_2024_data <- all %>%
  mutate(
    year_2018 = ifelse(served_year == "2018", 1, 0),
    year_2019 = ifelse(served_year == "2019", 1, 0),
    year_2020 = ifelse(served_year == "2020", 1, 0),
    year_2021 = ifelse(served_year == "2021", 1, 0),
    year_2022 = ifelse(served_year == "2022", 1, 0),
    year_2023 = ifelse(served_year == "2023", 1, 0)
  ) %>% 
  group_by(afn, family_type, race, ethnicity) %>%
  summarise(
    year_2018 = max(year_2018),
    year_2019 = max(year_2019),
    year_2020 = max(year_2020),
    year_2021 = max(year_2021),
    year_2022 = max(year_2022),
    year_2023 = max(year_2023),
    .groups = "drop"
  ) %>%
  select(afn, family_type, race, ethnicity, starts_with("year_")) %>%
  filter((year_2018 + year_2019 + year_2020 + 
            year_2021 + year_2022 + year_2023) > 0)

predict_2024_data_for_model <- predict_2024_data %>% 
  mutate(predict_2024 = as.factor(year_2018)) %>%  #dummy values
  select(-c(afn))

# randomForest 
RNGkind(sample.kind = "default")
set.seed(172172)
train.idx<- sample(x = 1:nrow(predict_2024_data_for_model), 
                   size =.8*nrow(predict_2024_data_for_model))
train.df <- predict_2024_data_for_model[train.idx,]
test.df <- predict_2024_data_for_model[-train.idx,]

myforest7_2024 <- 
  randomForest(as.factor(predict_2024) ~ .
               , data = train.df
               , ntree = 1000
               , mtry = floor(sqrt(ncol(train.df) - 2))
               , importance = TRUE)

pi_hat <- predict(myforest7_2024, test.df, 
                  type="prob")[,1]
rocCurve <- roc(response = test.df$predict_2024,
                predictor = pi_hat,
                levels = c(0,1))
plot(rocCurve, print.thres=TRUE, print.auc=TRUE)



# give me a table with the predictor variables and the returned predicted 
#   value column, predict_2024
predict_2024_data_output <- predict_2024_data
test_original <- predict_2024_data_output[-train.idx, ]
test_with_predictions <- test_original %>%
  mutate(predict_2024 = Predicted)

write.csv(test_with_predictions, 
          "~/STAT 190/test_with_predictions.csv",
          row.names = TRUE)



