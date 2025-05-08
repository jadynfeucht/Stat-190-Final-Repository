# Load libraries
library(tidyverse)
library(lubridate)
library(caret)
library(pROC)
library(randomForest)
library(ggplot2)

# Read the data
# This model was created before we got 2024 data set, so it uses 2023 data
data <- read.csv(choose.files(), header=T)
# File name: drake_export_v8_2024-02-13_100754_rev2_nolatlong.csv


str(data)
summary(data)

# First cleaning block
data <- data %>%  
  mutate( 
    ethnicity = ifelse(ethnicity == "" | is.na(ethnicity),  
                       "Not Selected", ethnicity), 
    housing = ifelse(housing == "" | is.na(housing),  
                     "Not Selected", housing), 
    housing_type = ifelse(housing_type == "" | is.na(housing_type),  
                          "Not Selected", housing_type), 
    income_source = ifelse(income_source == "" | is.na(income_source),  
                           "Not Selected", income_source),
    homeless = ifelse(homeless == "" | is.na(homeless),  
                      "Not Selected", homeless), 
    family_type = ifelse(family_type == "" | is.na(family_type),  
                         "Not Selected", family_type), 
    education = ifelse(education == "" | is.na(education),  
                       "Not Selected", education), 
    race = ifelse(race == "" | is.na(race),  
                  "Not Selected", race), 
    gender = ifelse(gender == "" | is.na(gender),  
                    "Not Selected", gender), 
    service_name = ifelse(service_name == "" | is.na(service_name),  
                          "Not Selected", service_name)) 

# Create return indicator -----------
data <- data %>%
  mutate(served_date = as.Date(served_date)) %>%
  arrange(individual_id, served_date) %>%
  group_by(individual_id) %>%
  mutate(visit_count = row_number(),
         will_return = lead(served_date, 1),
         will_return = !is.na(will_return)) %>%
  ungroup()

# Clean Variables and Prep Data -----------
model_data <- data %>%  
  mutate( 
    served_date = ymd(served_date), 
    dob = ymd(dob), 
    zip = as.numeric(zip), 
    served_year = year(served_date), 
    served_month = months(served_date), 
    served_weekday = weekdays(served_date, abbreviate = FALSE), 
    gender = as.factor(gender),
    race = as.factor(race),
    education = as.factor(education),
    income_source = as.factor(income_source),
    age = as.numeric(format(Sys.Date(), "%Y")) - as.numeric(substr(dob, 1, 4)),
    # created this column for easy graphing by month/year 
    MonthYear_date = as.Date(paste( 
      served_year, served_month, "01", sep = "-"),  
      format = "%Y-%b-%d") 
  ) %>%  
  # 2024 has 1.5 months of data - not worth it for us 
  filter(served_year != 2024) %>%
  drop_na(will_return, age, gender, race, education, income_source, annual_income)

# Filter to first visits only 
first_visits <- data %>% filter(visit_count == 1)

# TESTING --------------

# Train/test split
set.seed(123)
split <- createDataPartition(model_data$will_return, p = 0.8, list = FALSE)
train <- model_data[split, ]
test <- model_data[-split, ]

# Train a random forest model because predictor (will_return) is either true/false
# A first time visitor with either return to a pantry or will not

model <- randomForest(will_return ~ age + gender + race + education + income_source + annual_income + snap_household,
                      data = train, ntree = 250)

# Predict and evaluate model
preds <- predict(model, newdata = test)
confusionMatrix(preds, test$will_return)
# Accuracy: 0.7613
# Sensitivity: 0.017087
# Specificity:0.996117


########### VISUALS ###########

# Helps answer: Which features most influence return likelihood?
# Feature Impact: Bar Plot of Variable Importance
# Get importance and convert to data frame
importance_data <- importance(model)
importance_df <- data.frame(Feature = rownames(importance_data), Importance = importance_data[, "MeanDecreaseGini"])

# Plot with ggplot2
ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "#611bb8") +
  coord_flip() +
  labs(
    title = "Feature Importance: Will Return Prediction",
    x = "Feature",
    y = "Mean Decrease in Gini Index"
  ) +
  theme_minimal()


# ROC Curve
# Shows model performance and trade-off between sensitivity/specificity
probabilities <- predict(model, newdata = test, type = "prob")[,2]
roc_obj <- roc(test$will_return, probabilities)
plot(roc_obj, main = "ROC Curve")
auc(roc_obj)

# Distribution of Predicted Probabilities
# Helps visualize how confidently the model predicts return vs no return
ggplot(data.frame(prob = probabilities, actual = test$will_return), 
       aes(x = prob, fill = actual)) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 30) +
  labs(title = "Predicted Probability of Returning", x = "Probability", fill = "Actual Return")


# GENDER
ggplot(first_visits, aes(x = gender, fill = will_return)) +
  scale_fill_brewer(palette = "Paired") +
  geom_bar(position = "fill") +
  labs(title = "Return Rate by Gender", y = "Proportion", x = "Gender") +
  scale_y_continuous(labels = scales::percent)

# RACE
ggplot(first_visits, aes(x = race, fill = will_return)) +
  scale_fill_brewer(palette = "Paired") +
  geom_bar(position = "fill") +
  labs(title = "Return Rate by Race", y = "Proportion", x = "Race") +
  scale_y_continuous(labels = scales::percent)

# Income Source
ggplot(first_visits, aes(x = income_source, fill = will_return)) +
  scale_fill_brewer(palette = "Paired") +
  geom_bar(position = "fill") +
  labs(title = "Return Rate by Income Source", y = "Proportion", x = "Income Source") +
  scale_y_continuous(labels = scales::percent)

# Snap Household?
ggplot(first_visits, aes(x = snap_household, fill = will_return)) +
  scale_fill_brewer(palette = "Paired") +
  geom_bar(position = "fill") +
  labs(title = "Return Rate by Snap Participation", y = "Proportion", x = "Snap Participation", fill = "Will Return") +
  scale_y_continuous(labels = scales::percent)

# Education
first_visits$education <- factor(first_visits$education, 
                                 levels = c("Not Selected", "Unknown", "No Schooling", "Pre-K and younger", 
                                            "K-8 (Currently)", "Low Grade Grad", "9-12 (Currently)", "K-12 Drop Out", 
                                            "HSED/GED Grad (or current)", "HS Grad", "HS Grad / Some College",
                                            "College 2 or 4 yr Degree", "College Advanced Degree"))
ggplot(first_visits, aes(x = education, fill = will_return)) +
  scale_fill_brewer(palette = "Paired") +
  geom_bar(position = "fill") +
  labs(title = "Return Rate by Education Level", y = "Proportion", x = "Education", fill = "Will Return") +
  scale_y_continuous(labels = scales::percent) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Other models that I did not use because the accuracy: ----------

# Model 2 ------

model2 <- randomForest(will_return ~ age + gender + race + education + income_source + annual_income + snap_household,
                       data = train, ntree = 10)
# Predict and evaluate model
preds2 <- predict(model2, newdata = test)
confusionMatrix(preds2, test$will_return)
# Accuracy: 0.7565
# Sensitivity: 0.04605         
# Specificity: 0.98059 

# Model 3 ------

model3 <- randomForest(will_return ~ age + gender + race + education + income_source + annual_income + snap_household,
                       data = train, ntree = 50)
# Predict and evaluate model
preds3 <- predict(model3, newdata = test)
confusionMatrix(preds3, test$will_return)
# Accuracy: 0.7603
# Sensitivity: 0.020417        
# Specificity: 0.993742


# Model 4 ------

model4 <- randomForest(will_return ~ age + gender + race + education + income_source + annual_income + snap_household,
                       data = train, ntree = 150)
# Predict and evaluate model
preds4 <- predict(model4, newdata = test)
confusionMatrix(preds4, test$will_return)
# Accuracy: 0.7609
# Sensitivity: 0.017087
# Specificity:0.995569

# Model 5 ------

model5 <- randomForest(will_return ~ age + gender + race + education + income_source + annual_income + snap_household,
                       data = train, ntree = 350)
# Predict and evaluate model
preds5 <- predict(model5, newdata = test)
confusionMatrix(preds5, test$will_return)
# Accuracy: 0.7613
# Sensitivity: 0.017666
# Specificity:0.995889

# Model 6 ------

model6 <- randomForest(will_return ~ age + gender + race + education + income_source + annual_income + snap_household,
                       data = train, ntree = 500)
# Predict and evaluate model
preds6 <- predict(model6, newdata = test)
confusionMatrix(preds6, test$will_return)
# Accuracy: 0.7612
# Sensitivity: 0.017087
# Specificity:0.995934

# Model 7 ------

model7 <- randomForest(will_return ~ age + gender + race + education + income_source + annual_income + snap_household,
                       data = train, ntree = 175)
# Predict and evaluate model
preds7 <- predict(model7, newdata = test)
confusionMatrix(preds7, test$will_return)
# Accuracy: 0.7613
# Sensitivity: 0.017955
# Specificity:0.995843