source("Stats190 Clean Data.R")

#Graphing visits for historical data (2018-2023)
# Aggregate visitors by month
monthly_visitors <- all %>%
  group_by(served_year, served_month) %>%
  summarise(total_visitors = n(), .groups = "drop")
head(monthly_visitors)
str(monthly_visitors)

# Convert served_month to a factor to ensure order
monthly_visitors$served_month <- factor(monthly_visitors$served_month, levels = month_order)

ggplot(monthly_visitors, aes(x = served_month, y = total_visitors, group = served_year, color = as.factor(served_year))) +
  geom_line() +
  labs(title = "Monthly Visitors Over the Years",
       x = "Month", y = "Number of Visitors", color = "Year") +
  theme_minimal()
###########################################################################
#Messing around with models for predicting 2024
###########################################################################
#DON'T USE THIS MODEL
# Create a date column for time-series analysis
monthly_visitors <- monthly_visitors %>%
  mutate(month_index = row_number())  # Assigning a sequential index for time-series

# Fit a linear regression model
#model <- lm(total_visitors ~ month_index, data = monthly_visitors)
model <- lm(total_visitors ~ month_index + factor(served_month), data = monthly_visitors)

month_order <- c("January", "February", "March", "April", "May", "June",  
                 "July", "August", "September", "October", "November", "December")
# Create future_months for 12 months of 2024
future_months <- data.frame(
  month_index = seq(max(monthly_visitors$month_index) + 1, 
                    max(monthly_visitors$month_index) + 12),
  served_month = factor(month_order, levels = month_order)
)

# Predict for 2024 (assume new month indices for 2024)
# Add served_month to future_months
future_months <- future_months %>%
  mutate(served_month = factor(month_order, levels = month_order))  # Assign months in order
# Predict for 2024
predictions <- predict(model, newdata = future_months)

# Add predictions to the future_months data frame
future_months$predicted_visitors <- predictions
future_months

# Combine historical and predicted data
future_months <- future_months %>%
  mutate(served_year = 2024,  # Assuming predictions are for 2024
         served_month = month_order)  # Add month names to predictions

combined_data <- bind_rows(
  monthly_visitors %>% mutate(type = "Actual"),
  future_months %>% rename(total_visitors = predicted_visitors) %>% mutate(type = "Predicted")
)

# Plot the data
ggplot(combined_data, aes(x = served_month, y = total_visitors, group = served_year, color = type)) +
  geom_line(aes(linetype = type)) +  # Different line type for predictions
  facet_wrap(~served_year, scales = "free_x") +  # Separate panels for each year
  labs(title = "Monthly Visitors with 2024 Predictions",
       x = "Month", y = "Number of Visitors", color = "Data Type", linetype = "Data Type") +
  theme_minimal() +
  scale_color_manual(values = c("Past Years" = "blue", "Predicted for 2024" = "red"))

# Add predicted data to the existing dataset
future_months <- future_months %>%
  mutate(served_year = 2024,  # Year for predictions
         served_month = month_order)  # Add month names to predictions

# Combine historical and predicted data
combined_data <- bind_rows(
  monthly_visitors,
  future_months %>% rename(total_visitors = predicted_visitors)
)

# Plot the historical data and predictions together
ggplot(combined_data, aes(x = served_month, y = total_visitors, group = served_year, color = as.factor(served_year))) +
  geom_line() +
  geom_point(data = future_months, aes(x = served_month, y = predicted_visitors), color = "red", size = 2) +  # Highlight predictions
  labs(title = "Monthly Visitors with 2024 Predictions",
       x = "Month", y = "Number of Visitors", color = "Year") +
  theme_minimal() +
  scale_color_manual(values = c("2024" = "red", "Other Years" = "blue"))

# Add served_month to future_months
future_months <- future_months %>%
  mutate(served_month = factor(month_order, levels = month_order))  # Assign months in order

# Predict for 2024
predictions <- predict(model, newdata = future_months)

# Add predictions to the future_months data frame
future_months$predicted_visitors <- predictions

# Calculate the total number of predicted visitors for 2024
total_predicted_visitors_2024 <- sum(future_months$predicted_visitors)
total_predicted_visitors_2024
#164805

#########################################################################
#NEW MODEL, MORE ACCURATE
# Load necessary libraries
library(dplyr)
library(ggplot2)

# Aggregate visitors by month
monthly_visitors <- all %>%
  group_by(served_year, served_month) %>%
  summarise(total_visitors = n(), .groups = "drop")

# Convert served_month to a factor to ensure order
monthly_visitors$served_month <- factor(monthly_visitors$served_month, levels = month_order)

# Assign weights to each year, prioritizing 2023
monthly_visitors <- monthly_visitors %>%
  mutate(weight = case_when(
    served_year == 2023 ~ 10,  # Higher weight for 2023
    served_year == 2022 ~ 1,# Medium weight for 2022
    served_year == 2021 ~ 0,
    served_year == 2020 ~ 0,
    served_year == 2019 ~ 10,
    served_year == 2018 ~ 1
      
   #TRUE ~ 1                 # Lower weight for older years
  ))

# Create a date column for time-series analysis
monthly_visitors <- monthly_visitors %>%
  mutate(month_index = row_number())  # Assigning a sequential index for time-series

# Fit a weighted linear regression model
model <- lm(total_visitors ~ month_index + factor(served_month), 
            data = monthly_visitors, 
            weights = weight)

# Prepare future_months data frame for 2024 prediction
future_months <- data.frame(
  month_index = seq(max(monthly_visitors$month_index) + 1, 
                    max(monthly_visitors$month_index) + 12),
  served_month = factor(month_order, levels = month_order)  # Assign months in order
)

# Predict for 2024 using the weighted model
predictions <- predict(model, newdata = future_months)

# Add predictions to the future_months data frame
future_months <- future_months %>%
  mutate(predicted_visitors = predictions,
         served_year = 2024)  # Assign year for predictions

# Combine historical and predicted data
combined_data <- bind_rows(
  monthly_visitors %>% mutate(type = "Actual"),
  future_months %>% rename(total_visitors = predicted_visitors) %>% mutate(type = "Predicted")
)

# Plot the historical data and predictions together
ggplot(combined_data, aes(x = served_month, y = total_visitors, group = served_year, color = type)) +
  geom_line(aes(linetype = type)) +  # Different line type for predictions
  labs(title = "Monthly Visitors with 2024 Predictions (Weighted Regression)",
       x = "Month", y = "Number of Visitors", color = "Data Type", linetype = "Data Type") +
  theme_minimal() +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red"))

# Calculate the total number of predicted visitors for 2024
total_predicted_visitors_2024 <- sum(future_months$predicted_visitors)
total_predicted_visitors_2024
#####################################
#FINAL MODEL, MOST ACCURATE
#install.packages("prophet")
library(prophet)
library(dplyr)
library(ggplot2)

monthly_visitors <- all %>%
  group_by(served_year, served_month) %>%
  summarise(total_visitors = n(), .groups = "drop")

monthly_visitors <- monthly_visitors %>%
  mutate(ds = as.Date(paste(served_year, served_month, "01", sep = "-"), format = "%Y-%B-%d"),
         y = total_visitors) %>%
  select(ds, y) # Prophet needs columns named 'ds' (date) and 'y' (values)

model <- prophet()
model <- add_seasonality(model, name = "monthly", period = 12, fourier.order = 2)
model <- fit.prophet(model, monthly_visitors)

future <- make_future_dataframe(model, periods = 12, freq = "month")
forecast <- predict(model, future)

ggplot(forecast, aes(x = ds, y = yhat)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = yhat_lower, ymax = yhat_upper), fill = "lightblue", alpha = 0.3) +
  labs(title = "Visitor Forecast for 2024",
       x = "Month", y = "Predicted Visitors") +
  theme_minimal()

# Sum up the total predicted visitors for 2024
total_predicted_visitors_2024 <- forecast %>%
  filter(ds >= as.Date("2024-01-01") & ds <= as.Date("2024-12-31")) %>%
  summarise(total_visitors = sum(yhat)) %>%
  pull(total_visitors)

# Display the result
total_predicted_visitors_2024

######################################################################
#2024 data, checking our forecast
data_2024 <-read.csv("DMARC Data 2018-2024 copy.csv")
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(lubridate)

# Convert servedDate to Date format
data_2024 <- data_2024 %>% 
  mutate(servedDate = as.Date(servedDate))  

# Subset only dates from 2024
data_2024_2024 <- data_2024 %>% 
  filter(year(servedDate) == 2024)

# Verify filtering
print(nrow(data_2024_2024)) # Should match the expected count

# Ensure month names match the expected format
data <- data_2024_2024 %>%  
  mutate(
    served_year = year(servedDate), 
    served_month = format(servedDate, "%B"),  # Full month names for consistency
    served_weekday = weekdays(servedDate),
    
    # Create MonthYear_date for easy graphing
    MonthYear_date = as.Date(paste(served_year, served_month, "01", sep = "-"), 
                             format = "%Y-%B-%d")
  )  

# Aggregate visitors by month
monthly_visitors_2024 <- data %>%
  group_by(served_year, served_month) %>%
  summarise(total_visitors = n(), .groups = "drop")

# Ensure months are ordered correctly
monthly_visitors_2024$served_month <- factor(monthly_visitors_2024$served_month, 
                                             levels = month_order)

# Plot the actual monthly visitor data for 2024
ggplot(monthly_visitors_2024, aes(x = served_month, y = total_visitors, group = served_year, color = as.factor(served_year))) +
  geom_line() +
  labs(title = "Monthly Visitors Over the Years",
       x = "Month", y = "Number of Visitors", color = "Year") +
  theme_minimal()

# Compute total visitors for 2024
total_visitors_2024 <- sum(monthly_visitors_2024$total_visitors)
print(total_visitors_2024)














