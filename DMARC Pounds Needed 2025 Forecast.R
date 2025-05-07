# Clear environment
rm(list = ls())

# Load required libraries
library(tidyverse)
library(lubridate)
library(forecast)
library(tibble)
library(scales)
library(ggplot2)
library(dplyr)
library(patchwork)


# Load and prepare data
new_data <- read.csv("data_raw/DMARC Data 2018-2024 copy.csv")
new_data <- new_data %>%
  mutate(
    servedDate = ymd_hms(servedDate),
    dob = ymd(dob),
    served_year = year(servedDate),
    served_month = months(servedDate),
    served_weekday = weekdays(servedDate, abbreviate = FALSE),
    MonthYear_date = as.Date(paste(served_year, served_month, "01", sep = "-"),
                             format = "%Y-%B-%d")
  )
new_data$servedDate <- as.Date(new_data$servedDate)

# Household size & pounds needed
new_data <- new_data %>%
  group_by(houseHoldIdAfn, servedDate) %>%
  mutate(household_size = n(), pounds_needed = household_size * 12) %>%
  ungroup()

# Top 7 pantries (excluding affiliates)
last_visit <- new_data %>%
  group_by(location) %>%
  summarise(most_recent_visit = max(`servedDate`, na.rm = TRUE)) %>%
  arrange(desc(most_recent_visit))  # Check which pantries are still active
print(last_visit, n=Inf)

top7_ind_visit_LOCATION <- new_data %>%  
  filter(!location %in% c(
    "Affiliate - Loaves and Fishes", "Affiliate - Creston Area Food Pa",
    "Affiliate - Grace E Free Church", "Affiliate - Wesley Life", "United Upper Nile",
    "S.A. TEMPLE (WEST)", "S.A. CITADEL (EAST)", "Central Iowa Shelter and Service",
    "Affiliate - Dallas Center Food Pantry")) %>%
  group_by(location) %>%  
  summarise(num_people_served = n(), .groups = "drop") %>%  
  arrange(desc(num_people_served)) %>% 
  slice_head(n = 7)
print(top7_ind_visit_LOCATION)

top7_locations <- unique(as.character(top7_ind_visit_LOCATION$location))

top7_2024 <- new_data %>%  
  filter(!location %in% c(
    "Affiliate - Loaves and Fishes", "Affiliate - Creston Area Food Pa",
    "Affiliate - Grace E Free Church", "Affiliate - Wesley Life", "United Upper Nile",
    "S.A. TEMPLE (WEST)", "S.A. CITADEL (EAST)", "Central Iowa Shelter and Service",
    "Affiliate - Dallas Center Food Pantry")) %>%
  filter(served_year == 2024) %>%
  group_by(location) %>%  
  summarise(num_people_served = n(), .groups = "drop") %>%  
  arrange(desc(num_people_served)) %>% 
  slice_head(n = 7)
print(top7_2024)

top8_ind_visit_LOCATION <- new_data %>%
  filter(location %in% c(
    "Urbandale Food Pantry", "Families Forward Bidwell Pantry","Polk County River Place Food Pantry",
    "Johnston Partnership Food Pantry", "Catholic Charities Food Pantry", "WayPoint Resources",
    "West Des Moines Human Services", "IMPACT Community Action Partnership - Drake Neighborhood"),
    served_year >= 2018, served_year <= 2024) %>%
  group_by(location) %>%
  summarise(
    num_people_served_18_24 = n(),
    num_people_served_2024 = sum(served_year == 2024),
    .groups = "drop"
  ) %>%
  arrange(desc(num_people_served_18_24)) %>%
  slice_head(n = 9)
print(top8_ind_visit_LOCATION)

top8_locations <- unique(as.character(top8_ind_visit_LOCATION$location))

# Forecasting
forecast_new_data <- list()
forecast_months_2025 <- seq(from = as.Date("2025-01-01"), by = "month", length.out = 12)

for (loc in top8_locations) {
  pantry_monthly <- new_data %>%
    filter(location == loc) %>%
    mutate(year_month = floor_date(servedDate, "month")) %>%
    group_by(year_month) %>%
    summarise(monthly_pounds = sum(pounds_needed, na.rm = TRUE), .groups = "drop") %>%
    arrange(year_month)
  
  if (nrow(pantry_monthly) < 36) next  # Require at least 3 years of data
  
  start_year <- year(min(pantry_monthly$year_month))
  start_month <- month(min(pantry_monthly$year_month))
  ts_data <- ts(pantry_monthly$monthly_pounds, start = c(start_year, start_month), frequency = 12)
  
  train_length <- length(ts_data) - 12
  train_data <- window(ts_data, end = c(start_year + (train_length - 1) %/% 12, (train_length - 1) %% 12 + 1))
  test_data <- window(ts_data, start = c(start_year + (train_length) %/% 12, (train_length) %% 12 + 1))
  
  # Fit models
  stl_forecast <- stlf(train_data, h = 12, s.window = "periodic")
  arima_model <- auto.arima(train_data)
  arima_forecast <- forecast(arima_model, h = 12)
  ets_model <- ets(train_data)
  ets_forecast <- forecast(ets_model, h = 12)
  tbats_model <- tbats(train_data)
  tbats_forecast <- forecast(tbats_model, h = 12)
  nnetar_model <- nnetar(train_data)
  nnetar_forecast <- forecast(nnetar_model, h = 12)
  
  # RMSE
  rmse_values <- c(
    STL = accuracy(stl_forecast, test_data)[2, "RMSE"],
    ARIMA = accuracy(arima_forecast, test_data)[2, "RMSE"],
    ETS = accuracy(ets_forecast, test_data)[2, "RMSE"],
    TBATS = accuracy(tbats_forecast, test_data)[2, "RMSE"],
    NNAR = accuracy(nnetar_forecast, test_data)[2, "RMSE"]
  )
  
  best_model <- names(which.min(rmse_values))
  cat("Best model for", loc, ":", best_model, "\n")
  
  # Forecast full time series using best model
  final_forecast <- switch(best_model,
                           STL = stlf(ts_data, h = 12, s.window = "periodic"),
                           ARIMA = forecast(auto.arima(ts_data), h = 12),
                           ETS = forecast(ets(ts_data), h = 12),
                           TBATS = forecast(tbats(ts_data), h = 12),
                           NNAR = forecast(nnetar(ts_data), h = 12)
  )
  
  combined_fc <- tibble(
    month = forecast_months_2025,
    predicted_pounds = as.numeric(final_forecast$mean),
    method = best_model,
    location = loc
  )
  
  forecast_new_data[[loc]] <- combined_fc
}

forecast_all_2025 <- bind_rows(forecast_new_data)

###################################################
actual_pounds <- new_data %>%
  filter(location %in% top8_locations,
         year(servedDate) >= 2018, year(servedDate) <= 2024) %>%
  mutate(month = floor_date(servedDate, "month")) %>%
  group_by(location, month) %>%
  summarise(
    pounds = sum(pounds_needed, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(type = "Actual")

# ---------------------------------------------------------------
# Forecast 2024 Pounds Using STL (for production use)
# ---------------------------------------------------------------
forecast_list <- list()

for (pantry in top8_locations) {
  pantry_monthly <- new_data %>%
    filter(location == pantry) %>%
    mutate(year_month = floor_date(servedDate, "month")) %>%
    group_by(year_month) %>%
    summarise(monthly_pounds = sum(pounds_needed, na.rm = TRUE), .groups = "drop") %>%
    arrange(year_month)
  
  if (nrow(pantry_monthly) < 24) next
  
  ts_pantry <- ts(pantry_monthly$monthly_pounds, start = c(year(min(pantry_monthly$year_month)), month(min(pantry_monthly$year_month))), frequency = 12)
  
  fc <- stlf(ts_pantry, h = 12, s.window = "periodic")
  
  forecast_df <- tibble(
    location = pantry,
    month = seq(from = as.Date("2025-01-01"), by = "month", length.out = 12),
    pounds = as.numeric(fc$mean),
    type = "Forecast"
  )
  
  forecast_list[[pantry]] <- forecast_df
}

forecast_pounds <- bind_rows(forecast_list)

# ---------------------------------------------------------------
# Combine Forecast + Actual for Plotting
# ---------------------------------------------------------------
combined_data <- bind_rows(actual_pounds, forecast_pounds)

# ---------------------------------------------------------------
# Visualization of Actual vs Forecasted Pounds
# ---------------------------------------------------------------
ggplot(combined_data, aes(x = month, y = pounds, color = location, linetype = type)) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "Monthly Pounds of Food Needed by Pantry (2018–2025)",
    subtitle = "Actuals (2018–2024) and Forecasts (2024) for Top 8 Pantries",
    x = "Month", y = "Pounds of Food",
    color = "Pantry", linetype = "Data Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# Calculate 95th percentile buffer
historical_buffer <- new_data %>%
  filter(location %in% top8_locations,
         year(servedDate) >= 2018, year(servedDate) <= 2024) %>%
  mutate(month = floor_date(servedDate, "month")) %>%
  group_by(location, month) %>%
  summarise(monthly_pounds = sum(pounds_needed, na.rm = TRUE), .groups = "drop") %>%
  mutate(month_num = month(month)) %>%
  group_by(location, month_num) %>%
  summarise(
    avg_pounds = mean(monthly_pounds),
    high_pounds = quantile(monthly_pounds, 0.95),
    .groups = "drop"
  ) %>%
  mutate(suggested_buffer = high_pounds - avg_pounds)

# Add buffer to forecasts
forecast_with_buffer_2025 <- forecast_all_2025 %>%
  mutate(month_num = month(month)) %>%
  left_join(historical_buffer, by = c("location", "month_num")) %>%
  mutate(total_needed_with_buffer = predicted_pounds + suggested_buffer)


# Table
suggested_pounds_table <- forecast_with_buffer_2025 %>%
  select(location, month, predicted_pounds, total_needed_with_buffer) %>%
  mutate(
    forecasted_lbs = round(predicted_pounds),
    forecasted_lbs_with_buffer = round(total_needed_with_buffer),
    month = format(month, "%b %Y")
  ) %>%
  select(location, month, forecasted_lbs, forecasted_lbs_with_buffer) %>%
  arrange(location, month)

print(suggested_pounds_table, n = Inf)


# Function to calculate RMSE
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2, na.rm = TRUE))
}

# Forecasting and buffer calculation
for (pantry in top8_locations) {
  # Aggregate monthly total pounds needed per pantry
  pantry_monthly <- new_data %>%
    filter(location == pantry) %>%
    mutate(year_month = floor_date(servedDate, "month")) %>%
    group_by(year_month) %>%
    summarise(monthly_pounds = sum(pounds_needed, na.rm = TRUE), .groups = "drop") %>%
    arrange(year_month)
  
  if (nrow(pantry_monthly) < 24) next  # Skip if less than 2 years of data
  
  # Create time series object for forecasting
  start_year <- year(min(pantry_monthly$year_month))
  start_month <- month(min(pantry_monthly$year_month))
  ts_pantry <- ts(pantry_monthly$monthly_pounds, start = c(start_year, start_month), frequency = 12)
  
  # Fit multiple models
  fc_stlf <- stlf(ts_pantry, h = 12, s.window = "periodic")
  fc_ets <- ets(ts_pantry)
  fc_arima <- auto.arima(ts_pantry)
  
  # Calculate RMSE for each model
  actual_values <- pantry_monthly$monthly_pounds[(length(pantry_monthly$monthly_pounds)-11):length(pantry_monthly$monthly_pounds)]
  
  rmse_stlf <- rmse(actual_values, as.numeric(fc_stlf$mean))
  rmse_ets <- rmse(actual_values, forecast(fc_ets, h = 12)$mean)
  rmse_arima <- rmse(actual_values, forecast(fc_arima, h = 12)$mean)
  
  # Select the model with the lowest RMSE
  best_model <- ifelse(rmse_stlf <= rmse_ets & rmse_stlf <= rmse_arima, "STL", 
                       ifelse(rmse_ets <= rmse_arima, "ETS", "ARIMA"))
  
  # Fit the best model based on RMSE
  if (best_model == "STL") {
    forecast_best <- fc_stlf
  } else if (best_model == "ETS") {
    forecast_best <- fc_ets
  } else {
    forecast_best <- fc_arima
  }
  
  # Combine forecast results into a tibble
  forecast_months_2025 <- seq(from = as.Date("2025-01-01"), by = "month", length.out = 12)
  combined_fc <- tibble(
    month = forecast_months_2025,
    predicted_pounds = as.numeric(forecast_best$mean),
    method = paste(best_model, "Forecast"),
    location = pantry
  )
  
  # Merge with buffer data
  forecast_with_buffer_2025 <- combined_fc %>%
    mutate(month_num = month(month)) %>%
    left_join(historical_buffer, by = c("location", "month_num")) %>%
    mutate(
      total_needed_with_buffer = predicted_pounds + suggested_buffer  # Add buffer to forecast
    )
  
  # ---------------------------------------------------------------
  # Create Individual Graph for Each Location
  # ---------------------------------------------------------------
  ggplot(forecast_with_buffer_2025, aes(x = month, y = predicted_pounds, color = location, linetype = method, group = interaction(location, method))) +
    geom_line(linewidth = 1.2) +  # Main forecast lines
    geom_line(aes(y = total_needed_with_buffer), linetype = "dashed", linewidth = 1.2) +  # Dashed buffer lines
    labs(
      title = paste("Forecasted Pounds of Food Needed by Pantry (2025) -", pantry),
      subtitle = paste("Solid Line: Forecasted lbs, Dashed Line: Forecasted lbs with buffer", best_model, "Model"),
      x = "Month", y = "Forecasted Pounds of Food",
      color = "Pantry", linetype = "Forecast Method"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
  
  # ---------------------------------------------------------------
  # Create Individual Table for Each Location with Predicted Values
  # ---------------------------------------------------------------
  suggested_pounds_table <- forecast_with_buffer_2025 %>%
    select(location, month, predicted_pounds, total_needed_with_buffer) %>%
    mutate(
      predicted_pounds = round(predicted_pounds, 0),  # Round for presentation
      total_needed_with_buffer = round(total_needed_with_buffer, 0)
    ) %>%
    rename(
      forecasted_lbs = predicted_pounds,
      forecasted_lbs_with_buffer = total_needed_with_buffer
    ) %>%
    mutate(month = format(month, "%b %Y")) %>%
    arrange(location, month)
  
  print(suggested_pounds_table, n = Inf)  # Print individual table for each location
}

# Loop through top 7 locations
forecast_plots <- list()

for (loc in top8_locations) {
  loc_data <- forecast_with_buffer_2025 %>%
    filter(location == loc)
  
  # Create plot
  p <- ggplot(loc_data, aes(x = month)) +
    geom_line(aes(y = predicted_pounds, color = "Forecast"), linewidth = 1.2) +
    geom_line(aes(y = total_needed_with_buffer, color = "With Buffer"), linetype = "dashed", linewidth = 1.2) +
    labs(
      title = paste("Forecast -", loc),
      subtitle = paste("Method:", unique(loc_data$method)),
      x = "Month", y = "Pounds", color = "Legend"
    ) +
    scale_color_manual(values = c("Forecast" = "blue", "With Buffer" = "red")) +
    theme_minimal(base_size = 10)
  
  forecast_plots[[loc]] <- p
  
  # Create and print forecast table
  tab <- loc_data %>%
    mutate(
      month = format(month, "%b %Y"),
      forecasted_lbs = round(predicted_pounds),
      forecasted_lbs_with_buffer = round(total_needed_with_buffer),
      forecasted_people = round(predicted_pounds / 12)
    ) %>%
    select(month, forecasted_people, forecasted_lbs, forecasted_lbs_with_buffer)
  
  cat("\n===== Forecast Table for", loc, "=====\n")
  print(tab)
}

# Display all plots in a grid
wrap_plots(forecast_plots, ncol = 2)


# Loop through top 7 locations
forecast_plots <- list()

for (loc in top8_locations) {
  loc_data <- forecast_with_buffer_2025 %>%
    filter(location == loc)
  
  # Create plot
  p <- ggplot(loc_data, aes(x = month)) +
    geom_line(aes(y = predicted_pounds, color = "Forecast"), linewidth = 1.2) +
    geom_line(aes(y = total_needed_with_buffer, color = "With Buffer"), linetype = "dashed", linewidth = 1.2) +
    labs(
      title = paste("Forecast -", loc),
      subtitle = paste("Method:", unique(loc_data$method)),
      x = "Month", y = "Pounds", color = "Legend"
    ) +
    scale_color_manual(values = c("Forecast" = "blue", "With Buffer" = "red")) +
    scale_y_continuous(labels = comma) +
    theme_minimal(base_size = 10)
  
  
  # Save to list (optional)
  forecast_plots[[loc]] <- p
  
  # Print plot
  print(p)
  
  # Create and print forecast table
  tab <- loc_data %>%
    mutate(
      month = format(month, "%b %Y"),
      forecasted_lbs = round(predicted_pounds),
      forecasted_lbs_with_buffer = round(total_needed_with_buffer),
      forecasted_people = round(predicted_pounds / 12)
    ) %>%
    select(month, forecasted_people, forecasted_lbs, forecasted_lbs_with_buffer)
  
  cat("\n===== Forecast Table for", loc, "=====\n")
  print(tab)
}


