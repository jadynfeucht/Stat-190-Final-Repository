# Stat-190-Final-Repository

# DMARC Pounds Needed 2025 Forecast

## Overview

The goal of this forecast was to analyze and forecast monthly food needs (in pounds) for the top food pantries in the Des Moines Area Religious Council (DMARC) network from 2018 to 2024. Using historical data, the project predicts the expected food demand in 2025 and suggests buffer quantities to help ensure adequate supply during peak periods.

## Objectives

- Clean and transform raw pantry service data.
- Identify the top 8 food pantries based on historical service volume.
- Build multiple time series forecasting models for each pantry.
- Evaluate model performance using RMSE and select the best one.
- Forecast monthly food needs for 2025 using the best-performing model.
- Calculate a suggested buffer based on the 95th percentile of past data.
- Visualize actual vs. forecasted demand from 2018–2025.
- Generate tables to guide monthly food purchasing or distribution.

## Data

- **Source:** Internal pantry service dataset (2018–2024) from DMARC.
- **Format:** CSV
- **File Name:** `DMARC Data 2018-2024 copy.csv`
- **Key Variables:**
  - `servedDate`: Date of food distribution
  - `houseHoldIdAfn`: Household identifier
  - `location`: Pantry location
  - `dob`: Date of birth (used to count individuals in household)

## Key Features

- Time series forecasting using:
  - Seasonal Decomposition (STL)
  - ARIMA
  - ETS
  - TBATS
  - Neural Networks (NNAR)
- Automatic model selection per pantry based on lowest RMSE.
- Monthly breakdown of food needs in pounds per pantry.
- Visual plots comparing actual and predicted pounds.
- Buffer recommendation per pantry based on past variability.

## How to use Run the file
- Open the DMARC Pounds Needed 2025 Forecast R file
- Install the required R packages
- Change the Data source to wherever you have the data downloaded
- Change the Column names in the code to reflect the column names in your data
- Run the code!
- By the end you should have a table, and graph for each pantry that show you the number of pounds needed and the pounds needed with a buffer.
- You should also have a line graph that shows the pounds needed from 2018 on and the predicted pounds needed for the following year. 
