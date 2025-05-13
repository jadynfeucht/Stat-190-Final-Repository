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
- Automatic model selection per pantry based on the lowest RMSE.
- Monthly breakdown of food needs in pounds per pantry.
- Visual plots comparing actual and predicted pounds.
- Buffer recommendation per pantry based on past variability.

## How to Run the File
- Open the DMARC Pounds Needed 2025 Forecast R file
- Install the required R packages
- Change the Data source to wherever you have the data downloaded
- Change the Column names in the code to reflect the column names in your data
- Run the code!
- By the end you should have a table, and graph for each pantry that show you the number of pounds needed and the pounds needed with a buffer.
- You should also have a line graph that shows the pounds needed from 2018 on and the predicted pounds needed for the following year. 




# DMARC Pounds Needed 2025 Forecast
## Overview

This forecast aimed to use historical service data from the DMARC pantry system to predict the likelihood that an individual client will return. This helps DMARC proactively understand future demand, improve resource planning, and identify key factors driving client return behavior. 

## Objectives

- Clean and transform raw pantry service data for modeling.
- Build a model that can predict the likelihood of a client returning. 
- Understand which factors (like age, income, or SNAP participation) influence return behavior. 
- Evaluate model performance using accuracy, specificity, and sensitivity, and select the best one.
- Visualize the return likelihood based on top characteristics like gender, race, income source, whether they participate in SNAP, and the highest level of education they have attained.

## Data

- **Source:** Internal pantry service dataset (2018–2023) from DMARC.
- **Format:** CSV
- **File Name:** `drake_export_v8_2024-02-13_100754_rev2_nolatlong.csv`
- **Key Variables:**
  
  - `will_return`: A label I created that shows if the client came back after their first visit
  - `servedDate`: Date of food distribution
  - `individual_id`: Unique ID for each client
  - `dob`: Date of birth (used to calculate age)
  - `gender` and `race`: Client demographics
  - `education`: Highest level of education completed
  - `income_source`: Main source of income
  - `snap_household`: Whether they participate in SNAP

## Key Features

- Random Forest Model:
  - Predicts whether someone will return after their first visit.
  - Focuses on demographic and income-related data.
 
- Evaluation
  - Accuracy (how often it's right overall)
  - Sensitivity (how well it predicts returners)
  - Specificity (how well it predicts non-returners)
  - ROC curve and AUC (to compare different probability cutoffs)
  
- Visuals Created
  - Plot showing which variables mattered most to the model
  - ROC curve to show model performance and trade-off between sensitivity/specificity
  - Graph of the distribution of predicted probabilities
  - Graphs of predicted return probabilities, split by actual income for some of the main key variables (gender, race, income source, SNAP participation, and education. 

## How to Run the File
- Open the DMARC Individual Return Forecast R file.
- Install the required R packages.
- Change the Data source to wherever you have the data downloaded.
- Check that your column names match those used in the code.
- Run the code.
- By the end, you'll have a trained model, performance metrics, and several plots you can use to understand and explain what drives people to return.





# DMARC Predict Returning Families 2024
## Overview 
The purpose of this predictive model is to share details of the types of families who will be returning to the pantry in the upcoming year. We would expect that DMARC could use this information to support these families with resources from the pantry. The reason I decided this was important is that I would rather be supporting families in different ways, if possible, to begin seeing them less and less, until they are no longer coming to the pantry. I understand that DMARC does not have extra cash lying around but I would be interested in looking into some external resources to provide to these families.

# Objectives
- Clean and transform raw pantry service data for modeling.
- Predict whether a family will return in 2024 based on their past visit history.
- Visualize the distribution of likelihood of returning families.
- Understand the variables' influence on the end resulting prediction

## Data
- **Source:** Internal pantry service dataset (2018-2023) from DMARC.
- **Format:** CSV
- **File name:** drake_export_v8_2024-02-13_100754_rev2_nolatlong.csv
- **Key variables:** 
	- `family_type`: family members in the home
	- `race`: established by family member
	- `ethnicity`: established by family member
	- `served_year`: year of visit by family to the pantry
		- `year_2023`, `year_2022`, `year_2021`, `year_2020`, `year_2019`, `year_2018`
		  - If a family visited one or more times in a year, the year_20## columns were 1, while no visits was represented by a 0. 

## Key Features 

- Random Forest Model
	- Predicts if a family who has already been to the pantry will return again in 2024 
	- Uses predictor variables by family type and years visited


- Visuals
	- Plot of the predicted family returns in 2023 - training data set for random forest model
	- Evaluate accuracy of the random forest model
	- Plot of the predicted family returns in 2024 
	- Plot of predicted family type of returning families in 2024
	- Plot zoomed in on just the families who are expected to return in 2024, broken out by family type
	- Table of family types' expected likelihood of returning to the pantry
	- Plot with family type consolidated if children were involved
	- Table of family types' (where children were consolidated if involved) expected likelihood of returning to the pantry

## How to run
- Open the file titled DMARC Predict Returning Families.R
- Install all the packages listed
- Run entire code file 
