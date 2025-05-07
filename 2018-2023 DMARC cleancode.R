rm(list = ls()) 

### Cleaning Code ### 
library(tidyverse) 
library(lubridate) 
library(ggplot2) 
library(RColorBrewer) 

# note I use a relative file here 
all <- read.csv("data_raw/drake_export_v8_2024-02-13_100754_rev2_nolatlong (2).crdownload") 

# set order for the months for graphing on x-axis 
month_order <- c("January", "February", "March", "April", "May", "June",  
                 "July", "August", "September", "October", "November",  
                 "December") 

# set order for the days of week for graphing on x-axis 
weekday_order <- c("Sunday", "Monday", "Tuesday", "Wednesday",  
                   "Thursday", "Friday", "Saturday") 

# huge cleaning block  
all <- all %>%  
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

# another huge cleaning block  
all <- all %>%  
  mutate( 
    served_date = ymd(served_date), 
    dob = ymd(dob), 
    zip = as.numeric(zip), 
    served_year = year(served_date), 
    served_month = months(served_date), 
    served_weekday = weekdays(served_date, abbreviate = FALSE), 
    MonthYear_date = as.Date(paste( 
      served_year, served_month, "01", sep = "-"),  
      format = "%Y-%b-%d") 
  ) %>%  
  filter(served_year != 2024) 

# Cleaning locations 
all <- subset(all, location %in% c(   
  "Central Iowa Shelter and Service", "Clive Community Services",  
  "Caring Hands Eastview",  "Bidwell Riverside Center",  
  "Impact CAP - Ankeny", "DRAKE AREA FOOD PANTRY", "PARTNERSHIP PLACE",   
  "Polk County North Side", "Polk County River Place",  
  "Salvation Army - Clive",  "URBANDALE FOOD PANTRY", "S.A. TEMPLE (WEST)", 
  "WDM HUMAN SERVICES", "S.A. CITADEL (EAST)")) 

# makes zip numeric  
all$zip <-as.numeric(all$zip) 

# gets rid of all instances where zip is not applicable  
all <- all %>%  
  filter(!is.na(zip))  

# Adds the column pantry_zip that includes the zip code of the pantry  
all <- all %>%  
  mutate(pantry_zip = case_when(  
    location == "Central Iowa Shelter and Service" ~ '50314',  
    location == "Clive Community Services" ~ '50325',  
    location == "Caring Hands Eastview" ~ '50317',  
    location == "Bidwell Riverside Center" ~ '50315',  
    location == "Impact CAP - Ankeny" ~ '50023',  
    location == "DRAKE AREA FOOD PANTRY" ~ '50311',  
    location == "PARTNERSHIP PLACE" ~ '50131',  
    location == "Polk County North Side" ~ '50313',  
    location == "Polk County River Place" ~ '50310',  
    location == "Salvation Army - Clive" ~ '50325',  
    location == "URBANDALE FOOD PANTRY" ~ '50322',  
    location == "S.A. TEMPLE (WEST)" ~ '50314',  
    location == "WDM HUMAN SERVICES" ~ '50265', 
    location == "S.A. CITADEL (EAST)" ~ '50317',  
    TRUE ~ NA_character_  
  ))  

# Make pantry_zip a number  
all$pantry_zip <-as.numeric(all$pantry_zip)  

print(all)

unique(all$household)
