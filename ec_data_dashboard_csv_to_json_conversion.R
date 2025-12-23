##################################################################
## Author: Allie Schneider (CAP Early Childhood Policy Team, Policy Analyst) 
## Date written: 1/22/2025

## Description: For updates of the ECP team's child care and early learning data 
## dashboard, CSV files for each year of data must be converted to a JSON format
## that matches the requirements of the back-end data dashboard. This conversion 
## takes CSV UTF-8 files for each year of data as input and produces JSON files
## that can be uploaded directly to the dashboard. This code also ensures that 
## in the conversion process, strings, numbers, and n/a values are handled as 
## should be in the dashboard. Specifically, 0 and n/a values must be read in as 
## strings. All numbers must be retained as numbers.

##################################################################
## !!FOR UPDATE!! 
## This code is written so that minimal changes are required for future dashboard
## updates with new years of available data. Throughout the code, these locations
## are marked by the comment "!!FOR UPDATE!!". Two items need to be changed: 
##     1) The working directory must be set to the folder that contains CSV files 
##        to import. JSON files will also be exported into this folder.
##     2) Line 140 of code (years <- 2015:2024) must also be changed to match the 
##        years of data that must be converted. For example, when new 2025 data 
##        is available, this line should read years <- 2015:2025. To only read in 
##        and convert 2025 data, this would read years <- 2025:2025.

## Additionally, import csv files must match naming convention. Files are named
## "ec_data_YEAR_clean.csv". For example, data for 2023 is named 
## "ec_data_2023_clean.csv". The indicators file should be named "Indicators_clean.csv".

##################################################################

## Set working directory
## !!FOR UPDATE!! -- Set working directory to the folder with CSV files to import
setwd("C:/Users/aschneider/OneDrive - Center For American Progress/Documents/DATA DASHBOARD/CSV to JSON Conversion")

# ## Install packages
# install.packages("tidyverse")
# install.packages("jsonlite")
# install.packages("dplyr")

## Load required packages
library(tidyverse)
library(jsonlite)
library(dplyr)

##################################################################
## DEFINE FUNCTION that takes a CSV and converts to JSON
## Number of entries in the basic_statistics category and text_description 
## category vary based on year of available data. 
convert_csv_data_to_json <- function(input_csv, output_json, 
                                     basic_statistics_count,
                                     text_description_count) {

  # Read CSV file
  ec_data <- read_csv(input_csv, na = "n/a")

  
  # Handle n/a and 0 values as we need them to be handled for the dashboard
  retain_strings <- function(x) {
    if (is.character(x) == TRUE) {
      return(x) # Return strings as strings
      
    } else if (is.na(x)) {
      return("n/a") # Retain "n/a" as a string
      
    } else if (x == 0) {
      return("0") # Retain 0 as a string
      
    } else if (is.numeric(x) == TRUE) {
      return(as.double(x)) # Retain numbers as numbers
      
    } else {
      return() # Otherwise, keep as is!
    }
  }
  
  ## Apply retain_strings helper function across whole dataset
  ec_data <- ec_data %>%
    mutate(across(everything(), ~ map(.x, retain_strings)))
  
  ## Create 5 sections of the JSON file. Section names are marked before "/" in 
  ## each column header
  
  ## Section 1 -- state indicators
  state_indicators <- ec_data %>% 
    ## for column headers that start with "State_Indicators/", 
    select(starts_with("State_Indicators/")) %>% 
    ## rename entries by removing category name
    rename_with(~ str_remove(., "State_Indicators/")) %>% 
     filter(state != "")  ## filter by state

  
  
  ## Section 2 -- basic_statistics
  basic_statistics <- ec_data %>% 
    select(starts_with("basic_statistics/")) %>%
    rename_with(~ str_remove(., "basic_statistics/")) %>%
    filter(`SOTS category` != "") %>% 
    ## account for years where there are different numbers of basic_statistics entries
    slice_head(n = basic_statistics_count)
  
  
  ## Section 3 -- text_description
  text_description <- ec_data %>% 
    select(starts_with("text_description/")) %>% 
    rename_with(~ str_remove(., "text_description/")) %>% 
    filter(`Web category` != "") %>% 
   ## account for years where there are different numbers of text_description entries
   slice_head(n = text_description_count)
    
  ## Section 4 -- state_summary
  state_summary <- ec_data %>% 
    select(starts_with("State_Summary/")) %>% 
    rename_with(~ str_remove(., "State_Summary/")) %>% 
    filter(`State` != "")
  
  
  ## Create the JSON list structure
  converted_json <- list(
    State_Indicators = state_indicators,
    basic_statistics = basic_statistics,
    text_description = text_description,
    State_Summary = state_summary,
    CD_Summary = list() # Empty section
  )
  
  
  ## Save the newly created JSON file 
  write_json(converted_json, path = output_json, pretty = TRUE, auto_unbox = TRUE)
  
  message("JSON file successfully created: ", output_json)
}
  

##################################################################
## Create a loop to call the function repeatedly to cycle through each year of data

## First, define the years for which we have data
## !!FOR UPDATE!! -- CHANGE YEARS TO MATCH YEARS OF CSV DATA TO CONVERT
years <- 2015:2024 

## Next, define the number of entries for basic_statistics and text_description. 
## These values change based on the available data for each year.
## Because this code is meant to be easily updated in future years, the following
## for loop is used to easily count and define the number of entries for each year.

## INITIALIZE empty vars
basic_statistics_counts <- c()
text_description_counts <- c()

## FILL vars using loop
for (i in seq_along(years)) {
  ## Retrieves year
  year <- years[i]
  ## Read in csv
  data <- read_csv(paste0("ec_data_", year, "_clean.csv"))
  basic_count <- data %>% 
    filter(`basic_statistics/SOTS category` != "") %>% 
    nrow()
  text_count <- data %>% 
    filter(`text_description/Indicator` != "") %>% 
    nrow()
  basic_statistics_counts[i] <- basic_count
  text_description_counts[i] <- text_count
}

## LOOP over each year of CSV to convert to JSON
for (i in seq_along(years)) {
  ## Retrieves year
  year <- years[i]
  ## Retrieves input CSV file from working directory
  read_in_csv <- paste0("ec_data_", year, "_clean.csv")
  ## Names output JSON file
  export_json <- paste0("ec_data_", year, "_converted.json")
  
  ## call the function for each year
  convert_csv_data_to_json(input_csv = read_in_csv,
                           output_json = export_json,
                           basic_statistics_count = basic_statistics_counts[i],
                           text_description_count = text_description_counts[i])
}

## Convert indicators file!

indicators <- read.csv("Indicators_clean.csv", na = NA)
indicators[is.na(indicators)] <- ""
write_json(indicators, "Indicators_converted.json", pretty = TRUE, auto_unbox = TRUE)


