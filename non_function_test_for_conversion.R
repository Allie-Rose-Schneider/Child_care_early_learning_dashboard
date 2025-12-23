

## Set working directory
setwd("C:/Users/aschneider/OneDrive - Center For American Progress/Documents/DATA DASHBOARD/CSV to JSON Conversion")

## Load required packages
library(tidyverse)
library(jsonlite)
library(dplyr)
library(tidyr)


# Read CSV file
ec_data <- read_csv("ec_data_2016_INTEGERS_w_rank_vars_FILLED.csv", na = "n/a")

text_description_entries = 41
basic_statistics_entries = 41


  # #handle n/a and 0 values as we need them to be handled for the dashboard
  retain_strings <- function(x) {
    if (is.character(x) == TRUE) {
      return(x) # Return strings as strings

    } else if (is.na(x)) {
     return("n/a") # Retain "n/a" as a string

    } else if (x == 0) {
      return("0") # Retain 0 as a string
      
    } else if (is.numeric(x) == TRUE) {
      return(as.double(x)) # Retain 0 as a string

    } else {
      return() # Otherwise, keep as is!
    }
  }

  ## apply retain_strings helper function across whole dataset
    ec_data <- ec_data %>%
      mutate(across(everything(), ~ map(.x, retain_strings)))

## Create each of 5 sections of the JSON file, marked before "/" in each column header

## section 1 -- state indicators
state_indicators <- ec_data %>% 
  ## for column headers that start with "State_Indicators/", 
  select(starts_with("State_Indicators/")) %>% 
  ## rename entries by removing category name
  rename_with(~ str_remove(., "State_Indicators/")) %>% 
  filter(state != "") %>% ## state column where state is not blank
  split(.$state)


## section 2 -- basic_statistics
basic_statistics <- ec_data %>% 
  select(starts_with("basic_statistics/")) %>%
  rename_with(~ str_remove(., "basic_statistics/")) %>%
  filter(`SOTS category` != "") %>% 
  ## account for years where there are different numbers of basic_statistics entries
  slice_head(n = basic_statistics_entries)


## section 3 -- text_description
text_description <- ec_data %>% 
  select(starts_with("text_description/")) %>% 
  rename_with(~ str_remove(., "text_description/")) %>% 
  filter(`Web category` != "") %>% 
  ## account for years where there are different numbers of text_description entries
  slice_head(n = text_description_entries)

## section 4 -- state_summary
state_summary <- ec_data %>% 
  select(starts_with("State_Summary/")) %>% 
  rename_with(~ str_remove(., "State_Summary/")) %>% 
  filter(`State` != "")


## Now create the JSON list structure
converted_json <- list(
  State_Indicators = state_indicators,
  basic_statistics = basic_statistics,
  text_description = text_description,
  State_Summary = state_summary,
  CD_Summary = list() # Empty section
)




## Save the newly created JSON file 
write_json(converted_json, path = "ec_data_2016_converted.json", pretty = TRUE, auto_unbox = TRUE, na = "string")




