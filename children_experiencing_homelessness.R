
##CALCULATE Percentage of Children Experiencing Homelessness
## 10/24/24 
## Allie Schneider, CAP, Early Childhood Policy

## DATA NEEDED : 
## Population data for children under age 3 and children ages 3- and 4 -- “Census_pop_number.xlsx”
## Number of 1st graders experiencing homelessness – from Ed Data Express
##        * Download all years available, selecting by 1st grade and homeless children
##        * All years saved in “Homeless_1stgrade_enrollment_2015to2023.csv”
## Number of 1st graders (total) – from IES enrollment data
##        * Saved data by year as “Total_enrollment_1stgrade_YEAR.xlsx”
##        * Formatted data in new spreadsheet labeled “Total_enrollment_1stgrade_2015to2022.xlsx”
####################################################################################################

## SET-UP
library(dplyr)
library(tidyverse)
library(survey)
library(gtsummary)
library(writexl)
library(readxl)

#Set working directory
setwd("C:/Users/aschneider/OneDrive - Center For American Progress/Documents/DATA DASHBOARD/Data_hub_code_files/")



## STEP 1: Load in data

# read in data

## Census population counts for children ages 0-3 and ages 3&4
census_population_children <- read_excel("Census_pop_number.xlsx") # 2015-2023 

## 2015-2023 total number of 1st graders experiencing homelessness, counted by McKinny-Vento definition of homelessness
homeless_1stgraders <- read_excel("Homeless_1stgrade_enrollment_2015to2023_reformatted.xlsx")

## Fall 2015-2022, total number of 1st graders enrolled in public school
total_1stgraders <- read_excel("Total_enrollment_1stgrade_2015to2022.xlsx") 

## REMOVE EXTRA COLUMNS in homeless_1stgraders dataset



## STEP X: Estimate percentage of 1st graders experiencing homelessness in each state and in the United States

## create new data frame with estimates

pct_homeless_1stgraders <- data.frame(
  'State' = total_1stgraders$`State name`,
  'num_homeless_1stgraders_2015' = homeless_1stgraders[which(homeless_1stgraders$Year == 2015),]$num_homeless_1stgraders
)


## STEP X: Estimate number of infants/toddlers experiencing homelessness

## STEP X: Estimate number of 3- and 4-year-olds experiencing homelessness

## STEP X: Estimate percentage of infants/toddlers experiencing homelessness

## STEP X: Estimate percentage of 3- and 4-year-olds experiencing homelessness







