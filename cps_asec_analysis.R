################################################################################
### Code to calculate the percentage of infants/toddlers in poverty using the 
### SPM and OPM and data from the CPS ASEC. 

### Date last edited: September 11, 2025
### Author: Allie Schneider, ZTT Policy Research Analyst

################################################################################

## ---- Notes -----
# --> More can be found here on which data source to use: https://www.census.gov/topics/income-poverty/poverty/guidance/data-sources.html

# --> Survey dates are representative of one year prior. For example, the 2025 CPS ASEC 
#     is based on data collected in 2024.

# --> We were interested in both poverty rates for the infant/toddler population
#     and the population of young children ages 0-5. This code calculates estimates for both.

# --> The Census Bureau only provides csv microdata files for survey years 2019-2025. 
#     This code thus calculates poverty estimates starting at survey year 2019.
#     The SPM estimates for 2019 and beyond reflect the implementation of revised SPM methodology.

################################################################################

# Load packages
library(readr)
library(dplyr)
library(survey)
library(purrr)
library(ggplot2)
library(tidyr)
library(openxlsx)


# ---- Function to calculate poverty estimates for a given year ----
process_year <- function(year, base_dir) {
  
  # Step 1: Build paths
  zip_path  <- file.path(base_dir, paste0("asec", substr(year, 3, 4), ".zip"))
  unzip_dir <- file.path(base_dir, paste0("asec", substr(year, 3, 4), "_unzipped"))
  
  # Step 2: Unzip, overwrite okay
  unzip(zip_path, exdir = unzip_dir, overwrite = TRUE)
  
  # Step 3: Load files
  person_file <- file.path(unzip_dir, paste0("pppub", substr(year, 3, 4), ".csv"))
  repl_file   <- file.path(unzip_dir, paste0("asec_csv_repwgt_", year, ".csv"))
  
  person <- read_csv(person_file)
  repl   <- read_csv(repl_file)
  
  #Standardize case to lowercase
  names(person) <- tolower(names(person))
  names(repl)   <- tolower(names(repl))
  
  # Step 4: Merge replicate weights
  
  person <- left_join(person, repl, by = c("ph_seq" = "h_seq", "pppos" = "pppos"))
  
  
  # Step 5: Define age groups

  age_groups <- list(
    "Infants & Toddlers (0-2)" = 0:2,
    "Children <6" = 0:5
  )
  
  # ---- Loop through age groups ----
  results_list <- map_dfr(names(age_groups), function(group) {
    # Subset by age
    subset_data <- person %>% filter(a_age %in% age_groups[[group]])
  
  # Step 6: Setup survey design
  repwts <- grep("^pwwgt[1-9][0-9]*$", names(subset_data), value = TRUE)
  subset_data[repwts] <- lapply(subset_data[repwts], as.numeric)
  
  design <- svrepdesign(
    weights = ~pwwgt0,
    repweights = subset_data[, repwts],
    type = "Fay",
    rho = 0.5,
    data = subset_data,
    combined.weights = TRUE
  )
  
  # Step 7: Poverty estimates
  spm_rate <- svymean(~as.numeric(spm_poor == 1), design)
  opm_rate <- svymean(~as.numeric(perlis == 1), design)
  
  # Extract values
  Rate_SPM <- as.numeric(coef(spm_rate)) * 100
  SE_SPM   <- as.numeric(SE(spm_rate)) * 100
  Rate_OPM <- as.numeric(coef(opm_rate)) * 100
  SE_OPM   <- as.numeric(SE(opm_rate)) * 100
  
  # Format results w/ rounding + confidence intervals
  data.frame(
    Year = year - 1,             # Reflects the actual data year (vs the survey year)
    AgeGroup = group,
    Rate_SPM = round(Rate_SPM, 1),
    SE_SPM   = round(SE_SPM, 2),
    CI_Lower_SPM = round(Rate_SPM - 1.96 * SE_SPM, 1),
    CI_Upper_SPM = round(Rate_SPM + 1.96 * SE_SPM, 1),
    Rate_OPM = round(Rate_OPM, 1),
    SE_OPM   = round(SE_OPM, 2),
    CI_Lower_OPM = round(Rate_OPM - 1.96 * SE_OPM, 1),
    CI_Upper_OPM = round(Rate_OPM + 1.96 * SE_OPM, 1)
  )
  })
  
  return(results_list)
}

# ---- Run for survey years 2019–2025 (actually representative of 2018-2024 data) ----
base_dir <- "C:/Users/alschneider/OneDrive - Zero To Three/Documents/Data/CPS_ASEC_analysis"
all_results <- map_dfr(2019:2025, ~process_year(.x, base_dir))

# ---- Split into two sheets ----
infants <- all_results %>% filter(AgeGroup == "Infants & Toddlers (0-2)")
children <- all_results %>% filter(AgeGroup == "Children <6")

# ---- Save to Excel with multiple sheets ----
excel_out <- file.path(base_dir, "CPS_ASEC_Poverty_2019_2025.xlsx")
wb <- createWorkbook()
addWorksheet(wb, "Infants_Toddlers_0_2")
addWorksheet(wb, "Children_Under_6")

writeData(wb, "Infants_Toddlers_0_2", infants)
writeData(wb, "Children_Under_6", children)

saveWorkbook(wb, excel_out, overwrite = TRUE)

# ---- Preview ----
cat("\n✅ Results saved to Excel with two sheets:\n", excel_out, "\n")




