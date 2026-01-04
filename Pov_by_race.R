################################################################################
### Code to calculate the percentage of infants/toddlers in poverty by race/ethnicity
## using data from the CPS ASEC. 

### Date last edited: September 18, 2025
### Author: Allie Schneider, ZTT Policy Research Analyst

################################################################################


### BY RACE ETHNICITY


## load libraries
library(dplyr)
library(readr)
library(survey)
library(purrr)
library(openxlsx)
library(tidyr)



# ---- Function to calculate race/ethnicity poverty with n, SE, RSE ----
poverty_rse_by_age <- function(year, base_dir, age_min, age_max, age_label) {
  
  zip_path  <- file.path(base_dir, paste0("asec", substr(year,3,4), ".zip"))
  unzip_dir <- file.path(base_dir, paste0("asec", substr(year,3,4), "_unzipped"))
  unzip(zip_path, exdir = unzip_dir, overwrite = TRUE)
  
  person_file <- file.path(unzip_dir, paste0("pppub", substr(year,3,4), ".csv"))
  repl_file   <- file.path(unzip_dir, paste0("asec_csv_repwgt_", year, ".csv"))
  family_file <- file.path(unzip_dir, paste0("ffpub", substr(year,3,4), ".csv"))
  
  person <- read_csv(person_file)
  repl   <- read_csv(repl_file)
  family <- read_csv(family_file)
  
  names(person) <- tolower(names(person))
  names(repl)   <- tolower(names(repl))
  names(family) <- tolower(names(family))
  
  family_unique <- family %>% group_by(fh_seq) %>% slice(1) %>% ungroup()
  
  person_full <- person %>%
    left_join(repl, by = c("ph_seq" = "h_seq", "pppos" = "pppos")) %>%
    left_join(family_unique, by = c("ph_seq" = "fh_seq"))
  
  subset_data <- person_full %>%
    filter(a_age >= age_min & a_age <= age_max) %>%
    mutate(
      race_eth = case_when(
        pehspnon == 1 ~ "Hispanic",
        pehspnon == 2 & prdtrace == 1 ~ "Non-Hispanic White",
        pehspnon == 2 & prdtrace == 2 ~ "Non-Hispanic Black",
        pehspnon == 2 & prdtrace == 3 ~ "Non-Hispanic AIAN",    # AI/AN
        pehspnon == 2 & prdtrace == 4 ~ "Non-Hispanic Asian",
        pehspnon == 2 & prdtrace >= 6 ~ "Non-Hispanic Two+ Races",  # NEW
        TRUE ~ "Other/Unknown"
      )
    )
  
  
  repwts <- grep("^pwwgt[1-9][0-9]*$", names(subset_data), value = TRUE)
  subset_data[repwts] <- lapply(subset_data[repwts], as.numeric)
  
  results <- map_dfr(unique(subset_data$race_eth), function(race_cat) {
    
    data_race <- subset_data %>% filter(race_eth == race_cat)
    n_unweighted <- nrow(data_race)
    
    design <- svrepdesign(
      weights = ~pwwgt0,
      repweights = data_race[, repwts],
      type = "Fay",
      rho = 0.5,
      data = data_race,
      combined.weights = TRUE
    )
    
    opm <- svymean(~as.numeric(perlis == 1), design, na.rm = TRUE)
    rate <- as.numeric(coef(opm)) * 100
    se   <- as.numeric(SE(opm)) * 100
    rse  <- ifelse(rate > 0, se / rate * 100, NA)
    
    flag <- case_when(
      rse <= 30 ~ "Stable",
      rse > 30 & rse <= 50 ~ "Caution",
      rse > 50 ~ "Unstable"
    )
    
    data.frame(
      Year = year,
      AgeGroup = age_label,
      Race_Ethnicity = race_cat,
      Unweighted_n = n_unweighted,
      OPM_Rate = round(rate,1),
      SE = round(se,2),
      RSE = round(rse,1),
      Reliability = flag
    )
  })
  
  return(results)
}



# ---- Run for all years ----
base_dir <- "C:/Users/alschneider/OneDrive - Zero To Three/Documents/Data/CPS_ASEC_analysis"

race_0_2_full  <- map_dfr(2019:2025, ~poverty_rse_by_age(.x, base_dir, 0, 2, "Infants & Toddlers (0-2)"))
race_0_5_full  <- map_dfr(2019:2025, ~poverty_rse_by_age(.x, base_dir, 0, 5, "Children_Under_6"))
race_0_17_full <- map_dfr(2019:2025, ~poverty_rse_by_age(.x, base_dir, 0, 17, "Children_Under_18"))



# ---- Save to Excel ----
excel_out <- file.path(base_dir, "CPS_ASEC_Poverty_2019_2025_Race_RSE.xlsx")
wb <- createWorkbook()

addWorksheet(wb, "0_2_By_Race")
addWorksheet(wb, "0_5_By_Race")
addWorksheet(wb, "0_17_By_Race")

# Map worksheet names to actual data frames
sheet_data <- list(
  "0_2_By_Race"  = race_0_2_full,
  "0_5_By_Race"  = race_0_5_full,
  "0_17_By_Race" = race_0_17_full
)

# Bold style
bold_style <- createStyle(textDecoration = "bold")

# Write and style each sheet
for (sheet in names(sheet_data)) {
  df <- sheet_data[[sheet]]
  writeData(wb, sheet, df)
  
  # Bold the OPM_Rate column
  cols_rate <- which(names(df) == "OPM_Rate")
  addStyle(wb, sheet, bold_style,
           rows = 2:(nrow(df) + 1),
           cols = cols_rate,
           gridExpand = TRUE)
}

# Save workbook
saveWorkbook(wb, excel_out, overwrite = TRUE)
cat("\n✅ Race/ethnicity sheets with sample size, RSE, and reliability saved to:\n", excel_out, "\n")




#######################################################
## Create chart-ready data

# ---- Reshape to wide format helper ----
make_wide <- function(df) {
  df %>%
    select(Year, Race_Ethnicity, OPM_Rate) %>%
    pivot_wider(
      names_from = Race_Ethnicity,
      values_from = OPM_Rate
    ) %>%
    arrange(Year)
}


# ---- Create wide-format data ----
race_0_2_wide  <- make_wide(race_0_2_full)
race_0_5_wide  <- make_wide(race_0_5_full)
race_0_17_wide <- make_wide(race_0_17_full)

# ---- Save to Excel ----
excel_out_chart <- file.path(base_dir, "CPS_ASEC_Poverty_ByRace_ChartData.xlsx")
wb <- createWorkbook()

addWorksheet(wb, "Infants_0_2")
writeData(wb, "Infants_0_2", race_0_2_wide)

addWorksheet(wb, "Children_Under_6")
writeData(wb, "Children_Under_6", race_0_5_wide)

addWorksheet(wb, "Children_Under_18")
writeData(wb, "Children_Under_18", race_0_17_wide)

saveWorkbook(wb, excel_out_chart, overwrite = TRUE)

cat("\n✅ Chart-ready wide-format data saved to:\n", excel_out_chart, "\n")












