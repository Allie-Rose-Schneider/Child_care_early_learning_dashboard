################################################################################
### Code to calculate the percentage of infants/toddlers in poverty by metro area
## using data from the CPS ASEC. 

### Date last edited: September 19, 2025
### Author: Allie Schneider, ZTT Policy Research Analyst

################################################################################


## load libraries
library(dplyr)
library(readr)
library(survey)
library(purrr)
library(openxlsx)
library(tidyr)


# ---- Function to calculate poverty estimates by metro/non-metro ----
process_year_metro <- function(year, base_dir) {
  
  # Step 1: Build paths
  zip_path  <- file.path(base_dir, paste0("asec", substr(year, 3, 4), ".zip"))
  unzip_dir <- file.path(base_dir, paste0("asec", substr(year, 3, 4), "_unzipped"))
  
  # Step 2: Unzip
  unzip(zip_path, exdir = unzip_dir, overwrite = TRUE)
  
  # Step 3: Load files
  person_file <- file.path(unzip_dir, paste0("pppub", substr(year, 3, 4), ".csv"))
  repl_file   <- file.path(unzip_dir, paste0("asec_csv_repwgt_", year, ".csv"))
  family_file <- file.path(unzip_dir, paste0("ffpub", substr(year, 3, 4), ".csv"))
  hh_file     <- file.path(unzip_dir, paste0("hhpub", substr(year, 3, 4), ".csv"))
  
  person <- read_csv(person_file)
  repl   <- read_csv(repl_file)
  family <- read_csv(family_file)
  hh     <- read_csv(hh_file)
  
  # Standardize names
  names(person) <- tolower(names(person))
  names(repl)   <- tolower(names(repl))
  names(family) <- tolower(names(family))
  names(hh)     <- tolower(names(hh))
  
  # Deduplicate
  family_unique <- family %>% group_by(fh_seq) %>% slice(1) %>% ungroup()
  hh_unique     <- hh %>% group_by(h_seq) %>% slice(1) %>% ungroup()
  
  # Merge
  person_full <- person %>%
    left_join(repl,   by = c("ph_seq" = "h_seq", "pppos" = "pppos")) %>%
    left_join(family_unique, by = c("ph_seq" = "fh_seq")) %>%
    left_join(hh_unique,     by = c("ph_seq" = "h_seq"))
  
  # Define age groups
  age_groups <- list(
    "Infants & Toddlers (0-2)" = 0:2,
    "Children <6" = 0:5,
    "Children <18" = 0:17
  )
  
  # Loop
  results_list <- map_dfr(names(age_groups), function(group) {
    
    subset_data <- person_full %>%
      filter(a_age %in% age_groups[[group]]) %>%
      mutate(
        metro_status = case_when(
          gtmetsta == 1 ~ "Metro",
          gtmetsta == 2 ~ "Non-metro",
          gtmetsta == 3 ~ "Not identified",
          TRUE ~ "Unknown"
        )
      )
    
    repwts <- grep("^pwwgt[1-9][0-9]*$", names(subset_data), value = TRUE)
    subset_data[repwts] <- lapply(subset_data[repwts], as.numeric)
    
    # For each metro status
    map_dfr(unique(subset_data$metro_status), function(mcat) {
      subcat <- subset_data %>% filter(metro_status == mcat)
      if (nrow(subcat) == 0) return(NULL)
      
      design <- svrepdesign(
        weights = ~pwwgt0,
        repweights = subcat[, repwts],
        type = "Fay",
        rho = 0.5,
        data = subcat,
        combined.weights = TRUE
      )
      
      # Poverty measures
      spm_rate      <- svymean(~as.numeric(spm_poor == 1), design)
      opm_rate      <- svymean(~as.numeric(perlis == 1), design)
      deep_opm_rate <- svymean(~as.numeric(povll == 1), design, na.rm = TRUE)
      low_opm_rate  <- svymean(~as.numeric(povll %in% 4:7), design, na.rm = TRUE)
      
      # Extract values
      Rate_SPM      <- as.numeric(coef(spm_rate)) * 100
      SE_SPM        <- as.numeric(SE(spm_rate)) * 100
      Rate_OPM      <- as.numeric(coef(opm_rate)) * 100
      SE_OPM        <- as.numeric(SE(opm_rate)) * 100
      Rate_OPM_Deep <- as.numeric(coef(deep_opm_rate)) * 100
      SE_OPM_Deep   <- as.numeric(SE(deep_opm_rate)) * 100
      Rate_OPM_Low  <- as.numeric(coef(low_opm_rate)) * 100
      SE_OPM_Low    <- as.numeric(SE(low_opm_rate)) * 100
      
      # RSE calculations
      RSE_SPM      <- ifelse(Rate_SPM == 0, NA, (SE_SPM / Rate_SPM) * 100)
      RSE_OPM      <- ifelse(Rate_OPM == 0, NA, (SE_OPM / Rate_OPM) * 100)
      RSE_OPM_Deep <- ifelse(Rate_OPM_Deep == 0, NA, (SE_OPM_Deep / Rate_OPM_Deep) * 100)
      RSE_OPM_Low  <- ifelse(Rate_OPM_Low == 0, NA, (SE_OPM_Low / Rate_OPM_Low) * 100)
      
      data.frame(
        Year = year - 1,
        AgeGroup = group,
        MetroStatus = mcat,
        UnweightedN  = nrow(subcat),
        Rate_SPM      = round(Rate_SPM, 1),
        SE_SPM        = round(SE_SPM, 2),
        RSE_SPM       = round(RSE_SPM, 1),
        Flag_SPM      = ifelse(RSE_SPM > 30 | nrow(subcat) < 50, "Unreliable", ""),
        Rate_OPM      = round(Rate_OPM, 1),
        SE_OPM        = round(SE_OPM, 2),
        RSE_OPM       = round(RSE_OPM, 1),
        Flag_OPM      = ifelse(RSE_OPM > 30 | nrow(subcat) < 50, "Unreliable", ""),
        Rate_OPM_Deep = round(Rate_OPM_Deep, 1),
        SE_OPM_Deep   = round(SE_OPM_Deep, 2),
        RSE_OPM_Deep  = round(RSE_OPM_Deep, 1),
        Flag_OPM_Deep = ifelse(RSE_OPM_Deep > 30 | nrow(subcat) < 50, "Unreliable", ""),
        Rate_OPM_Low  = round(Rate_OPM_Low, 1),
        SE_OPM_Low    = round(SE_OPM_Low, 2),
        RSE_OPM_Low   = round(RSE_OPM_Low, 1),
        Flag_OPM_Low  = ifelse(RSE_OPM_Low > 30 | nrow(subcat) < 50, "Unreliable", "")
      )
    })
  })
  
  return(results_list)
}



# ---- Run for survey years 2019–2025 ----
base_dir <- "C:/Users/alschneider/OneDrive - Zero To Three/Documents/Data/CPS_ASEC_analysis"
metro_results <- map_dfr(2019:2025, ~process_year_metro(.x, base_dir))

# ---- Export to Excel ----
excel_out <- file.path(base_dir, "CPS_ASEC_Poverty_MetroStatus_2019_2025.xlsx")
wb <- createWorkbook()

addWorksheet(wb, "MetroStatus")
writeData(wb, "MetroStatus", metro_results)

saveWorkbook(wb, excel_out, overwrite = TRUE)

cat("\n✅ Results saved to Excel:\n", excel_out, "\n")




# ---- Filter to exclude "Not identified" and keep only Metro/Non-metro ----
metro_chart_data <- metro_results %>%
  filter(MetroStatus %in% c("Metro", "Non-metro")) %>%
  select(Year, AgeGroup, MetroStatus, Rate_OPM) %>%
  pivot_wider(
    names_from = MetroStatus,
    values_from = Rate_OPM,
    names_prefix = "OPM_"
  ) %>%
  arrange(AgeGroup, Year)

# ---- Excel output path ----
excel_out_chart <- file.path(base_dir, "Metro_OPM_ChartData.xlsx")

# ---- Create workbook ----
wb <- createWorkbook()

# ---- Add worksheet ----
addWorksheet(wb, "Metro_OPM_Wide")

# ---- Write data ----
writeData(wb, "Metro_OPM_Wide", metro_chart_data)

# ---- Save workbook ----
saveWorkbook(wb, excel_out_chart, overwrite = TRUE)

cat("\n✅ Chart-ready metro OPM data saved to Excel:\n", excel_out_chart, "\n")


