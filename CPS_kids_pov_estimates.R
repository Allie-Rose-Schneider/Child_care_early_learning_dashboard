#########################################
## Estimated percentage of children in poverty

## Written: 9/14/2023
## Allie Schneider, CAP, Early Childhood
#######################################
## packages

#install.packages('ipumsr')
library(ipumsr)
library(dplyr)
library(tidyverse)
library(survey)
library(gtsummary)
library(writexl)

#setwd("C:/Users/aschneider/OneDrive - Center For American Progress/Documents/Data_hub_code_files/CPS Poverty Data")

## Working with IPUMS CPS Extract -- 

# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

## load data
ddi <- read_ipums_ddi("cps_00013.xml")
data <- read_ipums_micro(ddi)

## create a subset of the data with only ages under 6
pov_data_kids <- data[which(data$AGE<=5),]

## subset for under age 5 (using for EHS/HS percent in pov served)
pov_data_under5 <- data[which(data$AGE<=4)]

## --------------------------------------------------------------------------------
## NOTES
## --------------------------------------------------------------------------------

## We want to look at SPM poverty and official poverty rate for children 
## at the national level

## OFFPOV -- official poverty status (IPUMS constructed)
## coding: 01 -> below poverty line, 02 -> above poverty line, 99 -> not in universe

## SPMPOV -- SPM unit's poverty status
## coding: 0 -> Above poverty (SPM), 1 -> Below poverty (SPM)


## Weighting for calculating OFFPOV --
## ASECWT is a person-level weight that should be used in analyses of individual-level 
## CPS supplement data. Since the CPS relies on a complex stratified sampling scheme, 
## it is essential to use one of the provided weighting variables.

## Weighting for calculating SPMPOV --
## Researchers should use the weight developed for the SPM when using SPMPOV. This weight 
## is available in SPMWT.SPM weight is a family based weight and should be used if doing 
## family-level analysis (e.g. % families receiving foodstamps). For individual-level 
## analysis, the ASEC supplement weights should be used.

## ASECWT can thus be used for both analyses ****

## --------------------------------------------------------------------------------

### UPDATE -- commenting out state-level analysis because CPS ASEC should only be used for 
### national-level estimates

## PREPPING DATA

## recode OFFPOV variables
pov_data_kids <- pov_data_kids %>%
  ## recode OFFPOV variable
  mutate(OFFPOV_new = case_when(
    OFFPOV == 1 ~ 1,
    OFFPOV == 2 ~ 0,
    OFFPOV == 99 ~ NA)) 

# ## input state names for FIP Codes
# pov_data_kids <- pov_data_kids %>%
#   mutate(STATE = case_when(
#     STATEFIP == 1 ~ "AL",
#     STATEFIP == 2 ~ "AK",
#     STATEFIP == 4 ~ "AZ",
#     STATEFIP == 5 ~ "AR",
#     STATEFIP == 6 ~ "CA",
#     STATEFIP == 8 ~ "CO",
#     STATEFIP == 9 ~ "CT",
#     STATEFIP == 10 ~ "DE",
#     STATEFIP == 11 ~ "DC",
#     STATEFIP == 12 ~ "FL",
#     STATEFIP == 13 ~ "GA",
#     STATEFIP == 15 ~ "HI",
#     STATEFIP == 16 ~ "ID",
#     STATEFIP == 17 ~ "IL",
#     STATEFIP == 18 ~ "IN",
#     STATEFIP == 19 ~ "IA",
#     STATEFIP == 20 ~ "KS",
#     STATEFIP == 21 ~ "KY",
#     STATEFIP == 22 ~ "LA",
#     STATEFIP == 23 ~ "ME",
#     STATEFIP == 24 ~ "MD",
#     STATEFIP == 25 ~ "MA",
#     STATEFIP == 26 ~ "MI",
#     STATEFIP == 27 ~ "MN",
#     STATEFIP == 28 ~ "MS",
#     STATEFIP == 29 ~ "MO",
#     STATEFIP == 30 ~ "MT",
#     STATEFIP == 31 ~ "NE",
#     STATEFIP == 32 ~ "NV",
#     STATEFIP == 33 ~ "NH",
#     STATEFIP == 34 ~ "NJ",
#     STATEFIP == 35 ~ "NM",
#     STATEFIP == 36 ~ "NY",
#     STATEFIP == 37 ~ "NC",
#     STATEFIP == 38 ~ "ND",
#     STATEFIP == 39 ~ "OH",
#     STATEFIP == 40 ~ "OK",
#     STATEFIP == 41 ~ "OR",
#     STATEFIP == 42 ~ "PA",
#     STATEFIP == 44 ~ "RI",
#     STATEFIP == 45 ~ "SC",
#     STATEFIP == 46 ~ "SD",
#     STATEFIP == 47 ~ "TN",
#     STATEFIP == 48 ~ "TX",
#     STATEFIP == 49 ~ "UT",
#     STATEFIP == 50 ~ "VT",
#     STATEFIP == 51 ~ "VA",
#     STATEFIP == 53 ~ "WA",
#     STATEFIP == 54 ~ "WV",
#     STATEFIP == 55 ~ "WI",
#     STATEFIP == 56 ~ "WY"
#   ))

## Dealing with NIU (not in universe) values
## Some NIU values exist for the OFFPOV measure -- 968 in total (all years) dataset
## None for the SPM poverty measure
## We will remove rows where the OFFPOV measure is NIU when we calculate OFFPOV estimates


## make a data subset for each year of interest -- 2015 to 2023 
## (refers to survey year, not calendar year -- eg 2023 survey year is for 2022 calendar year)
pov_data_2015 <- pov_data_kids[which(pov_data_kids$YEAR == 2015),]
pov_data_2016 <- pov_data_kids[which(pov_data_kids$YEAR == 2016),]
pov_data_2017 <- pov_data_kids[which(pov_data_kids$YEAR == 2017),]
pov_data_2018 <- pov_data_kids[which(pov_data_kids$YEAR == 2018),]
pov_data_2019 <- pov_data_kids[which(pov_data_kids$YEAR == 2019),]
pov_data_2020 <- pov_data_kids[which(pov_data_kids$YEAR == 2020),]
pov_data_2021 <- pov_data_kids[which(pov_data_kids$YEAR == 2021),]
pov_data_2022 <- pov_data_kids[which(pov_data_kids$YEAR == 2022),]
pov_data_2023 <- pov_data_kids[which(pov_data_kids$YEAR == 2023),]


## --------------------------------------------------------------------------------

## OFFICIAL POVERTY ESTIMATES STATE-BY-STATE
## Probably some more efficient way of doing this... but loop wasn't working for me

## Because some NIU (recoded as NIU) values exist for OFFPOV, we want to remove those 
## rows before calculating OFFPOV estimates

# ## 2023 (2022 calendar year)
# OFFPOV_kids_2023 <- pov_data_2023 %>%
#   drop_na() %>%  ## drop rows with NA
#   group_by(STATE) %>%
#   summarise(
#     total = n(),
#     sum_pov_WTS = sum(ASECWT[OFFPOV_new == 1]),
#     sum_all_WTS = sum(ASECWT),
#     weighted_offpov_PCT = 100*sum_pov_WTS/sum_all_WTS
#   )

## 2022 (2021 calendar year)
OFFPOV_kids_2022 <- pov_data_2022 %>% 
  drop_na() %>%  ## drop rows with NA
  group_by(STATE) %>%
  summarise(
    total = n(),
    sum_pov_WTS = sum(ASECWT[OFFPOV_new == 1]),
    sum_all_WTS = sum(ASECWT),
    weighted_offpov_PCT = 100*sum_pov_WTS/sum_all_WTS
  )

## 2021 (2020 calendar year)
OFFPOV_kids_2021 <- pov_data_2021 %>% 
  drop_na() %>%  ## drop rows with NA
  group_by(STATE) %>%
  summarise(
    total = n(),
    sum_pov_WTS = sum(ASECWT[OFFPOV_new == 1]),
    sum_all_WTS = sum(ASECWT),
    weighted_offpov_PCT = 100*sum_pov_WTS/sum_all_WTS
  )

## 2020 (2019 calendar year)
OFFPOV_kids_2020 <- pov_data_2020 %>% 
  drop_na() %>%  ## drop rows with NA
  group_by(STATE) %>%
  summarise(
    total = n(),
    sum_pov_WTS = sum(ASECWT[OFFPOV_new == 1]),
    sum_all_WTS = sum(ASECWT),
    weighted_offpov_PCT = 100*sum_pov_WTS/sum_all_WTS
  )

## 2019 (2018 calendar year)
OFFPOV_kids_2019 <- pov_data_2019 %>% 
  drop_na() %>%  ## drop rows with NA
  group_by(STATE) %>%
  summarise(
    total = n(),
    sum_pov_WTS = sum(ASECWT[OFFPOV_new == 1]),
    sum_all_WTS = sum(ASECWT),
    weighted_offpov_PCT = 100*sum_pov_WTS/sum_all_WTS
  )

## 2018 (2017 calendar year)
OFFPOV_kids_2018 <- pov_data_2018 %>% 
  drop_na() %>%  ## drop rows with NA
  group_by(STATE) %>%
  summarise(
    total = n(),
    sum_pov_WTS = sum(ASECWT[OFFPOV_new == 1]),
    sum_all_WTS = sum(ASECWT),
    weighted_offpov_PCT = 100*sum_pov_WTS/sum_all_WTS
  )

## 2017 (2016 calendar year)
OFFPOV_kids_2017 <- pov_data_2017 %>% 
  drop_na() %>%  ## drop rows with NA
  group_by(STATE) %>%
  summarise(
    total = n(),
    sum_pov_WTS = sum(ASECWT[OFFPOV_new == 1]),
    sum_all_WTS = sum(ASECWT),
    weighted_offpov_PCT = 100*sum_pov_WTS/sum_all_WTS
  )

## 2016 (2015 calendar year)
OFFPOV_kids_2016 <- pov_data_2016 %>% 
  drop_na() %>%  ## drop rows with NA
  group_by(STATE) %>%
  summarise(
    total = n(),
    sum_pov_WTS = sum(ASECWT[OFFPOV_new == 1]),
    sum_all_WTS = sum(ASECWT),
    weighted_offpov_PCT = 100*sum_pov_WTS/sum_all_WTS
  )


## --------------------------------------------------------------------------------

## SPM POVERTY ESTIMATES STATE-BY-STATE

# ## 2023 (2022 calendar year)
# SPM_kids_2023 <- pov_data_2023 %>% 
#   group_by(STATE) %>%
#   summarise(
#     total = n(),
#     sum_pov_WTS = sum(ASECWT[SPMPOV == 1]),
#     sum_all_WTS = sum(ASECWT),
#     weighted_SPM_PCT = 100*sum_pov_WTS/sum_all_WTS
#   )

## 2022 (2021 calendar year)
SPM_kids_2022 <- pov_data_2022 %>% 
  group_by(STATE) %>%
  summarise(
    total = n(),
    sum_pov_WTS = sum(ASECWT[SPMPOV == 1]),
    sum_all_WTS = sum(ASECWT),
    weighted_SPM_PCT = 100*sum_pov_WTS/sum_all_WTS
  )

## 2021 (2020 calendar year)
SPM_kids_2021 <- pov_data_2021 %>% 
  group_by(STATE) %>%
  summarise(
    total = n(),
    sum_pov_WTS = sum(ASECWT[SPMPOV == 1]),
    sum_all_WTS = sum(ASECWT),
    weighted_SPM_PCT = 100*sum_pov_WTS/sum_all_WTS
  )

## 2020 (2019 calendar year)
SPM_kids_2020 <- pov_data_2020 %>% 
  group_by(STATE) %>%
  summarise(
    total = n(),
    sum_pov_WTS = sum(ASECWT[SPMPOV == 1]),
    sum_all_WTS = sum(ASECWT),
    weighted_SPM_PCT = 100*sum_pov_WTS/sum_all_WTS
  )

## 2019 (2018 calendar year)
SPM_kids_2019 <- pov_data_2019 %>% 
  group_by(STATE) %>%
  summarise(
    total = n(),
    sum_pov_WTS = sum(ASECWT[SPMPOV == 1]),
    sum_all_WTS = sum(ASECWT),
    weighted_SPM_PCT = 100*sum_pov_WTS/sum_all_WTS
  )

## 2018 (2017 calendar year)
SPM_kids_2018 <- pov_data_2018 %>% 
  group_by(STATE) %>%
  summarise(
    total = n(),
    sum_pov_WTS = sum(ASECWT[SPMPOV == 1]),
    sum_all_WTS = sum(ASECWT),
    weighted_SPM_PCT = 100*sum_pov_WTS/sum_all_WTS
  )

## 2017 (2016 calendar year)
SPM_kids_2017 <- pov_data_2017 %>% 
  group_by(STATE) %>%
  summarise(
    total = n(),
    sum_pov_WTS = sum(ASECWT[SPMPOV == 1]),
    sum_all_WTS = sum(ASECWT),
    weighted_SPM_PCT = 100*sum_pov_WTS/sum_all_WTS
  )

## 2016 (2015 calendar year)
SPM_kids_2016 <- pov_data_2016 %>% 
  group_by(STATE) %>%
  summarise(
    total = n(),
    sum_pov_WTS = sum(ASECWT[SPMPOV == 1]),
    sum_all_WTS = sum(ASECWT),
    weighted_SPM_PCT = 100*sum_pov_WTS/sum_all_WTS
  )


## --------------------------------------------------------------------------------

## OFFICIAL POVERTY ESTIMATES NATIONAL

#YEAR REFERS TO SURVEY YEAR

OFFPOV_kids_nat <- pov_data_kids %>% 
  drop_na() %>%  ## drop rows with NA
  group_by(YEAR) %>%
  summarise(
    total = n(),
    sum_pov_WTS = sum(ASECWT[OFFPOV_new == 1]),
    sum_all_WTS = sum(ASECWT),
    weighted_offpov_PCT = 100*sum_pov_WTS/sum_all_WTS
  )

## under age 5 for HS/EHS calculation
OFFPOV_under5_nat <- pov_data_under5 %>% 
  drop_na() %>%  ## drop rows with NA
  group_by(YEAR) %>%
  summarise(
    total = n(),
    sum_pov_WTS = sum(ASECWT[OFFPOV_new == 1]),
    sum_all_WTS = sum(ASECWT),
    weighted_offpov_PCT = 100*sum_pov_WTS/sum_all_WTS
  )


## --------------------------------------------------------------------------------

## SPM POVERTY ESTIMATES NATIONAL

## year refers to SURVEY YEAR

SPMPOV_kids_nat <- pov_data_kids %>% 
  group_by(YEAR) %>%
  summarise(
    total = n(),
    sum_pov_WTS = sum(ASECWT[SPMPOV == 1]),
    sum_all_WTS = sum(ASECWT),
    weighted_SPMpov_PCT = 100*sum_pov_WTS/sum_all_WTS
  )



## --------------------------------------------------------------------------------
###################################################################################
###################################################################################

## Clean up for export 


## --------------------------------------------------------------------------------
## OFFPOV ALL YEARS ALL STATES

## Create dataset where each row refers to a state, columns correspond to OFFPOV 
## estimates for each year
## years should correspond to CALENDAR YEAR rather than SURVEY YEAR
## so for example survey year 2023 will be labeled 2022

OFFPOV_kids_state_export <- data.frame(
  'STATE' = OFFPOV_kids_2022$STATE,
  '2022_offpov' = OFFPOV_kids_2023$weighted_offpov_PCT,
  '2021_offpov' = OFFPOV_kids_2022$weighted_offpov_PCT,
  '2020_offpov' = OFFPOV_kids_2021$weighted_offpov_PCT,
  '2019_offpov' = OFFPOV_kids_2020$weighted_offpov_PCT,
  '2018_offpov' = OFFPOV_kids_2019$weighted_offpov_PCT,
  '2017_offpov' = OFFPOV_kids_2018$weighted_offpov_PCT,
  '2016_offpov' = OFFPOV_kids_2017$weighted_offpov_PCT,
  '2015_offpov' = OFFPOV_kids_2016$weighted_offpov_PCT
)

## --------------------------------------------------------------------------------
## SPM ALL YEARS ALL STATES
## Create dataset where each row refers to a state, columns correspond to SPM pov 
## estimates for each year
## years should correspond to CALENDAR YEAR rather than SURVEY YEAR
## so for example survey year 2023 will be labeled 2022

SPM_kids_state_export <- data.frame(
  'STATE' = SPM_kids_2022$STATE,
  '2022_SPM' = SPM_kids_2023$weighted_SPM_PCT,
  '2021_SPM' = SPM_kids_2022$weighted_SPM_PCT,
  '2020_SPM' = SPM_kids_2021$weighted_SPM_PCT,
  '2019_SPM' = SPM_kids_2020$weighted_SPM_PCT,
  '2018_SPM' = SPM_kids_2019$weighted_SPM_PCT,
  '2017_SPM' = SPM_kids_2018$weighted_SPM_PCT,
  '2016_SPM' = SPM_kids_2017$weighted_SPM_PCT,
  '2015_SPM' = SPM_kids_2016$weighted_SPM_PCT
)

## --------------------------------------------------------------------------------
## OFFPOV ALL YEARS NATIONAL

OFFPOV_kids_nat_export <- data.frame(
  'YEAR' = OFFPOV_kids_nat$YEAR,
  'offpov' = OFFPOV_kids_nat$weighted_offpov_PCT
)
## replace survey years with calendar year values
OFFPOV_kids_nat_export <- OFFPOV_kids_nat_export[-c(1),]
OFFPOV_kids_nat_export$YEAR <- c(2015,2016,2017,2018,2019,2020,2021, 2022)


## --------------------------------------------------------------------------------
## SPM ALL YEARS NATIONAL

SPM_kids_nat_export <- data.frame(
  'YEAR' = SPMPOV_kids_nat$YEAR,
  'SPM' = SPMPOV_kids_nat$weighted_SPMpov_PCT
)
## replace survey years with calendar year values
SPM_kids_nat_export <- SPM_kids_nat_export[-c(1),]
SPM_kids_nat_export$YEAR <- c(2015,2016,2017,2018,2019,2020,2021, 2022)


#### Export Data

write_xlsx(OFFPOV_kids_state_export, "/Users/aschneider/OneDrive - Center For American Progress/Documents/Data_hub_code_files/CPS Poverty Data/OFFPOV_kids_state_export.xlsx")

write_xlsx(SPM_kids_state_export, "/Users/aschneider/OneDrive - Center For American Progress/Documents/Data_hub_code_files/CPS Poverty Data/SPM_kids_state_export.xlsx")

write_xlsx(OFFPOV_kids_nat_export, "/Users/aschneider/OneDrive - Center For American Progress/Documents/Data_hub_code_files/CPS Poverty Data/OFFPOV_kids_nat_export.xlsx")

write_xlsx(SPM_kids_nat_export, "/Users/aschneider/OneDrive - Center For American Progress/Documents/Data_hub_code_files/CPS Poverty Data/SPM_kids_nat_export.xlsx")



