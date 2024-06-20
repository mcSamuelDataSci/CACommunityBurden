## IHME Global Burden of Disease (GBD) data -- https://vizhub.healthdata.org/gbd-results/
## This R script processes IHME GBD raw data
##   - Create link file for measure, sex, age, cause, and metric. Export link file to myCCB/myInfo
##   - Keep "_id" variables, remove "_name" variables. Export processed IHME data to myCCB/myData


## 6-13-2024 data permalink
## CAUSE: https://vizhub.healthdata.org/gbd-results/?params=gbd-api-2021-permalink/c9278086e13861416b5f1d783a2e5cea
## RISK: https://vizhub.healthdata.org/gbd-results?params=gbd-api-2021-permalink/b028caf06beb05dee8e0501abf30ac3c


## Old data (2019) permalink
## CAUSE: https://vizhub.healthdata.org/gbd-results/?params=gbd-api-2019-permalink/8127d29e45ddd636a5c1da72cc777dd0
## RISK: https://vizhub.healthdata.org/gbd-results/?params=gbd-api-2019-permalink/f62e5e5bd5d5198c68a32b7f055f1e06
## RISK: https://vizhub.healthdata.org/gbd-results/?params=gbd-api-2019-permalink/49c7c2958c28f6e146603cc417e7a952



library(dplyr)
library(readr)
library(openxlsx)


server <- TRUE

if (!server) source           ("g:/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")
if  (server) source("/mnt/projects/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")

ihme_path <- paste0(ccbUpstream, "IHME/")


# Read in IHME cause data
ihme_cause1 <- read_csv(paste0(ihme_path, "data/IHME-GBD_2021_DATA-591719e3-1.csv"))
ihme_cause2 <- read_csv(paste0(ihme_path, "data/IHME-GBD_2021_DATA-591719e3-2.csv"))

ihme_cause <- bind_rows(ihme_cause1, ihme_cause2)


# Read in IHME risk data
ihme_risk <- read_csv(paste0(ihme_path, "data/IHME-GBD_2021_DATA-0fe99698-1.csv"))


# Create cause link from level 1, 2, and 3 cause lists downloaded separately
lev1_cause <- read_csv(paste0(ihme_path, "data/levelMake/level1-IHME-GBD_2021_DATA-2c997b22-1.csv")) %>%
  select(cause_id, cause_name) %>%
  mutate(level="lev1")

lev2_cause <- read_csv(paste0(ihme_path, "data/levelMake/level2-IHME-GBD_2021_DATA-1d2f356d-1.csv")) %>%
  select(cause_id, cause_name) %>%
  mutate(level="lev2")

lev3_cause <- read_csv(paste0(ihme_path, "data/levelMake/level3-IHME-GBD_2021_DATA-38e1fdda-1.csv")) %>%
  select(cause_id, cause_name) %>%
  mutate(level="lev3")

ihmeCauseLink <- bind_rows(lev1_cause, lev2_cause, lev3_cause)


# Create risk link from level 1, 2, and 3 risk lists downloaded separately
lev1_risk <- read_csv(paste0(ihme_path, "data/levelMake/level1-IHME-GBD_2021_DATA-404c71b1-1.csv")) %>%
  select(rei_id, rei_name) %>%
  mutate(level="lev1")

lev2_risk <- read_csv(paste0(ihme_path, "data/levelMake/level2-IHME-GBD_2021_DATA-d3fa56d7-1.csv")) %>%
  select(rei_id, rei_name) %>%
  mutate(level="lev2")

lev3_risk <- read_csv(paste0(ihme_path, "data/levelMake/level3-IHME-GBD_2021_DATA-a2143f2f-1.csv")) %>%
  select(rei_id, rei_name) %>%
  mutate(level="lev3")

ihmeRiskLink <- bind_rows(lev1_risk, lev2_risk, lev3_risk)


# Create other links for measure, sex, age, metric, and risk
ihmeMeasureLink <- ihme_cause %>% 
  select(measure_id, measure_name) %>% 
  unique()

ihmeSexLink <- ihme_cause %>% 
  select(sex_id, sex_name) %>% 
  unique() 

# Convert age_name to factor
ageLevels <- c("<5 years", "5-14 years", "15-19 years", "20-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", 
               "45-49 years", "50-74 years", "75-84 years", "85+ years", "All ages")

ihmeAgeLink <- ihme_cause %>% 
  select(age_id, age_name) %>% 
  mutate(age_name = factor(age_name, levels = ageLevels)) %>% 
  arrange(age_name) %>% 
  unique()

ihmeMetricLink <- ihme_cause %>% 
  select(metric_id, metric_name) %>% 
  unique()



# Create workbook for the link file
wb <- createWorkbook()
addWorksheet(wb, sheetName = "measure", gridLines = FALSE)
addWorksheet(wb, sheetName = "sex", gridLines = FALSE)
addWorksheet(wb, sheetName = "age", gridLines = FALSE)
addWorksheet(wb, sheetName = "metric", gridLines = FALSE)
addWorksheet(wb, sheetName = "cause", gridLines = FALSE)
addWorksheet(wb, sheetName = "risk", gridLines = FALSE)


writeDataTable(wb, sheet = "measure", x = ihmeMeasureLink, colNames = TRUE, rowNames = FALSE)
writeDataTable(wb, sheet = "sex", x = ihmeSexLink, colNames = TRUE, rowNames = FALSE)
writeDataTable(wb, sheet = "age", x = ihmeAgeLink, colNames = TRUE, rowNames = FALSE)
writeDataTable(wb, sheet = "metric", x = ihmeMetricLink, colNames = TRUE, rowNames = FALSE)
writeDataTable(wb, sheet = "cause", x = ihmeCauseLink, colNames = TRUE, rowNames = FALSE)
writeDataTable(wb, sheet = "risk", x = ihmeRiskLink, colNames = TRUE, rowNames = FALSE)


# Export link file
saveWorkbook(wb, file = paste0(ccbInfo, "ihmeLink.xlsx"), overwrite = TRUE)


# remove "_name" and only keep "_id"
ihme_cause_app <- ihme_cause %>% 
  select(-c(ends_with("_name"), location_id)) %>% 
  filter(cause_id %in% ihmeCauseLink$cause_id) # only keep level 1, 2, and 3 causes

ihme_risk_app <- ihme_risk %>% 
  select(-c(ends_with("_name"), location_id)) %>% 
  filter(rei_id %in% ihmeRiskLink$rei_id) # only keep level 1, 2, and 3 risks


# Export processed IHME cause data
saveRDS(ihme_cause_app, file = paste0(ccbData, "ihme_cause.RDS"))

# Export processed IHME risk data
saveRDS(ihme_risk_app, file = paste0(ccbData, "ihme_risk.RDS"))
