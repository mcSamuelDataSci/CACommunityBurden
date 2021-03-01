# JASPO - Explore IHME data; compare 2019 data file vs 2017 data file
options(scipen=999)

# 2017 Data - has cause and risk
ihmeData <- readRDS("/mnt/projects/FusionData/0.CCB/myUpstream/IHME/data/v2IHME.RDS")


# Cause list linkage
causeList <- read.csv("/mnt/projects/FusionData/0.CCB/myUpstream/IHME/data/cause_list.csv", header = T) %>%
  select(cause_id, cause_name, level)

# New CAUSE downloaded dataset
dataCause <- read.csv("/mnt/projects/FusionData/0.CCB/myUpstream/IHME/IHME-GBD_2019_Cause.csv", header = T) %>%
  mutate(display = "cause") 

# dataCause <- read.csv("/mnt/projects/FusionData/0.CCB/myUpstream/IHME/IHME-GBD_2019_Cause.csv", header = T) %>%
#   mutate(display = "cause") %>%
#   #left_join(causeList, by = "cause_id")  %>%
#   select(id_num = cause_id,
#          measure_id,
#          year_id = year,
#          location_id,
#          sex_id,
#          age_group_id = age_id,
#          metric_id,
#          val,
#          upper,
#          lower,
#          id_name = cause_name,
#          display)

# New RISK downloaded dataset
dataRisk <- read.csv("/mnt/projects/FusionData/0.CCB/myUpstream/IHME/IHME-GBD_2019_Risk.csv", header = T) %>%
  mutate(display = "risk") %>%
  select(-rei_name, -rei_id)

# dataRisk <- read.csv("/mnt/projects/FusionData/0.CCB/myUpstream/IHME/IHME-GBD_2019_Risk.csv", header = T) %>%
#   mutate(display = "risk") %>%
#   select(id_num = cause_id, 
#          measure_id, 
#          year_id = year, 
#          location_id, 
#          sex_id, 
#          age_group_id = age_id, 
#          metric_id, 
#          val, 
#          upper, 
#          lower, 
#          id_name = cause_name, 
#          display)


fullData <- bind_rows(dataCause, dataRisk)

# check <- filter(fullData, !year_id %in% 2018:2019)


## -------------------------------

# Compare 2017, YLD Rate, Total Sex

oldCheck <- ihmeData %>%
  filter(year_id == 2017, sex_id == 3, measure_id == 3, metric_id == 1, display == "cause") %>%
  select(id_num, old = val, id_name)


newCheck <- dataCause %>%
  filter(year == 2017, sex_id == 3, measure_id == 3, metric_id == 1) %>%
  select(cause_id, new = val, cause_name)

check <- oldCheck %>%
  full_join(newCheck, by = c("id_num" = "cause_id")) %>%
  mutate(diff = new - old, 
         abs = abs(diff), 
         perc_diff = 100*(new - old)/old) %>%
  select(id_num, id_name, cause_name, old, new, diff, abs, perc_diff)


# IDs: 494-497 (Stroke)

stroke <- check %>%
  filter(id_num %in% 494:497)

# ID: 542-557 (Alzheimers, Neurological, ...) - biggest differeneces

# ID 521 (Cirrhosis and other chronic liver diseases) YLD #

# ID 534 (Gallbladder and biliary diseases) YLD #

# ID 626 (Musckoskeletal) YLD #

# ID 619 (Endocrine, metabolic, blood, and immune disorders) YLD #

# pivot wider - # of deaths and YLD

# datCounty

datCounty <- readRDS("/mnt/projects/FusionData/0.CCB/myCCB/myData/real/datCounty.RDS") %>%
  left_join(select(deathCauseLink, causeCode, causeName), by = "causeCode")

server <- T
CCB <- F

source("/mnt/projects/FusionData/Standards/FusionStandards.R")


# All years
oldCheck <- ihmeData %>%
  filter(year_id == 2017, sex_id == 3, measure_id == 3, metric_id == 3, display == "cause") %>%
  select(id_num, old = val, id_name)

oldData <- ihmeData %>%
  select(-location_id, -age_group_id, old = val, -upper, -lower)

newData <- fullData %>%
  filter(!year %in% 2018:2019) %>%
  select(-location_id, -location_name, -age_id, -age_name, -upper, -lower, -cause_name, new = val)

fullCompare <- oldData %>%
  full_join(newData, by = c("year_id" = "year", 
                            "id_num" = "cause_id", 
                            "measure_id", 
                            "metric_id", 
                            "sex_id", 
                            "display")) %>%
  select(year_id, id_name, first_parent, measure_name, metric_name, sex_name, display, old, new) %>%
  mutate(diff = new - old, 
         abs = abs(diff))
