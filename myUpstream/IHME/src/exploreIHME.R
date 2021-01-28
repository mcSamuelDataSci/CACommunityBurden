rm(list=ls())

ihmeData <- readRDS("/mnt/projects/FusionData/0.CCB/myUpstream/IHME/data/v2IHME.RDS")

unique(ihmeData$display)


# Cause list linkage
causeList <- read.csv("/mnt/projects/FusionData/0.CCB/myUpstream/IHME/data/cause_list.csv", header = T) %>%
  select(cause_id, cause_name, level)

dataCause <- read.csv("/mnt/projects/FusionData/0.CCB/myUpstream/IHME/IHME-GBD_2019_Cause.csv", header = T) %>%
  mutate(display = "cause") %>%
  left_join(causeList, by = "cause_id")

%>%
  select(id_num = cause_id, 
         measure_id, 
         year_id = year, 
         location_id, 
         sex_id, 
         age_group_id = age_id, 
         metric_id, 
         val, 
         upper, 
         lower, 
         id_name = cause_name, 
         display)

dataRisk <- read.csv("/mnt/projects/FusionData/0.CCB/myUpstream/IHME/IHME-GBD_2019_Risk.csv", header = T) %>%
  mutate(display = "risk") %>%
  select(id_num = cause_id, 
         measure_id, 
         year_id = year, 
         location_id, 
         sex_id, 
         age_group_id = age_id, 
         metric_id, 
         val, 
         upper, 
         lower, 
         id_name = cause_name, 
         display)


fullData <- bind_rows(dataCause, dataRisk)

check <- filter(fullData, !year_id %in% 2018:2019)




check1 <- ihmeData %>%
  filter(year_id == 2017, sex_id == 3, measure_id == 3, metric_id == 3, display == "cause") %>%
  select(id_num, old = val, id_name)




check2 <- dataCause %>%
  filter(year == 2017, sex_id == 3, measure_id == 3, metric_id == 3) %>%
  select(cause_id, new = val)

df <- check1 %>%
  full_join(check2, by = c("id_num" = "cause_id")) %>%
  mutate(diff = new - old, 
         abs = abs(diff))
