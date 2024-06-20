server <- T

ifelse(server,
       source("/mnt/projects/FusionData/0.CCB/myCCB/Standards/FusionStandards.R"),
       source("G:/FusionData/Standards/0.CCB/myCCB/FusionStandards.R"))

options(scipen=9999)

dataIHME <- readr::read_csv(paste(ccbUpstream,"IHME","IHME-GBD_2009_2018_2019_AllAges_Cause2_Sex.csv",sep='/')) %>% 
  full_join(readr::read_csv(paste(ccbUpstream,"IHME","IHME-GBD_2009_2018_2019_AgeCat_Cause2_Sex.csv",sep='/'))) %>%
  full_join(readr::read_csv(paste(ccbUpstream,"IHME","IHME-GBD_2009_2018_2019_AllAges_Risk2_Sex.csv",sep='/'))) %>%
  full_join(readr::read_csv(paste(ccbUpstream,"IHME","IHME-GBD_2009_2018_2019_AgeCat_Risk2_Sex.csv",sep='/'))) %>% 
  mutate(level=2, display = ifelse(is.na(rei_name),'cause','risk'), id_name = ifelse(is.na(rei_name),cause_name,rei_name)) %>%
  rename(year_id = year, age_group_id = age_id) %>%
  select(measure_id,year_id,location_id,sex_id,age_group_id,metric_id,
         val, upper, lower, id_name, display, level)

write_rds(dataIHME, file = paste(ccbData,'enhancementIHME.RDS',sep='/'))