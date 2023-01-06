server    <- FALSE
whichData <- "real"   # "fake"

if (!server) source           ("g:/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")
if  (server) source("/mnt/projects/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")



  ed_work <- readRDS(paste0(securePlace, "myData/oshpd_ed.RDS"))
  
  library(summarytools)
  
  
  temp <- ed.work %>% group_by(year,ccs_dx_prin) %>% summarise(N = n()) %>% pivot_wider(values_from = N, names_from = year)
  junk <- ed.work %>% filter(year == 2021, is.na(ccs_dx_prin))
  freq(junk$dx_prin)
  badones <- pull(junk,dx_prin)
  
  moretemp <-   ed.work %>% filter(dx_prin %in% badones) %>% group_by(year, dx_prin) %>% summarise(N = n()) %>% pivot_wider(values_from = N, names_from = year)
  
  
  
  
  R519
  Z20822
  
  
  
  temp <- ed_work %>% group_by(year,ccs_dx_prin) %>% summarise(N = n()) %>% pivot_wider(values_from = N, names_from = year)
  junk <- ed_work %>% filter(year == 2021, is.na(ccs_dx_prin))
  freq(junk$dx_prin)
  badones <- pull(junk,dx_prin)
  
  moretemp <-   ed_work %>% filter(dx_prin %in% badones) %>% group_by(year, dx_prin) %>% summarise(N = n()) %>% pivot_wider(values_from = N, names_from = year)
  
  
  
  cssStuff <- read_excel(paste0(fusionPlace,"0.CCB.resources/Project Resources/CCS mapping/ccs_dx_icd10cm_2019_1-BETA.xlsx"))
  checkThis <- filter(cssStuff,'ICD-10-CM CODE' %in% badones)
  
  
#  R519 - Headache, unspecified
#  Z20822 - Contact with and (suspected) exposure to COVID-19
#  R059 - Cough, unspecified
#  M5450 -  Low back pain, unspecified
#  O99891 -  Other specified diseases and conditions complicating pregnancy
  
  
  # https://www.icd10data.com/ICD10CM/Codes/R00-R99/R50-R69/R51-/R51.9
  # https://www.icd10data.com/ICD10CM/Codes/Z00-Z99/Z20-Z29/Z20-/Z20.822
  # https://www.icd10data.com/ICD10CM/Codes/R00-R99/R00-R09/R05-/R05.9
  
  ccsMap <- read_excel(paste0(ccbUpstream,"upstreamInfo/Fusion-ccs_dx_icd10cm_2019_1-BETA.xlsx"))  %>%
    select(icd10    = "'ICD-10-CM CODE'",
           ccscode  = "'CCS CATEGORY'", 
           ccsname  =  "'CCS CATEGORY DESCRIPTION'") %>%
    mutate(icd10    = str_remove_all(icd10,"'"),
           ccscode  = str_remove_all(ccscode,"'")
    )
  
  ccsMap    <- ccsMap %>% select(-ccsname)
  ed_work <- left_join(ed_work,ccsMap,by=c("dx_prin"="icd10"))
  
  
  check <- filter(ed_work, ccs_dx_prin != ccscode)
  check <- filter(ed_work, is.na(ccs_dx_prin))
  
  
  freq(check$ccsname)
  
  
  check2 <- filter(ed_work, dx_prin == "R519")
  
  check2 <- filter(ed_work, dx_prin == "U071")
  
  