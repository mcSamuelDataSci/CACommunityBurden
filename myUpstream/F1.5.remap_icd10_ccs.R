# ICD-10-CM to CCS Remapping

## Change the ICD-10-CM to CCS mapping for the following conditions:
## 1. Drug overdose (based on discussion with the Substance and Addiction Prevention Branch (SAPB))
##    Used California Overdose Surveillance Dashboard's Data Definition for ED Visits and Hospitalizations as a starting point
##    https://skylab.cdph.ca.gov/ODdash/?tab=DD

## 2. Alcohol-related
##    Used CDC's alcohol-related ICD codes for 100% Alcohol-Attributable
##    https://www.cdc.gov/alcohol/ardi/alcohol-related-icd-codes.html


## Read in the original ICD-10-CM to CCS mapping
ccsMap <- read_excel(paste0(ccbUpstream,"upstreamInfo/FUSION-ccs_dx_icd10cm_2019_1-BETA.xlsx"))

## Rename columns for easier processing
names(ccsMap)[names(ccsMap) == "'ICD-10-CM CODE'"] <- "icd10"
names(ccsMap)[names(ccsMap) == "'CCS CATEGORY'"] <- "ccscode"
names(ccsMap)[names(ccsMap) == "'CCS CATEGORY DESCRIPTION'"] <- "ccscodeDesc"


ccsMap <- ccsMap %>% 
  mutate(icd10 = str_remove_all(icd10, "'"), 
         ccscode = str_remove_all(ccscode, "'"))

## Drug overdose ---------------------------------------------------------------
## Remap ICD-10-CM codes between T36 and T50
ccsMap <- ccsMap %>% 
  mutate(char1 = str_sub(icd10, 1, 1), 
         dig23 = as.numeric(str_sub(icd10, 2, 3)), 
         char24 = str_sub(icd10, 2, 4),
         char14 = str_sub(icd10, 1, 4), 
         char5 = str_sub(icd10, 5, 5),
         char6 = str_sub(icd10, 6, 6),
         )

ccsMap <- ccsMap %>% 
  mutate(drugOD = (char1 == "T") & (dig23 >= 36) & (dig23 <= 50))


## List of ICD-10-CM codes where intent is in the 5th character instead of the 6th character
fifthChar <- c("369", "379", "399", "414", "427", "439", "459", "479", "499")


## Drug overdose (NEW ccscode = 1000) includes accidental (5th/6th character = 1) and undetermined intent (5th/6th character = 4)
## Suicide (ccscode = 662) - 5th/6th character = 2
## Assault by drug (NEW ccscode = 1001) - 5th/6th character = 3
## Adverse effect (ccscode = 2617) - 5th/6th character = 5
## Underdosing (ccscode = 244) - 5th/6th character = 6

ccsMap <- ccsMap %>% 
  mutate(ccscode = case_when(
    drugOD & (char24 %in% fifthChar & (char5 == "1" | char5 == "4") | !(char24 %in% fifthChar) & (char6 == "1" | char6 == "4")) ~ "1000", # drug overdose [NEW]
    drugOD & (char24 %in% fifthChar & char5 == "2" | !(char24 %in% fifthChar) & char6 == "2") ~ "662", # suicide
    drugOD & (char24 %in% fifthChar & char5 == "3" | !(char24 %in% fifthChar) & char6 == "3") ~ "1001", # assault [NEW]
    drugOD & (char24 %in% fifthChar & char5 == "5" | !(char24 %in% fifthChar) & char6 == "5") ~ "2617", # adverse effect
    drugOD & (char24 %in% fifthChar & char5 == "6" | !(char24 %in% fifthChar) & char6 == "6") ~ "244", # underdosing
    TRUE ~ ccscode
  ))


ccsMap <- ccsMap %>% 
  mutate(ccscodeDesc = case_when(
    drugOD & (char24 %in% fifthChar & (char5 == "1" | char5 == "4") | 
                !(char24 %in% fifthChar) & (char6 == "1" | char6 == "4")) ~ "Drug overdose", # drug overdose [NEW]
    drugOD & (char24 %in% fifthChar & char5 == "2" | 
                !(char24 %in% fifthChar) & char6 == "2") ~ "Suicide and intentional self-inflicted injury", # suicide
    drugOD & (char24 %in% fifthChar & char5 == "3" | 
                !(char24 %in% fifthChar) & char6 == "3") ~ "Assault by drug", # assault [NEW]
    drugOD & (char24 %in% fifthChar & char5 == "5" | 
                !(char24 %in% fifthChar) & char6 == "5") ~ "Adverse effects of medical drugs", # adverse effect
    drugOD & (char24 %in% fifthChar & char5 == "6" | 
                !(char24 %in% fifthChar) & char6 == "6") ~ "Other injuries and conditions due to external causes", # underdosing
    TRUE ~ ccscodeDesc
  ))


## Add a column to note reason to add/change
ccsMap <- ccsMap %>% 
  mutate('Reason to change' = case_when(
    ccscode == "3000" | ccscode == "3001" ~ "New condition", # Covid-19
    drugOD ~ "OPP/SAPB discussion", 
    TRUE ~ ""
  ))




## Drug overdose ---------------------------------------------------------------
## List of ICD-10 CM codes for 100% Alcohol-Attributable
alcohol_icd <- c("F103", "F104", "F105", "F106", "F107", "F108", "F109", "F100", "F101", "F102", 
                 "G621", "G312", "G721", "I426", "K292", "K700", "K701", "K702", "K703", "K704",
                 "K709", "K852", "K860", "Q860", "P043")

## Remap ICD-10-CM codes for 100% Alcohol-Attributable to CCS = 660: Alcohol-related disorders
ccsMap <- ccsMap %>% 
  mutate(ccscode = ifelse(icd10 %in% alcohol_icd, "660", ccscode))



## Rename columns to their original names
names(ccsMap)[names(ccsMap) == "icd10"] <- "'ICD-10-CM CODE'"
names(ccsMap)[names(ccsMap) == "ccscode"] <- "'CCS CATEGORY'"
names(ccsMap)[names(ccsMap) == "ccscodeDesc"] <- "'CCS CATEGORY DESCRIPTION'"

ccsMap <- ccsMap %>% 
  select(-c(char1:drugOD))

## Output revised CSS mapping
write.xlsx(ccsMap, paste0(ccbUpstream,"upstreamInfo/FUSION-ccs_dx_icd10cm_2019_1-BETA.xlsx"))
