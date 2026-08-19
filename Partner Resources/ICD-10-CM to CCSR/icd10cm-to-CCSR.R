## Script for mapping ICD-10-CM in hospitalizations and ED visits data to Clinical Classifications Software Refined (CCSR) categories


library(dplyr)
library(stringr)
library(readxl)
library(readr)
library(ggplot2)



# sample hospitalization data (n = 10,000)
pdd_sample <- read_csv("pdd_sample.csv")

# sample ED visit data (n = 10,000)
ed_sample <- read_csv("ed_sample.csv")


# CCSR mapping for ICD-10-CM ========================================================
ccsr <- read_excel("DXCCSR-Reference-File-v2025-1.xlsx", sheet = "DX_to_CCSR_Mapping", skip = 1) %>% 
  select(icd10cm = `ICD-10-CM Code`, 
         icd10cm_desc = `ICD-10-CM Code Description`, 
         causeCodeCCSR = `CCSR Category`, 
         causeNameCCSR = `CCSR Category Description`, 
         hosp_default = `Inpatient Default CCSR (Y/N/X)`, 
         ed_default = `Outpatient Default CCSR (Y/N/X)`, 
         default_rationale = `Rationale for Default Assignment`)

# CCSR body systems
ccsr_body <- read_excel("DXCCSR-Reference-File-v2025-1.xlsx", sheet = "Naming_Conventions", range = "A2:B25")


# Map CCSR category codes to category description and body system
hospCauseLinkCCSR <- ccsr %>% 
  select(causeCodeCCSR, causeNameCCSR) %>% 
  mutate(topLevCode = str_sub(causeCodeCCSR, 1, 3)) %>% 
  left_join(ccsr_body %>% select(topLevCode = "3-Character Abbreviation", topLevDesc = "CCSR for ICD-10-CM Body System"), by = "topLevCode") %>% 
  mutate(topLevName = case_when(
    topLevCode %in% c("BLD", "DEN", "DIG", "EAR", "END", "EYE", "EXT", "FAC", "GEN", "MUS", "NVS", "SKN") ~ "Other systems", # Consolidate smaller body systems
    topLevCode == "CIR" ~ "Circulatory",
    topLevCode == "INF" ~ "Infectious",
    topLevCode == "INJ" ~ "Injuries",
    topLevCode == "MBD" ~ "Mental",
    topLevCode == "NEO" ~ "Neoplasms",
    topLevCode %in% c("PNL", "MAL") ~ "Perinatal", # Combine Perinatal conditions and Congenital malformations
    topLevCode == "PRG" ~ "Pregnancy",
    topLevCode == "RSP" ~ "Respiratory",
    topLevCode == "SYM" ~ "Symptoms",
    topLevCode == "XXX" ~ "Non-principal"
  ), 
  causeNameShort = case_when( # Shorter CCSR category description for charts
    causeNameCCSR == "Superficial injury; contusion, initial encounter" ~ "Contusion",
    causeNameCCSR == "Complication of genitourinary device, implant or graft, initial encounter" ~ "Complication of genitourinary device/implant/graft",
    causeNameCCSR == "Fracture of the neck of the femur (hip), initial encounter" ~ "Hip fracture",
    grepl(", initial encounter", causeNameCCSR) ~ str_remove(causeNameCCSR, ", initial encounter"), 
    grepl("Other specified and unspecified", causeNameCCSR) ~ str_replace(causeNameCCSR, "Other specified and unspecified", "Other"),
    grepl("Cancer of", causeNameCCSR) ~ str_to_sentence(paste0(str_remove(causeNameCCSR, "Cancer of "), " cancer")), 
    causeNameCCSR == "Pneumonia (except that caused by tuberculosis)" ~ "Pneumonia (not caused by tuberculosis)",
    causeNameCCSR == "Hemolytic jaundice and perinatal jaundice" ~ "Hemolytic/perinatal jaundice",
    causeNameCCSR == "Appendicitis and other appendiceal conditions" ~ "Appendicitis",
    causeNameCCSR == "Schizophrenia spectrum and other psychotic disorders" ~ "Schizophrenia",
    causeNameCCSR == "Diabetes mellitus with complication" ~ "Diabetes mellitus",
    causeNameCCSR == "Skin and subcutaneous tissue infections" ~ "Skin and soft tissue infections",
    causeNameCCSR == "Acute myocardial infarction" ~ "Heart attack",
    causeNameCCSR == "Complications specified during childbirth" ~ "Complications during childbirth",
    causeNameCCSR == "Uncomplicated pregnancy, delivery or puerperium" ~ "Uncomplicated pregnancy",
    causeNameCCSR == "Hypertension and hypertensive-related conditions complicating pregnancy; childbirth; and the puerperium" ~ "Hypertension complicating pregnancy",
    causeNameCCSR == "Spondylopathies/spondyloarthropathy (including infective)" ~ "Spondylopathies, spondyloarthropathy",
    causeNameCCSR == "Respiratory failure; insufficiency; arrest" ~ "Respiratory failure",
    causeNameCCSR == "Other specified upper respiratory infections" ~ "Other upper respiratory infections",
    causeNameCCSR == "Abdominal pain and other digestive/abdomen signs and symptoms" ~ "Abdominal pain",
    causeNameCCSR == "Nonspecific chest pain" ~ "Chest pain",
    causeNameCCSR == "Musculoskeletal pain, not low back pain" ~ "Musculoskeletal pain (not low back)",
    causeNameCCSR == "Other specified upper respiratory infections" ~ "Other upper respiratory infections",
    TRUE ~ causeNameCCSR
  )) %>% 
  filter(!is.na(causeCodeCCSR)) %>% 
  distinct() %>% 
  arrange(causeCodeCCSR) %>% 
  relocate(causeNameShort, .after = causeNameCCSR)



# Map principal and other diagnoses for hosp/ED ========================================================
## default == Y: default CCSR categories
## default == N: non-default CCSR categories when one ICD-10-CM is mapped to multiple CCSR categories
## default == X: unacceptable ICD-10-CM for principal diagnosis in hospitalization or first listed diagnosis in ED visit

hospCauseLinkCCSR1 <-  hospCauseLinkCCSR %>% select(causeCodeCCSR, topLevCCSR = topLevName)


# map principal diagnosis for hosp - only include hosp_default == Y
ccsr_hosp_prin <- ccsr %>% 
  filter(hosp_default == "Y") %>% 
  left_join(hospCauseLinkCCSR1, by = "causeCodeCCSR")

# map other diagnoses for hosp - include hosp_default == Y or X
ccsr_hosp_other <- ccsr %>% 
  filter(hosp_default == "Y" | hosp_default == "X") %>% 
  left_join(hospCauseLinkCCSR1, by = "causeCodeCCSR")

# map principal diagnosis for ED - only include ed_default == Y  
ccsr_ed_prin <- ccsr %>% 
  filter(ed_default == "Y") %>% 
  left_join(hospCauseLinkCCSR1, by = "causeCodeCCSR")

# map other diagnoses for ED - include ed_default == Y or X
ccsr_ed_other <- ccsr %>% 
  filter(ed_default == "Y" | ed_default == "X") %>% 
  left_join(hospCauseLinkCCSR1, by = "causeCodeCCSR")


hosp_lookup_prin  <- setNames(ccsr_hosp_prin$causeCodeCCSR, ccsr_hosp_prin$icd10cm)
hosp_lookup_other <- setNames(ccsr_hosp_other$causeCodeCCSR, ccsr_hosp_other$icd10cm)
ed_lookup_prin    <- setNames(ccsr_ed_prin$causeCodeCCSR, ccsr_ed_prin$icd10cm)
ed_lookup_other   <- setNames(ccsr_ed_other$causeCodeCCSR, ccsr_ed_other$icd10cm)


pdd_mapped <- pdd_sample %>% 
  mutate(causeCode = hosp_lookup_prin[diag_p]) %>% # map ICD-10-CM in principal diagnosis
  mutate(across(starts_with("odiag"), ~ hosp_lookup_other[.], .names = "ccsr_{col}")) # map ICD-10-CM in all other diagnoses


ed_mapped <- ed_sample %>% 
  mutate(causeCode = ed_lookup_prin[diag_p]) %>% 
  mutate(across(starts_with("odiag"), ~ ed_lookup_other[.], .names = "ccsr_{col}"))


# Count the number of hospitalization by CCSR categories based on principal diagnosis ========================================================
pdd_ccsr <- pdd_mapped %>% 
  left_join(hospCauseLinkCCSR %>% select(causeCode = causeCodeCCSR, causeNameCCSR, causeNameShort, topLevName), by = "causeCode") %>% 
  filter(!is.na(causeCode)) %>% 
  count(causeCode, causeNameCCSR, causeNameShort, topLevName, name = "n_hosp")

# Count the number of ED visit by CCSR categories based on principal diagnosis 
ed_ccsr <- ed_mapped %>% 
  left_join(hospCauseLinkCCSR %>% select(causeCode = causeCodeCCSR, causeNameCCSR, causeNameShort, topLevName), by = "causeCode") %>% 
  filter(!is.na(causeCode)) %>% 
  count(causeCode, causeNameCCSR, causeNameShort, topLevName, name = "n_ed")


# Create bar chart: Top 10 Causes based on number of hospitalizations ========================================================
pdd_ccsr %>% 
  slice_max(n_hosp, n = 10) %>%  # Filter on causes in top 10 for number of hospitalization
  ggplot(aes(x = n_hosp, y = reorder(causeNameShort, n_hosp))) + 
  geom_col(fill = "blue") +
  labs(x = "Number of Hospitalizations", y = "Cause of Hospitalization", title = "Top 10 Causes based on Number of Hospitalizations")


