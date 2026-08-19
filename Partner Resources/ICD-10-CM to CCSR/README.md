# Changelog

Please view "changelog.md" to see the history of updates made to the CCB Hospitalization and Emergency Department (ED) Condition List.

# California Community Burden of Disease (CCB) Hospitalization and ED Condition List

- ICD-10-CM Codes

  - For each hospitalization, one condition is established and coded as the chief cause of the admission, and is noted as the **Principal or Primary diagnosis**. Up to 24 other conditions that coexist at the time of admission, that develop subsequently during the hospital stay, or that affect the treatment received are also included and noted as **Other or Secondary diagnoses**.
  - Coding for these Principal and Other diagnoses are based on the ICD-10-CM system (from October 1, 2015 forward; prior to this time ICD-9-CM was used), along with standardized guidance.
  - The codes entered by the hospitals are subject to multiple sources of variation or potential error (e.g. selective use of codes for billing purposes). Nevertheless, since the data are used in the CCB in summary form, the overall patterns displayed are likely to be meaningful and informative.

- Grouping of ICD-10-CM Codes

  - The CCB uses the [Clinical Classifications Software Refined (CCSR)](https://hcup-us.ahrq.gov/toolssoftware/ccsr/dxccsr.jsp) system for mapping ICD-10-CM codes. CCSR is a tool provided by the Agency for Research and Quality Healthcare Cost and Utilization Project. CCSR aggregates more than 70,000 ICD-10-CM diagnosis codes into over 530 clinically meaningful categories across 22 body systems that generally follow chapters within the ICD-10-CM Code Book. CCSR replaced the beta version of the Clinical Classifications Software (CCS) in 2019.

  - ICD-10-CM diagnoses codes are mapped to CCSR categories using the “DX_to_CCSR_Mapping” sheet in the DXCCSR-Reference-File-v2025-1.xlsx file from the Agency for Healthcare Research and Quality (AHRQ) Healthcare Cost & Utilization Project (H-CUP) found [here](https://hcup-us.ahrq.gov/toolssoftware/ccsr/DXCCSR_v2025-1.zip). Hospitalization principal diagnosis ICD-10-CM codes are mapped from the ICD-10-CM code (in column A) to the CCSR code (in column C) if a “Y” is present in the “Inpatient Default” column E. Other, non-primary ICD-10-CM codes are mapped to the corresponding CCSR code if either a “Y” or an “X” is present in the “Inpatient Default” column. For emergency department visits, this same procedure is used based on the “Outpatient Default” column F. In the very rare situation where an improper ICD-10-CM code is used for the principal diagnosis (indicated by the “X” in column E or F), the CCSR values is coded as missing. In a few instances, the names of CCSR categories are shortened so that they be easily displayed in various charts on our CCB dashboard.

  - A “Top Level” that is similar to the one in the condition list for death data was created based on the 22 body systems. Top level conditions include:

    1)  Circulatory diseases
    2)  Infectious diseases
    3)  Injuries
    4)  Mental disorders
    5)  Neoplasms
    6)  Perinatal conditions
    7)  Respiratory diseases
    8)  Symptoms
    9)  Other systems

Please visit the [CCB Technical Documentation page](https://skylab.cdph.ca.gov/communityBurden/?tab=technicaldocumentation) to learn more about the CCB Hospitalization and ED Condition List.

# About this folder

This folder provides resources and R code for mapping ICD-10-CM Hospitalization and ED codes to CCSR conditions. A description of each file is below:

- **DXCCSR-Reference-File-v2025.1.xlsx**:

  - First sheet: Table of Contents

  - Second sheet: Naming_Conventions

    - List of CCSR for ICD-10-CM body systems and the corresponding 3-character CCSR abbreviation for the body system

    - Used in R script to map CCSR categories to a “Top Level” category that is similar to the CCB Death Condition List. These "Top Level" categories were created based on the 22 body systems.

  - Third sheet: CCSR_Categories

    - Searchable list of CCSR categories

    - Not used in R script

  - Fourth sheet: DX_to_CCSR_Mapping

    - Used in R script to map ICD-10-CM codes to CCSR categories.

- **ed_sample.csv**

  - 100,000 ED records containing year, sex, principal diagnosis ICD-10-CM codes, and secondary diagnoses ICD-10-CM codes. These records were randomly sampled from the raw ED data. This data file is read into the R script as an example to show the mapping process.

- **pdd_sample.csv**

  - 100,000 Hospitalization records containing year, sex, principal diagnosis ICD-10-CM codes, and secondary diagnoses ICD-10-CM codes. These records were randomly sampled from the raw hospitalization data. This data file is read into the R script as an example to show the mapping process.

- **Icd10cm-to-CCSR.R**

  - R script showing how to map ICD-10-CM codes to CCSR categories

### *Date Created: August 19, 2026*

### *Last Modified: August 19, 2026*
