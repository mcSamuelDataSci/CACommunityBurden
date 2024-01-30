**TECHNICAL DOCUMENTATION**  

- [Notes](#notes)  
    -   [Note Regarding Data De-Identification](#note-data-deidentification)  
    -   [Note Regarding Year or Year Group](#note-year-group)    
    -   [Note Regarding "Race and Ethnicity" for Death Data](#note-race-and-ethnicity-death)    
    -   [Note Regarding "Race and Ethnicity" for Hospitalization and Emergency Department Data](#note-race-and-ethnicity-hospitalization-ed)    
    -   [Note Regarding "Communities"](#note-communities)  
- [Data Sources and Other Key Inputs:](#data-sources)  
    -   [Death Data](#death-data)  
    -   [Hospitalizations and Emergency Department Data](#hospitalization-and-ed-data)  
    -   [Population Data](#population-data)  
    -   [Social Determinants of Health (SDOH)](#sdoh)  
- [Condition List and ICD-10 Mapping for Deaths:](#icd-10)   
    -   [The Structure of the Condition List](#condition-list-structure)    
    -   [Overall Mapping of the Condition List](#mapping-of-condition-list)
    -   [Modifications to Specific Conditions](#modifications-icd-10)  
        -   [Alzheimer's Disease](#alzheimers)
        -   [Drug Overdose](#drug-overdose)
        -   [Alcohol-related Conditions](#alcohol-related)
- [Condition List and ICD-10-CM Mapping for Hospitalizations and Emergency Department Visits:](#icd-10-cm)  
    -   [ICD-10-CM Codes](#icd-10-cm-cdoes)  
    -   [Grouping of ICD-10-CM Codes](#icd-10-cm-grouping)  
    -   [Modifications to Specific Conditions](#modifications-icd-10-cm)  
- [Geography/GIS (State, County, Census Tract) Issues](#geography-issues)  
    -   [State and County Designation in Death Data](#state-county-designation)  
    -   [Boundary Files](#boundary-files)  
    -   [Census Tract Data Issues](#census-tract-issues)    
- [Formulas and Measures](#formulas-and-measures)  
    -   [Years of Life Lost (YLL)](#years-of-life-lost)  
    -   [Crude Rates](#crude-rates)  
    -   [Age Adjusted Rates](#age-adjusted-rates)  
    -   [Life Expectancy](#life-expectancy)  

<br>

**Notes** <a id="notes"></a>
    
-   Note Regarding Data De-Identification: <a id="note-data-deidentification"></a>
    -   A number of steps have been taken to address data security and confidentiality concerns, including 1) aggregation of year into 5-year groups for data displayed at the community and census tract level, 2) showing less granular cause of death data at more granular geographic levels, 3) suppressing all measures for “strata” or “cells” where the corresponding number of deaths is <11, 4) for gender, age group, race and ethnicity, and educational attainment stratified data, if one “cell” within a strata is suppressed per #3, at least one complementary “cell” is suppressed to avoid arithmetic back-calculation of the suppressed cell.  Data at the county level or below, for race and ethnicity, and age group, are aggregated in three calendar-year groups. These procedures assure compliance with the California Health and Human Services Agency [Data De-Identification Guidelines (DDG)](https://www.dhcs.ca.gov/dataandstats/Documents/DHCS-DDG-V2.0-120116.pdf).

<br>

-   Note Regarding Year or Year Group: <a id="note-year-group"></a>
    -   At the County and State levels of geography, YEAR is the individual year of death, with data from 2000 to the current year, 2022. At the Community and Census Tract levels of geography, all data are displayed for the years 2018 to 2022 combined. These years are combined for statistical stability, so that for these more granular levels of geography, the displayed data are still meaningful, and not the result of random fluctuations.

<br>

-   Note Regarding **"Race and Ethnicity"** for Death Data: <a id="note-race-and-ethnicity-death"></a>
    -   Race and Ethnicity is shown in the CCB as one combined variable, with mutually exclusive values, in hierarchical order of:
        -   If a person is listed as “Latino” or “Hispanic” in the original data source, they are shown in the CCB as “Latino” *regardless of any other designation of race*.
        -   For all *other* persons, if they are listed in the original data source with more than one racial group, they are shown in the CCB as “Multi-race”.
            -   For example, if they are listed in the original data source as “Black” and as “Asian” they will be shown in the CCB as “Multi-race”.
            -   If they are listed in the original data source with more than one sub-racial/geographic group *within* one racial group, they will be shown in the CCB in the racial group, not as “Multi-race”. For example, a person listed in the original data source as Japanese and as Chinese would be shown in the CCB as “Asian”, not “Multi-race”.
            -   If a person is listed as “Other” race AND as one other single race, they will be shown in the CCB as that single race group, not as “multi-race”.
        -   All *other* persons (i.e. not Latino and not multi-race) will be shown in the CCB in a single-race.   
    

            The following labeling is used for these race/ethnic groups:
            
            | Race/Ethnicity Name                        | Abbreviations |
            |:-------------------------------------------|:--------------|
            | American Indian or Alaska Native           | AI/AN         |
            | Black                                      | Black         |
            | Asian                                      | Asian         |
            | Latino                                     | Latino        |
            | Native Hawaiian and other Pacific Islander | NH/PI         |
            | White                                      | White         |
            | Multi-Race                                 | Multi-Race    |
            | Other                                      | Other         |
            | Unknown                                    | Unknown       |


<br>

-   Note Regarding **“Race and Ethnicity”** for Hospitalization and Emergency Department Data: <a id="note-race-and-ethnicity-hospitalization-ed"></a>
    -   Coding and labeling of race and ethnicity for hospitalization and emergency department data largely follows the same approach as for death data described above.
    -   However, prior to 2019 the “Native Hawaiian and other Pacific Islander” and “Multi-Race” categories were not included in those data–presumably such persons would have been included in the “Asian” or “Other” categories as appropriate.

<br>

-   Note Regarding “Communities”: <a id="note-communities"></a>
    -   Throughout the CCB, communities are defined by [Medical Service Study Areas (MSSAs)](https://gis.data.ca.gov/datasets/559dc5a04ae0499ebfec8f0c774703d9_0/about), a unique California geographic designation based on aggregation of census tracts, constructed by the California Department of Health Care Access and Information (HCAI), with each decennial census. MSSAs provide the CCB with a good surrogate for “communities” because:
        -   there are 542 MSSAs for the 2010 census, providing much more geographic granularity than the 58 California counties and much greater numerical/statistical stability than the 8000+ California 2010 census tracts,
        -   in general, they are aligned with “communities” in the important sense of geographic, cultural, and sociodemographic similarities (although this is generally more true for urban than rural MSSAs, because of the larger size of MSSAs in rural areas),
        -   the names associated with each MSSA have some resonance in many cases with local ideas of “community.”
        -   Although not yet implemented in a fully automated fashion, users can work with the CCB project team to create their own customized communities (based on designated census tracts) for incorporation into the CCB.

<br>

<hr>

**Data Sources and Other Key Inputs:** <a id="data-sources"></a>
    
-   Death Data <a id="death-data"></a>
    -   From the California Integrated Vital Records System [(CalIVRS)](https://www.cdph.ca.gov/Programs/CHSI/Pages/Data-Applications.aspx), based on death certificates/reports processed to the California Department of Public Health, Center for Health Statistics and Informatics (CHSI).
        -   Files used: “Death Static Master Files (DSMF)” for 2000 to 2004 and “California Comprehensive Death Files (CCDF)” for 2005 to the current year.
    -   Because CCDF Files were used for 2005 to the current year, deaths of California residents that occurred and were recorded OUTSIDE of California, those years have NOT yet been incorporated into any of the CCB working data, visualizations or tables.
    -   A death record was considered to be of a California resident based on field “71, Residence State/Province” for the most recent data and on field “46 State of Residence” for 2001-2004 data. A tiny fraction of these records geocoded to locations outside of California, and others had anomalies suggesting the possibility that the residence was not in California. However, the number of such anomalies is relatively minuscule, such that they are extraordinarily unlikely to have any impact on observed patterns and trends.
    -   County was based on field “62, Decedent’s County of Residence Based on City/State (NCHS Code)” for 2011 to the current year’s data and on field “35, Place of Decedent’s Residence” for 2001-2004 data except when modified as noted in “Census Tract Data Issues” below.
    -   California death data are geocoded using the CDPH geocoding service, which uses StreetMap Premium for ArcGIS. We have not determined if there is a confidence score or match score below which the census tract for an address is not provided. For 2011to the current year where the CCB uses these data to determine census tract (and therefore communities), a high percentage of records geocoded to a valid census tract (96.4% to 97.2%). The remaining records contained invalid addresses and/or other anomalies. While this overall rate of geocoding is high, there is substantial variation in the geocoding percent between counties, and some counties (particularly some rural counties) have geocoding rates as much as half of those noted above.
    -   Note: As with almost all public health and administrative data used to assess population patterns and trends in morbidity and mortality, vital statics death data are **not error free**. “Data quality issues inherent in death certificate data most often stem from errors related to the fact that the **responsibility** for providing complete and accurate information **lies with the informant and certifier**. This may impact classification of deaths and consequently under- or over-estimate  mortality.”

<br>

-   Hospitalizations and Emergency Department Data <a id="hospitalization-and-ed-data"></a>
    -   Hospitalization Data
        -   Hospitalization data are based on 2020 to 2022 nonpublic Patient Discharge Data received from CDPH’s Center for Health Statistics and Informatics [(CHSI)](https://www.cdph.ca.gov/Programs/CHSI/Pages/Program-Landing1.aspx), as provided to CHSI by the California Department of Health Care Access and Information [(HCAI)](https://hcai.ca.gov/). HCAI provides such files from inpatient data they collect from California-licensed hospitals. The data set consists of a record for each inpatient discharged from a California-licensed hospital. Licensed hospitals include general acute care, acute psychiatric, chemical dependency recovery, and psychiatric health facilities. Data are not collected from Veteran’s Administration, Military or other Federal Hospitals or from Tribal Hospitals.   
        -   Detailed information for the current HCAI Patient Discharge Data and data system can be found [here](https://hcai.ca.gov/wp-content/uploads/2023/10/PDD_DataDictionary_2022_Final-ADA.pdf).  
    

    -   Hospitalization Charges
        -   For each hospitalization one total charge is listed, reflecting the charges associated with the primary condition as well as any other charge associated with the hospitalization.
            -   Total charge is all charges for services rendered during the length of stay for patient care at the facility, based on the hospital’s full established rates. Charges include, but are not limited to daily hospital services, ancillary services, and patient care services.
            -   Hospital-based physician fees are excluded, as are items like take-home drugs, television, follow-up home health visits, ambulance services, etc.
            -   Hospitals report ‘Total Charges’ to HCAI for the last 365 days of stay. However, starting in 2015, in the files released by HCAI ‘Total Charges’ are adjusted to reflect the entire length of stay.
            -   A hospitalization can have multiple discharges if the patient moves between type of care (e.g. psychiatric to acute would be two discharges during the same overall hospital stay) and each discharge has a related total charge.
            -   Charges of $1 specify ‘no charge’ or charity care.
        -   The noted **charges** are based on hospitals’ administrative systems and **do not indicate actual costs/payments for those charges**.
        -   Nevertheless, because the CCB describes **summary data**, the charts and tables shown provide **valuable information regarding the patterns of the monetary burden** of disease/conditions in California from the hospitalization perspective.
        -   For some hospitalizations, no charges are reported to HCAI, and for some hospitalizations implausibly high charges (likely errors) have been excluded, so total charges may be slight underestimates from this perspective. ‘Average’ charges in these charts are based on the median rather than the mean, so are largely not impacted by these issues.  
        
    -   Emergency Department Data
        -   Emergency Department data are based on 2020 to 2022 nonpublic Emergency Department data received from CDPH’s Center for Health Statistics and Informatics (CHSI), and originally provided by the California Department of Health Care Access and Information (HCAI). HCAI provides such files from emergency care data collected from hospital emergency departments and also of ambulatory surgery data collected from general acute care hospitals and licensed freestanding ambulatory surgery clinics in California. Each record within the data set consists of one outpatient encounter, also known as a service visit. Data collected for these encounters include demographic, clinical, payer, and facility information. 
        -   Detailed information for the current HCAI Emergency Department Data and data system can be found [here](https://hcai.ca.gov/wp-content/uploads/2023/10/EDAS_DataDictionary_2022_Final-ADA.pdf).

<br>

-   Population Data <a id="population-data"></a>
    -   For **counties**, all population denominator data are from the California Department of Finance (DOF).    
        -   2000-2009 data are [California and Counties Population by Age, Race/Hispanics, and Gender: 2000–2010](https://dof.ca.gov/forecasting/demographics/estimates/annual-intercensal-population-estimates-by-race-ethnicity-with-age-and-gender-detail/) estimates    
        -   2010-2022 data are Vintage 2020 [Complete State and County Projections](https://www.dof.ca.gov/forecasting/demographics/Projections/) (Table P-3; released 2021.7.14)
            -   This file is no longer available for download on DOF’s website. For a copy of this file, please contact [CCB](mailto:ccb@cdph.ca.gov).
    -   For **census tracts** (and therefore communities) population denominator data are based on the 2015-2019 [American Community Survey](https://www.census.gov/programs-surveys/acs/guidance.html) 5-year extracts (tables B01001_001E, B01001_002E, and B01001_026E) Community population data are generated by aggregating these census data up to the community level.
        -   Due to the changed boundaries between the 2010 and 2020 Census boundaries and the death data analyzed in the CCB still being geocoded to 2010 census tract boundaries, more recent ACS 5-year data are not being used at this time. Soon, tract of residence information using the new 2020 boundaries will be available. Once 2020 census tract data are  available in the death data, the CCB will be updated to use the most recent ACS data.    
            -   In general, due to the ACS data release schedule where data for the “current year” are not always available, population data for the “prior year” will be used as denominators for the “current year” until true “current year” population data are available.
        -   ACS data are extracted directly from the Census/ACS API (Application Program Interface) using the [R tidycensus package](https://walkerke.github.io/tidycensus/).

<br>

-   Social Determinants of Health (SDOH) <a id="sdoh"></a>
    -   SDOH are the conditions in which people are born, grow, live, work, and age, including the health system. These circumstances are shaped by the distribution of money, power and resources at global, national and local levels. 	
    -   The CCB currently contains an exploratory set of SDOH variables extracted from the American Community Survey using the R tidycensus package,  the California Healthy Places Index (HPI) via their API, and other data sources.

<br>

<hr>

**Condition List and ICD-10 Mapping for Deaths:** <a id="icd-10"></a>

-   The Structure of the Condition List <a id="condition-list-structure"></a>
    -   The “California Community Burden of Disease Condition List” is the hierarchical list of grouped causes of death, or “conditions”, that can be examined and explored in the CCB.
    -   The Condition List has three “levels”: The “Top Level”, the “Public Health Level”, and the “Detail Level”
        -   The “Top Level” of the Condition list includes 6 broad mutually exclusive and exhaustive groups:
            1)	Communicable, maternal, and nutritional conditions
            2)	Cancer/Malignant neoplasms
            3)	Cardiovascular diseases
            4)	Other Chronic Conditions
            5)	Injuries
            6)	Perinatal Conditions
            -   For data displayed at the census tract level, only the Top Level is included due to sample size and statistical reliability limitations.
        -   The next level, the “Public Health” level, splits the top level into around 75 mutually exclusive and exhaustive conditions. These conditions are based on meaningful public health programmatic groupings and all have large enough numbers for useful exploration.        
            -   this is the default level for most charts in the CCB and for data/maps displayed at the community level.
        -   The final “Detail Level” includes several conditions of special interest or significance, but for which the numbers are insufficient or inappropriate to include as a unique “Public Health Level” category.
            -   Detailed Level conditions are available for the State and for counties, but not for communities or census tracts.
        -   All three levels combined include about 100 categories.
    -   For some conditions for some charts an abbreviated label is used. For most this is unambiguous, but for a couple additional detail may be useful:
        -   “Alzheimer’s disease” = “Alzheimer disease and other dementias”
        -   “Lung Cancer” = “Trachea, bronchus and lung cancers”
    -   A diagram of the Conditions List is [here](https://skylab.cdph.ca.gov/communityBurden/_w_260397a5/icd10_to_condition_IMAGE.pdf), and shows the current structure, the levels, and all the conditions therein.
    -   Documentation of the mapping of all ICD-10 codes to this Condition List and notation of all modifications to the original WHO system are documented and described in the CCB resource [here](https://rst4-01.cdph.ca.gov/s/a244a4d32e2df7ef520fb/rmd_output/1/icd10_to_CAUSE.xlsx).
    -   The Condition List is regularly updated and refined based on input from partners and on emerging priority conditions. We welcome your input on the condition list by emailing [ccb@cdph.ca.gov](mailto:ccb@cdph.ca.gov)

   
<br>
  
-   Overall Mapping of the Condition List <a id="mapping-of-condition-list"></a>    
    -   Conditions in the Condition List are based on aggregations of individual International Classification of Diseases version 10 (ICD-10) cause of death codes. The Condition List aggregation system was developed sequentially based on:
        -   The starting point was the ICD10-to-condition mapping from the “WHO methods and data sources for global burden of disease estimates 2000-2015, January 2017”, pages 24-29 “Annex Table A GHE cause categories and ICD-10 codes”.  While a web link to this original mapping is no longer available, current highly similar work and methods are available [here](https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&cad=rja&uact=8&ved=2ahUKEwichPv8-PGDAxWMJEQIHTHGC0oQFnoECAYQAQ&url=https%3A%2F%2Fcdn.who.int%2Fmedia%2Fdocs%2Fdefault-source%2Fgho-documents%2Fglobal-health-estimates%2Fghe2019_daly-methods.pdf&usg=AOvVaw39uaf0zckmfffL7ZFt2Wkk&opi=89978449).
        -   Then, refinements were made based on comparing the mapping of all California deaths (about 4 million) from 2000 to 2015 using the original WHO system to a more recent and more detailed mapping system developed by the Institute for Health Metrics and Evaluation (IHME) at the University of Washington. IHME shared this mapping system with the CCB team in the form of an Excel spreadsheet with over 41,000 rows, each with a unique individual ICD-10 code mapped to a hierarchal list of cause codes. (For reference, a similar IHME mapping, but with row-based summary lists of ICD-10 codes can be found on the “Files (3)” tab of the IHME webpage [here](https://ghdx.healthdata.org/record/ihme-data/gbd-2019-cause-icd-code-mappings). For many ICD-10 codes and/or categories, the IHME system appeared to be more accurate, more detailed, or otherwise more useful for California than the WHO system, in which case such changes were made to the CCB Condition List mapping.    
            -   We could not use this IHME system as the primary basis for the CCB Condition List because the IHME spreadsheet mapping resulted in 721,783 (19.2%) of California deaths from 2000 to 2015 being mapped to “garbage codes”, for which more sophisticated methods would need to be employed. The possibility of redistributing these “garbage codes” to valid categories could be explored with additional resources.
            -   Relevant recent IHME publications include: 
                -   Latest IHME/GBD results and methods: [Global burden of 369 diseases and injuries in 204 countries and territories, 1990–2019: a systematic analysis for the Global Burden of Disease Study 2019](https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(20)30925-9/fulltext)
                -   US Burden of Disease: [The State of US Health, 1990-2016 Burden of Diseases, Injuries, and Risk Factors Among US States, JAMA 2018](https://jamanetwork.com/journals/jama/fullarticle/2678018)
                -   US County Level Trends: [US County-Level Trends in Mortality Rates for Major Causes of Death, 1980-2014, JAMA 2016](https://jamanetwork.com/journals/jama/fullarticle/2592499).
    -   In order to make the CCB Condition List more meaningful for public health, conditions that were previously in catch-all categories (e.g. “Other Infectious Diseases/Nutritional Deficiencies”, “Other or unspecified cardiovascular diseases”, etc.) were made their own Public Health Level condition if:
        -   The condition was listed as the primary (or underlying) cause of death in more than 1,000 death records in 2021, or
        -   The condition was listed as the secondary (or contributory) cause of death in more than 5,000 death records in 2021.
        -   For example, “Urinary Tract Infections” was moved out of “Other Chronic Conditions” and is now a Public Health Level condition since it was the primary cause for 1,366 deaths in California in 2021.
    -   The hierarchical “outline” structure was further modified and rearranged in other ways to enhance useability/readability including by, for example, removing infrequently occurring “high level” categories in the list (e.g. “Intestinal nematode infections”; “sense organ diseases”) and moving any of their contents to corresponding general/generic categories (e.g., respectively, “Other infectious diseases”, “Other Chronic Conditions”)

<br>

-   Modifications to Specific Conditions <a id="modifications-icd-10"></a>
    -   The Condition List was then modified for several conditions, with input from CDPH subject matter experts, to make the conditions more consistent with California public health programmatic areas and/or priority or emerging public health conditions. Modifications include:
        -   Elevating “Congestive Health Failure” from “Other Cardiovascular disease” to a unique condition within the “Cardiovascular Disease” group
        -   Eliminating the “Respiratory infections” category, with its two subcategories of “Lower respiratory infections” and “Upper respiratory infections”; in favor of the unique conditions of “influenza”, “Pneumonia”, and, as of 2020, “COVID-19”, with the very few remaining respiratory infections included in the catch-all “Other Infectious Diseases/Nutritional Deficiencies” condition.
        -   <a id="alzheimers"></a>In order to distinguish between deaths caused by the “Public Health Level” condition “Alzheimer’s disease and other dementias”, after consulting with clinicians and referencing [a research paper](https://pubmed.ncbi.nlm.nih.gov/31112120/), we have created the following “Detail Level” subgroups:
            -   Alzheimer’s
                -   G30.0, G30.1, G30.8 and G30.9, all of which specifically note Alzheimer’s
            -   Unspecified Dementia
                -   F03: Unspecified dementia  
                -   G31.1: Senile degeneration of brain, not elsewhere classified  
                -   G31.8: Other specified degenerative diseases of nervous system  
                -   G31.9: Degenerative disease of nervous system, unspecified  
            -   Vascular Dementia
                -   F01.0: Vascular dementia of acute onset
                -   F01.1: Multi-infarct dementia
                -   F01.3: Mixed cortical and subcortical vascular dementia
                -   F01.8: Other vascular dementia
                -   F01.9: Vascular dementia, unspecified
            -   Note: Frontotemporal Dementia (G31.0) is not included as a “Detailed Level” group, because there are few deaths from this cause, but is included in the total “Public Health Level” condition “Alzheimer’s disease and other dementias”
        -   <a id="drug-overdose"></a>In order to make drug- and poisoning-related conditions in the CCB as meaningful as possible for public health, and to maintain the condition list as mutually exclusive and exhaustive, we have modified these conditions from WHO and IHME standards based on discussion with the CDPH Substance and Addition Prevention Branch (SAPB) and on the CDC [“Consensus Recommendations for National and State Poisoning Surveillance – ISW7 2012”](https://cdn.ymaws.com/www.cste.org/resource/resmgr/injury/isw7.pdf).
            -   The “Drug overdose (poisoning/substance use disorders)” condition includes “accidental poisonings by drugs” codes (X40-X44) **and** “substance use disorder codes” (F11-F16, F18, F19), but not alcohol use disorder (F10) which is included in the separate detailed level “Alcohol use disorders” condition. This conditions also includes “newborn (suspected to be) affected by maternal use of drugs of addiction” (P044).
            -   “Drug overdose (poisoning/substance use disorders)” **does not include**:
                -   “Intentional self-poisoning by drugs” (X60-X64), which is included in the “Suicide” condition
                -   “Assault by drug poisoning” (X85), which is included in the “Homicide” condition
                -   “Drug poisoning of undermined intent” (Y10-Y16), which is included in the “Injuries of unknown intent” condition
            -   The separate “Poisonings (non-drug)” conditions includes poisoning with non-drug-related substances (X46-X49).
        -   <a id="alcohol-related"></a>In order to make alcohol-related conditions in the CCB as meaningful as possible for public health, and to maintain the condition list as mutually exclusive and exhaustive, we have modified these conditions from WHO and IHME standards based on discussion with the CDPH Injury and Violence Prevention Branch (IVPB) and on the CDC [Alcohol-Related Disease Impact (ARDI)](https://www.cdc.gov/alcohol/ardi/alcohol-related-icd-codes.html) ICD-10 codes (using 100% Alcohol-attributable code only).
            -   “Alcohol-related conditions” is under the broad “Injury” condition group, and includes alcoholic liver disease codes, Alcoholic psychosis, Alcohol abuse, Alcohol dependence syndrome, Alcohol poisoning, and a number of other infrequent conditions included in the list below.
            -   Because the majority of alcohol-related deaths are due to “alcoholic liver disease”, and because of the utility of looking at “alcoholic liver disease” in relation to other liver disease deaths, there are two detail level causes listed under Alcohol-related deaths:
                -   Alcoholic liver disease
                -   Other alcohol-related
            -   The table below shows the full list of causes (and their corresponding ICD-10 codes) that make up “alcohol-related conditions”:


                
                | Cause                                                               | ICD-10             |
                |:--------------------------------------------------------------------|:-------------------|
                | Alcoholic psychosis                                                 | F10.3-F10.9        |
                | Alcohol abuse                                                       | F10.0, F10.1       |
                | Alcohol dependence syndrome                                         | F10.2              |
                | Alcohol polyneuropathy                                              | G62.1              |
                | Degeneration of nervous system due to alcohol                       | G31.2              |
                | Alcoholic myopathy                                                  | G72.1              |
                | Alcohol cardiomyopathy                                              | I42.6              |
                | Alcoholic gastritis                                                 | K29.2              |
                | Alcoholic liver disease                                             | K70.0-K70.4, K70.9 |
                | Alcohol-induced acute pancreatitis                                  | K85.2              |
                | Alcohol-induced chronic pancreatitis                                | K86.0              |
                | Fetal alcohol syndrome                                              | Q86.0              |
                | Fetus and newborn affected by maternal use of alcohol               | P04.3              |
                | Alcohol poisoning                                                   | X45, Y15           |
                | Evidence of alcohol involvement determined by level of intoxication | Y91                |

    
<br>

<hr>

**Condition List and ICD-10-CM Mapping for Hospitalizations and Emergency Department Visits:** <a id="icd-10-cm"></a>

-   ICD-10-CM Codes <a id="icd-10-cm-cdoes"></a>
    -   For each hospitalization, one condition is established and coded as the chief cause of the admission, and is noted as the **Principal or Primary diagnosis**. Up to 24 other conditions that coexist at the time of admission, that develop subsequently during the hospital stay, or that affect the treatment received are also included and noted as **Other or Secondary diagnoses**.
    -   Coding for these Principal and Other diagnoses are based on the ICD-10-CM system (from October 1, 2015 forward; prior to this time ICD-9-CM was used), along with standardized guidance.
    -   The codes entered by the hospitals are subject to multiple sources of variation or potential error (e.g. selective use of codes for billing purposes). Nevertheless, since the data are used in the CCB in summary form, the overall patterns displayed are likely to be meaningful and informative.

<br>

-   Grouping of ICD-10-CM Codes <a id="icd-10-cm-grouping"></a>
    -   ICD-10-CM codes are highly detailed and specific, with more than 93,000 codes. There are many ways these codes can be grouped or summarized into meaningful categories (e.g., the Global Burden of Disease system (GBD), the Major Diagnostic Categories (MDC) and the Medicare Severity Diagnosis Related Group (MS-DRG)). At this time, we use the [Clinical Classifications Software (CCS)](https://www.hcup-us.ahrq.gov/tools_software.jsp) system for mapping ICD-10-CM codes. CCS is a tool provided by the Agency for Research and Quality Healthcare Cost and Utilization Project. CCS aggregates the ICD codes into a manageable number (285) of clinically meaningful categories to make it easier to quickly understand diagnosis patterns. The system is evolving, with the current system organized across 21 body systems, which generally follow the structure of the ICD-10-CM diagnosis chapters. CSS for ICD-10-CM was initially released as a beta version in 2015, where each ICD-10-CM code maps uniquely to one of 283 categories. The beta version was later replaced by the Clinical Classifications Software Refined (CCSR) in 2019. In the CCSR, each ICD-10-CM can be mapped to more than one category. Allowing individual ICD-10-CM codes to be mapped to multiple CSSR categories is intended to better match ICD-10-CM codes with the clinical intent of CSSR categories. This change helps address the fact that ICD-10-CM codes sometimes document (1) multiple conditions, or (2) a condition and a common symptom or manifestation. All ICD-10-CM are mapped to one of 520 categories in the “first” category. Archives for the CCSR and CCS beta versions can be found [here](https://hcup-us.ahrq.gov/toolssoftware/ccsr/ccsr_archive.jsp#ccsr). Because the CCB condition list for hospitalizations and Emergency Department visits must be mutually exclusive and exhaustive, the beta version of CCS is used for mapping ICD-10-CM codes. 

<br>

-   Modifications to Specific Conditions <a id="modifications-icd-10-cm"></a>
    -   The CSS list was then modified for several conditions, with input from CDPH subject matter experts, to make the conditions more consistent with California public health programmatic areas and/or priority or emerging public health conditions. Modifications include:
        -   In order to make drug overdose ED visits and hospitalizations in the CCB as meaningful as possible for public health, we used the definition of "All Drugs" ED visits and hospitalizations from the [California Overdose Surveillance Dashboard](https://skylab.cdph.ca.gov/ODdash/?tab=Home) as a starting point, and made some modifications to it, based on careful review and on the CCB need for the condition list to be mutually exclusive and exhaustive. 
        -   As with the California Overdose Surveillance Dashboard:
            -   Accidental- and undetermined-intent drug poisoning are included in “Drug overdose”.  
            -   Unlike the California Overdose Surveillance Dashboard:  
                -   Subsequent encounters and sequela are included in “Drug overdose” in addition to initial encounters, as CCB is record-based rather than individual-based. 
                -   Intentional self-harm and assault are not included in “Drug overdose”. They are included in "Suicide and intentional self-inflicted injury" and "Assault by drug", respectively. 
        -   Starting from the CCS system, we expanded the definition of alcohol-related disorders based on CDC [Alcohol-Related Disease Impact (ARDI)](https://www.cdc.gov/alcohol/ardi/alcohol-related-icd-codes.html) ICD-10-CM codes (100% alcohol-attributable chronic causes) to map alcohol-related disorders ED visits and hospitalizations.  
        -   The table below shows the full list of causes and their corresponding ICD-10-CM codes that make up "alcohol-related disorders".  
        
            | Cause                                                               | ICD-10             |
            |:--------------------------------------------------------------------|:-------------------|
            | Alcoholic psychosis                                                 | F10.9              |
            | Alcohol abuse                                                       | F10.1              |
            | Alcohol dependence syndrome                                         | F10.2              |
            | Alcohol polyneuropathy                                              | G62.1              |
            | Degeneration of nervous system due to alcohol                       | G31.2              |
            | Alcoholic myopathy                                                  | G72.1              |
            | Alcohol cardiomyopathy                                              | I42.6              |
            | Alcoholic gastritis                                                 | K29.2              |
            | Alcoholic liver disease                                             | K70.0-K70.4, K70.9 |
            | Alcohol-induced acute pancreatitis                                  | K85.2              |
            | Alcohol-induced chronic pancreatitis                                | K86.0              |
            | Fetal alcohol syndrome                                              | Q86.0              |
            | Fetus and newborn affected by maternal use of alcohol               | P04.3              |
            | Alcohol use complicating pregnancy, childbirth, and the puerperium  | O99.31             |

<br>

<hr>

**Geography/GIS (State, County, Census Tract) Issues** <a id="geography-issues"></a>  
  
-   State and County Designation in Death Data <a id="state-county-designation"></a>
    -   As of September 2022, the decedent’s state and county of residence are based on the method recommended by the VSB Assessment and Policy Section (VSB-APS) within CDPH’s Center for Health Statistics and Informatics (CHSI):
        -   For death records assigned a census tract based on a geocoded residence address:
            -   Positions 1-2 of the census tract ID correspond to state FIPS code, with the value “06” representing California. This is used to identify California residents. 
            -   Positions 3-5 of the census tract ID correspond to county FIPS code, and is used to assign county of residence.
        -   For death records *without* a known census tract:
            -   California residents are identified using the “STATE RESIDENCE” field with the value “CA”.
            -   County of residence is assigned using the “FINAL DECEDENT'S COUNTY OF RESIDENCE BASED ON GEOCODE (NCHS CODE)” field which contains county FIPS codes. 

<br>

-   Boundary Files <a id="boundary-files"></a>
    -   Boundary (or “shape”) files for the CCB were generated using the tracts() function of the [R tigris package](https://github.com/walkerke/tigris), and with removal of islands off the west coast of some counties using a custom island removal function.                

<br>

-   Census Tract Data Issues  <a id="census-tract-issues"></a>
    -   Of the 8041 California census tracts in the 2010 Census “Tiger” files, five have zero land area (only water), and (not surprisingly…) have zero population and zero deaths. These tracts are excluded from CCB data processing and display.
    -   Of the remaining 8036 tracts, 12 have zero population and zero deaths. By definition, these tracts are excluded from all analyses, and will show values of “0” or missing for all measures in all maps. These tracts are wholly comprised of industrial facilities, airports, or parks.
        -   Census tract issues to be addressed:
            -   Census tracts (and communities) where greater than 50 percent of the population live in congregant living quarters will be noted in the future with an “*” on relevant maps and charts in an upcoming CCB release. For some comparisons (e.g. of rates) these tracts could be removed from the larger geographies in which they are contained.
            
<br>

<hr>

**Formulas and Measures** <a id="formulas-and-measures"></a>  

-   Years of Life Lost (YLL) <a id="years-of-life-lost"></a>  
    -   Two methods are available for calculating YLL.
    -   In the current implementation of the CCB, we use the first method below. For local health departments, or others using their own implementation of the CCB, both method are coded, and either one can be selected (in the “E1.make_death_datasets_for_app.R” file)
        -   The first method is simpler, and is based on summing for *all* deaths, the number of years prior to age 75 that *each* death occurs, with 0 YLL used for deaths occurring at ages >= 75. This method has the advantage of (1) emphasizing more strongly deaths that occur at younger ages and (2) being simpler to explain and understand. It has the disadvantage of not being consistent with the methods of the Institute for Health Metrics and Evaluation, prohibiting direct comparisons with their results.
        -   The second method uses the current approach of the Global Burden of Disease Study. With this method the YLL for each death is based on the age at death, and the additional number of years a person living in an optimal setting could be expected to live (page 30, [here](GlobalDALYmethods_2000_2015.pd.pdf)). For example, someone dying at birth would be associated with 91.94 YLL, someone dying at 25 associated with 67.08 years, and someone dying at 98 with 3.70 years. Beyond the published data, when we use this method, we associate 1.0 YLL for anyone dying above age 105.

<br>

-   Crude Rates <a id="crude-rates"></a>  
    -   All rates are expressed per 100,000 people based on the following calculations:
        -   100,000*(number (e.g. deaths, potential years of life lost) / midyear population)
    -   Confidence intervals for crude rates are based on the pois.approx() function of the [R epitools package](https://github.com/cran/epitools).

<br>

-   Age Adjusted Rates <a id="age-adjusted-rates"></a>
    -   Age-adjusted rates are based on the “direct” method, using standard definitions and procedures. A great description and the motivation for these methods can be found [here](https://www.cdc.gov/nchs/data/statnt/statnt06rv.pdf), from CDC.
    -   The Standard Population used is based on the 2000 projected U.S. population [published by CDC/NCHS in January 2001](https://www.cdc.gov/nchs/data/statnt/statnt20.pdf)–specifically, Table 2, Distribution #1 was used, but with age groups <1 and 1-4 combined. These ten age-groupings and corresponding standard population numbers can be found [here](https://github.com/mcSamuelDataSci/CACommunityBurden/blob/master/myCCB/myInfo/Age%20Group%20Standard%20and%20US%20Standard%202000%20Population.xlsx).
    -   The age-adjustment calculation, and generation of confidence intervals, was conducted using the “ageAdjust.Direct()” function of the [R epitools package](https://github.com/cran/epitools).
    -   Because a very small number of census tracts with otherwise useful data had zero population in one or more age strata (often the youngest or oldest strata, for just one sex), the above-mentioned function was modified such that rates in such strata were assigned to (reasonably enough) be 0 (rather than undefined/infinity), allowing an adjusted rate to be calculated.

<br>

-   Life Expectancy <a id="life-expectancy"></a>
    -   Life tables for tracts, communities, counties and states are generated from age specific mortality rates, which are the quotient of deaths during a calendar year to the person-years of exposure to mortality hazard, approximated by the population of the same age range at the midpoint of the year (July 1). Age structured population data for communities are estimated using aggregation of census tract level data from the American Community Survey, 5-year sample (table B01001; multiple years). County and state population by age are estimated by the Demographic Research Unit, CA Department of Finance. 
    -   Mortality and exposure data were combined on the basis of 1-, 3-, or 5-year intervals. Life tables with fewer than 700 deaths or 10,000 person-years of exposure were censored. Intra-age mortality (nax) was calculated for ages below 5 using values from a standard population (California life tables from the [US Mortality Database](https://usa.mortality.org/state.php?state=CA)). Confidence intervals were calculated from standard errors for age specific probabilities of death using Chiang’s method (Chiang 1984) with adjustments to the final age group (Eayres 2004).
        -   Chiang, C.L. 1984. The Life Table and its Applications. Robert E Krieger Publ Co., pp. 153-168.
        -   D. Eayres and E.S. Williams. Evaluation of methodologies for small area life expectancy estimation. Journal of Epidemiology & Community Health 2004. 58(3):243-249.
        -   *United States Mortality Database*. University of California, Berkeley (USA). Available at [usa.mortality.org](https://usa.mortality.org/) (data downloaded on 2020-02-27).
        

<br>

