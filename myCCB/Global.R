# =============================================================================
# "Global.R" file     
#
#   Core file for Shiny Application
#
#   Loads standards; Set Paths
#   Designates global constants for application
#   Loads all packages needed for application                                                           
#   Reads in datasets and info files needed for application                                                          
#   Load functions used to create visualizations
#   Loads and creates app text files/objects
#   Creates vectors used for Shiny app inputs   
#
# 
#   Michael Samuel, Jaspreet Kang
#   2021
#
# =============================================================================


# - 1. LOAD STANDARDS & SET PATHS ----------------------------------------------------------------------------------------------------

## Shuo test

CCB         <- TRUE
server      <- TRUE
myPlace     <- getwd()

source(paste0(myPlace,"/Standards/FusionStandards.R"))

raceLink    <- select(raceLink, raceCode,raceName,raceNameShort)


# - 2. GLOBAL CONSTANTS --------------------------------------------------------------------------------------------------

whichData <- 'real' # Change to fake to run app on local machine without access to real datasets

viewType <- "Present"

# TEXT Constants
VERSION           <- "Version P3.0" # Used in ui.R
criticalNumber    <- 11 # Used in input_widgets.R
appTitle          <- "California Community Burden of Disease Engine" # Used in ui.R

figureAttribution <- "California Department of Public Health" # Used in make_MAPS.R



# eliminates "Rplots.pdf" error generated only on CDPH Shiny Server, from tmap leaflet map
pdf(NULL) 


# USE consistent map projection system throughout all app code !
proj1 <- "+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
proj2 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"


# - 3. LOAD PACKAGES ------------------------------------------------------------------------------------------------------

library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinydashboard)
library(shinyBS)
library(magrittr)
library(leaflet)
library(tmap)
library(sf)
library(classInt) # Used in maps
library(plotly)
library(markdown)
library(directlabels)
library(scales)
library(visNetwork) # used in IHME tab
library(DT)
library(cowplot)
library(officer)
library(docxtractr)
library(openxlsx)
library(purrr)
library(reactable)
library(reactablefmtr, pos = length(search()))
library(tidyselect)

# This resolves the error message for creating a map: "Error: Shape contains invalid polygons. Please fix it or set tmap_options(check.and.fix = TRUE) and rerun the plot"
# tmap_options(check.and.fix = TRUE)

# - 4. READ DATASETS & INFO ------------------------------------------------------------------------------------------------------

# SHAPE FILES - MAPS TAB
shape_Tract        <- st_read(path(ccbData, "shape_Tract.shp"), stringsAsFactors=FALSE)
shape_Comm         <- st_read(path(ccbData, "shape_Comm.shp"), stringsAsFactors=FALSE)
shape_County       <- st_read(path(ccbData, "shape_County.shp"), stringsAsFactors=FALSE)


# MORTALITY DATASETS
datTract            <- readRDS(path(ccbData, whichData, "datTract.RDS"))
datComm             <- readRDS(path(ccbData, whichData, "datComm.RDS"))
datCounty           <- readRDS(path(ccbData, whichData, "datCounty.RDS")) %>% filter(year <= maxYear)
datCounty_RE        <- readRDS(path(ccbData, whichData, "datCounty_RE.RDS")) 
datCounty_3year     <- readRDS(path(ccbData, whichData, "datCounty_3year.RDS")) 
datCounty_AGE_3year <- readRDS(path(ccbData, whichData, "datCounty_AGE_3year.RDS")) 
datCounty_5year     <- readRDS(path(ccbData, whichData, "datCounty_5year.RDS")) 
datCounty_EDU       <- readRDS(path(ccbData, whichData, "datCounty_EDU.RDS")) 
datState_AGE        <- readRDS(path(ccbData, whichData, "datState_AGE.RDS"))
datState_RE       <- readRDS(path(ccbData, whichData, "datState_RE.RDS"))

datCounty_mcod <- readRDS(path(ccbData, whichData, "datCounty_MCOD.RDS")) %>%
  filter(!grepl("Z", causeCode)) %>%
  mutate(Ndeaths_total = Ndeaths_primary + Ndeaths_other,
         pPrimary = Ndeaths_primary / Ndeaths_total,
         pOther = Ndeaths_other / Ndeaths_total)

eduMap        <- as.data.frame(read_csv(paste0(ccbInfo, "/Education Codes and Names.csv")))
datCounty_EDU <- left_join(datCounty_EDU, eduMap, by="eduCode")

# MORTALITY DATASETS - DISPARITIES TAB
ageTest_LOW   <- readRDS(path(ccbData, "real/disparity_ageLow.RDS"))
ageTest_HIGH  <- readRDS(path(ccbData, "real/disparity_ageHigh.RDS"))

raceTest_LOW  <- readRDS(path(ccbData, "real/disparity_raceLow.RDS"))
raceTest_HIGH <- readRDS(path(ccbData, "real/disparity_raceHigh.RDS"))

sexTest_LOW   <- readRDS(path(ccbData, "real/disparity_sexLow.RDS"))
sexTest_HIGH  <- readRDS(path(ccbData, "real/disparity_sexHigh.RDS"))

# HOSPITALIZATION/ED DATASETS - AGE RACE FOCUS TABS
focusData  <- path(ccbData,"real/age_race_focus_data")
death_age   <- readRDS(path(focusData, "deaths_age_stn.RDS"))
hosp_age    <- readRDS(path(focusData, "hospital_age_stn.RDS"))
ed_age      <- readRDS(path(focusData, "ed_age_stn.RDS"))
death_race  <- readRDS(path(focusData, "deaths_race_stn.RDS"))
hosp_race   <- readRDS(path(focusData, "hospital_race_stn.RDS"))
ed_race     <- readRDS(path(focusData, "ed_race_stn.RDS"))

# HOSPITALIZATION DATASETS - HOSPITALIZATIONS TAB
oshpd_PDD_primary  <- readRDS(file = path(ccbData, whichData, "oshpd_PDD_primary.rds"))

oshpd_PDD_any      <- readRDS(file=path(ccbData, whichData,"/oshpd_PDD_any.rds")) %>%
                       select(year, county, sex, causeCode = ccsCode, Nany = n_hosp_any)


# POPULATION DATASETS - DEMOGRAPHICS TAB
popData_AgePyramid  <- readRDS(path(ccbData, "popData_AgePyramid.RDS"))
popData_RacePie     <- readRDS(path(ccbData, "popData_RacePie.RDS"))
popData_RaceAge     <- readRDS(path(ccbData, "popData_RaceAge.RDS"))
popData_trends      <- readRDS(path(ccbData, "popData_SexRaceAge_Trends.RDS"))

# SOCIAL DETERMINANTS OF HEALTH DATASETS
sdohTract         <- readRDS(path(ccbData, "sdohTract.RDS"))
sdohComm          <- readRDS(path(ccbData, "sdohComm.RDS"))
sdohCounty        <- readRDS(path(ccbData, "sdohCounty.RDS"))


# IHME DATASETS
riskByCauseData <- readRDS(path(ccbData, "risk-by-cause.RDS"))

endpoints <- read.csv(path(ccbInfo, "IHME_API_endpoints.csv"), header = TRUE)

ihmeData <- readRDS(paste0(ccbData, "v2IHME.RDS")) %>%
  subset(., year_id >= 1990)

# BURDEN VIEW
load(path(ccbData, whichData, "burdenViewData.RData"))

dataSets <- dataSets %>%
  map(~rename_with(.x, .cols = starts_with("year"), .fn = ~str_to_lower(gsub("G.*", "", .))))

plot_title[3] <- "Increase in Death Rates"
plot_title[7] <- "Reportable Disease Cases*"

# CID
dcdcData <- read_csv(path(ccbData, whichData, "CID/dcdcData.csv"))
cidRecentYear <- function(myBranch) {
  tDat <- dcdcData %>% group_by(Branch) %>% summarise(year = max(Year))
  filter(tDat, Branch == myBranch) %>% pull(year)
  
}

cidFootnote <- paste0("*The most recent year of data for STDs is ", cidRecentYear("STD") ,
                      ", for TB ", cidRecentYear("TB"), 
                      ", for vaccine preventable diseases ", cidRecentYear("VPD"), 
                      ", and for other reportable infectious diseases ", cidRecentYear("IDB"))

SummaryText     <- read.csv("myData/Burden View Report/SummaryText.csv",    
                            colClasses = "character", na.strings = "NA")




# URL PARAMETER LINKAGE
queryLink_tab <- readxl::read_xlsx(path(myPlace, 'myInfo/queryParameter_Linkage.xlsx'), sheet = 'tab')

chartYearMap    <-  read_excel(paste0(ccbInfo, "Year to Year-Group Linkage.xlsx"))


# - 5. LOAD FUNCTIONS ------------------------------------------------------------------------------------------------------------------------------------

# PLOTTING/TABLE FUNCTIONS
source(paste0(ccbFunctions, "make_MAPS.R"))
source(paste0(ccbFunctions, "make_rank_CAUSE_chart.R")) 
source(paste0(ccbFunctions, "make_cause_TABLE.R"))
source(paste0(ccbFunctions, "make_rank_GEOGRAPHY_chart.R"))
source(paste0(ccbFunctions, "make_ANY_TREND_chart.R"))
source(paste0(ccbFunctions, "make_DISPARITY_chart.R"))
source(paste0(ccbFunctions, "make_TREND-EDUCATION_chart.R"))
source(paste0(ccbFunctions, "make_LIFE-EXPECTANCY_chart.R"))
source(paste0(ccbFunctions, "make_SDOH_scatter_chart.R"))
source(paste0(ccbFunctions, "make_OSHPD_chart.R"))
source(paste0(ccbFunctions, "make_OSHPD_ANY_PRIMARY_chart.R"))
source(path(ccbFunctions, "make_rank_multibar_chart.R"))
source(path(ccbFunctions, "make_DEMOGRAPHICS_charts_V2.R"))
source(path(ccbFunctions, "make_topTrends.R"))
source(path(ccbFunctions, "make_LifeCourse_viz.R"))
source(path(ccbFunctions, "make_MCOD_charts.R"))
source(path(ccbFunctions, "make_burdenView.R"))

# Shuo Life Course - Source life table functions

# HELPER FUNCTIONS
source(paste0(ccbFunctions, "helperFunctions/wrapLabels.R"))
source(paste0(ccbFunctions, "helperFunctions/dottedSelectInput.R"))
source(paste0(ccbFunctions, "helperFunctions/stopQuietly.R"))

# IHME FUNCTIONS
source(paste0(myPlace,"/IHMEwork/arrows_Global.Part.R"))
source(paste0(myPlace,"/IHMEwork/riskByCause_Global.Function.R"))


# - 6. APP TEXT ---------------------------------------------------------------------------------------------------------------------------------------

appText         <- docxtractr::read_docx(path(ccbData, "appText/appText.docx")) # Read in appText Word Doc
appText         <- docx_extract_tbl(appText, 1) # Extract table
appTextL        <- split(appText$Text, seq(nrow(appText))) # Convert data frame into a list
names(appTextL) <- appText$varName # Add varNames to list

# We use appTextL list object to insert text throughout the app

# For hospTab text
HospitalizationsTab   <- paste(appTextL$hospA,"<br><br>", appTextL$hospB)
HospitalPrimaryAnyTab <- paste(appTextL$hospA,"<br><br>", appTextL$hospC)


# NEWS AND UPDATES
news_and_updates <- docxtractr::read_docx(paste0(ccbData, "/appText/newsUseCCB_Word.docx"))

news_and_updates <- docx_extract_tbl(news_and_updates, 1) %>%
  mutate(Text = paste("<li>", Date, Update, "</li>"))
news_and_updates <- paste('\n<li>Welcome to the CCB! App updates are listed below. If you have any questions, please contact <a href = "mailto: CCB@cdph.ca.gov">CCB@cdph.ca.gov</a></li>\n<br>', (paste(news_and_updates$Text, collapse = "\n")), sep = "\n")

# library(kableExtra)
# news_and_updates <- docxtractr::docx_extract_tbl(news_and_updates, 1) %>%
#   mutate(Date = paste0("<b>", Date, "</b>"),
#          temp = paste(Date, Update)) %>%
#   select(`<center>If you have any questions, please contact CCB@cdph.ca.gov</center>` = temp) %>%
#   kbl(escape = F) %>%
#   kable_styling(full_width = F) %>%
#   add_header_above(header = "Welcome to the CCB! App updates are listed below.", 
#                    bold = T, color = "white", background = "#0F79BF", font_size = 20) %>%
#   column_spec(1, background = c("#CFE4F2", "#BBD8EC"))



# WARNING MESSAGES
multiRaceWarning <- "** Note: Multirace data are NOT RELIABLE due to changing data collection practices"
cityMessage <- "These data are not yet available for Local Health Department city jurisdictions of Berkeley, Long Beach, Pasadena and their corresponding county Local Health Departments of Alameda HD and Los Angeles HD."
cityLHJs <- c("Alameda HD", "Berkeley", "Los Angeles HD", "Long Beach", "Pasadena")

# - 7. CREATE VECTORS FOR SHINY INPUTS -------------------------------------------------------------------------------------------------------------


# -- DEATH MEASURE DROPDOWNS ---------

deathMeasures <- c("Ndeaths", "cDeathRate", "aRate", "YLL", "YLLper", "YLL.adj.rate", "mean.age", "SMR")

deathMeasuresNames <- c(
  "Number of deaths",
  "Crude Death Rate per 100,000 population",
  "Age-Adjusted Death Rate",
  "Years of Life Lost (YLL)",
  "YLL Rate per 100,000 population",
  "Age-Adjusted YLL Rate",
  "Mean Age at Death",
  "Standard Mortality Ratio")

names(deathMeasures) <- deathMeasuresNames


# MOST TABS
deathMeasures_Dropdown         <- deathMeasures[deathMeasures != 'SMR']

# AGE TREND TAB
deathMeasures_Dropdown_noADJ   <- deathMeasures[!deathMeasures %in% c("aRate", "YLL.adj.rate", "SMR")]

# RANK BY CAUSE TAB
deathMeasuresShort_Dropdown    <- deathMeasures[deathMeasures %in% c("Ndeaths", "aRate", "YLLper", "mean.age", "SMR")]

# SDOH TAB
deathMeasures_Dropdown_SDOH    <- deathMeasures[deathMeasures %in% c("Ndeaths", "cDeathRate", "aRate")]
mean_age_sort <- c("Youngest to Oldest", "Oldest to Youngest")



# HOSPITALIZATION INPUTS
hospMeasures      <- c("n_hosp", "cHospRate", "ahospRate","avg_los", "charges", "cChargeRate", "avgcharge", "avgcharge_per_day", "medcharge", "medcharge_per_day")

hospMeasuresNames <- c("Number of Hospitalizations", "Crude Hospitalization Rate", "Age-Adjusted Hospitalization Rate", "Average Length of Stay (Days)", "Total Charges",
                       "Crude Charge Rate", "Average Charges", "Average Charges per Day", "Median Charges", "Median Charges per Day")

hospMeasures_Revalue        <- hospMeasuresNames #used in function to rename from short to long names
names(hospMeasures_Revalue) <- hospMeasures


shorthospList <- c(-2, -3, -6, -7, -8, -10)
shortMDCList  <- c(-2,-3,-4,-6,-8, -10)
hM_short      <- hospMeasures[shorthospList] 
hMNames_short <- hospMeasuresNames[shorthospList]#Used in shiny app dropdown menu



MDC_DRG_ICD <- c("icd10_cm", "mdc", "drg")
MDC_DRG_ICD_names <- c("Global Burden", "Major Diagnostic Code", "Diagnostic Related Groups")

MDC_DRG_ICD_Dropdown <- MDC_DRG_ICD
names(MDC_DRG_ICD_Dropdown) <- MDC_DRG_ICD_names


hMDCRevalue_short <- hospMeasures_Revalue[shortMDCList]
hMDCNames_short <- hospMeasuresNames[shortMDCList]

hMDCDrop_down <- c("Number of Hospitalizations", "Total Charges (in thousands)", "Average Charges (in thousands)", "Median Charges (in thousands)")

MDC_DRG <- c("mdc", "drg")
MDC_DRGNames <- c("Major Diagnostic Code", "Diagnostic Related Groups")

MDCDRG_Dropdown <- MDC_DRG
names(MDCDRG_Dropdown) <- MDC_DRGNames




# CAUSES - FULL LIST, PUBLIC HEALTH LEVEL LIST, TOP LEVEL LIST
fullList          <- deathCauseLink[, "causeCode"]
names(fullList)   <- deathCauseLink[, "causeList" ]

phList            <- deathCauseLink %>% filter(nchar(causeCode) <= 3)
phCode            <- phList[, "causeCode"]
names(phCode)     <- phList[, "causeList" ]

bigList           <- deathCauseLink %>% filter(nchar(causeCode) <= 1)
bigCode           <- bigList[, "causeCode"]
names(bigCode)    <- bigList[, "causeList"]

## MCOD LIST --------------------------------------------------------------------------

# CAUSES - MCOD TAB
ph <- function(myBroad) { 
  tDat <- deathCauseLink %>% 
    filter(nchar(causeCode) == 3, grepl(myBroad, causeCode)) %>% 
    mutate(causeList = sub("^.{3}", "  ", causeList))
  
  split(tDat$causeCode, tDat$causeList) 
  }

causeList_mcod <- list(`A. - Communicable, maternal, perinatal and nutritional conditions` = ph("A"), 
                       `B. - Cancer/Malignant neoplasms` = ph("B"), 
                       `C. - Cardiovascular diseases` = ph("C"), 
                       `D. - Other Chronic` = ph("D"), 
                       `E. - Injuries` = ph("E"), 
                       `Z. - Unknown/Missing Value` = ph("Z"))

# MCOD Measures
mcodMeasures <- c("Primary Number of Deaths" = "Ndeaths_primary", 
                  "Secondary Number of Deaths" = "Ndeaths_other", 
                  "Total Number of Deaths" = "Ndeaths_total", 
                  "Percent Primary" = "pPrimary", 
                  "Percent Secondary" = "pOther")




# SOCIAL DETERMINANTS OF HEALTH

sdohLink <- readxl::read_excel(paste0(standardsPlace, "sdohLink.xlsx")) %>%
  filter(inCCB == "x")

sdohVec <- setNames(sdohLink$sdohCode, sdohLink$sdohName)

# sdohVec  <- c("hpi2score", 
#               "insured", 
#               "est_edu",
#               "inpreschool",
#               "est_pov",
#               "est_rent30up",
#               "est_rent50up",
#               "houserepair", 
#               "parkaccess",
#               "est_net")
# 
# sdohVecL <- c(
#   "Healthy Places Index score",                                   
#   "Percentage of adults aged 18 to 64 years currently insured",
#   "Percent of the Population over 25 with a Bachelor's degree or greater",
#   "Percentage of 3 and 4 year olds enrolled in school",                    
#   "Percent of the population with an income below federal poverty level", # ORexceeding 200% of federal poverty level",
#   "Percentage of renters paying more that 30% of household income for rent",
#   "Percentage of renters paying more that 50% of household income for rent",
#   "Percent of households with kitchen facilities and plumbing",
#   "Percentage of the population living within a half-mile of a park, beach, or open space greater than 1 acre",
#   "Percent with an Internet subscription" 
# )
# 
# names(sdohVec) <- sdohVecL



# COUNTY LIST
# cityLHJs <- c("Alameda HD", "Berkeley", "Los Angeles HD", "Long Beach", "Pasadena")

lList         <- sort(as.character(unique(datCounty$county))) # Used in input widgets
# lList <- lList_lhj <- lList_lhj[!lList_lhj %in% cityLHJs]
# lListNoState  <- lList[lList != STATE] # Not used anywhere





# --- Older way of analysing OSHPD data - Michael and JASPO discuss soon !!!! Put in ClickUp --------------------------------
# calculated_metrics <- readRDS(file = path(myPlace, "myData/",whichData,"countyOSHPD.rds"))
# full_oshpd_summary <- readRDS(file = path(myPlace, "myData/", whichData, "full_oshpd_summary.rds"))
# any_primary_diff   <-   readRDS(file = path(myPlace, "myData/", whichData, "any_primary_stackedbar.rds"))


# MDC DRG ----------------------------------------------------------------------------------------------------

#Saved OSHPD MDC_DRG file in myCBD/myInfo
# hdCodes   <- read.delim(paste0(myPlace, "/myInfo/MDC_DRG.txt"), header = FALSE, sep = "=") 
# hdCodes <- hdCodes %>% rename(mdc_drg_codes = V1, names = V2) %>% mutate(mdc_drg_codes = as.character(mdc_drg_codes), names = as.character(names))
# full_CAUSE_mdcdrg_list <- read.csv(paste0(myPlace, "/myInfo/fullCAUSE_mdcdrgicd.csv"), header = TRUE, sep = ",")





#----------------------------------- JC REASON FOR HAVING THIS?

# death_age$MAINSTRATA <-  death_age$MAINSTRATA # JC

# library(fs) # Is ths used anywhere? # JC
# CBD     <- dir_info(recursive = TRUE,type="file") # JC
# CBDinfo <- cbind(as.vector(path_dir(CBD$path)),as.vector(path_file(CBD$path))) # JC


# ---- Not used anymore, but would like to get back -------------------------------------------------------------

# #SUBSITE Constants
# subSite     <- FALSE
# #subsiteList <- c("Calaveras", "Fresno", "Kings", "Madera","Merced", "San Joaquin","Stanislaus","Tulare")
# #subsiteName <- "San Joaquin Public Health Consortium Community Burden of Disease" 
# subsiteList <- c("Butte")
# subsiteName <- "Butte County CBD"
# 
# if (subSite){
#   appTitle <- subsiteName  
#   shape_County <- filter(shape_County, county %in% subsiteList)
#   shape_Comm   <- filter(shape_Comm,   county %in% subsiteList)
#   shape_Tract  <- filter(shape_Tract,  county %in% subsiteList)
#   
#   datCounty    <- filter(datCounty,    county %in% subsiteList)
#   datComm      <- filter(datComm,      county %in% subsiteList)
#   datTract     <- filter(datTract,     county %in% subsiteList)
#   
#   mdc_drg      <- filter(mdc_drg,      county %in% subsiteList)
#   
# }





# -------------

# deathMeasures <- c("Ndeaths", "cDeathRate", "aRate", "YLL", "YLLper", "YLL.adj.rate", "mean.age", "SMR")
# 
# deathMeasuresNames <- c(
#   "Number of deaths",
#   "Crude Death Rate per 100,000 population",
#   "Age-Adjusted Death Rate",
#   "Years of Life Lost (YLL)",
#   "YLL Rate per 100,000 population",
#   "Age-Adjusted YLL Rate",
#   "Mean Age at Death",
#   "Standard Mortality Ratio")
# 
# # Need this for shiny deathMeasure dropdowns
# names(deathMeasures) <- deathMeasuresNames
# 
# # Need this for dMRevalue_short, used in rank by cause function for sorting a measure (facet chart)
# names(deathMeasuresNames) <- deathMeasures
# 
# # MOST TABS
# deathMeasures_Dropdown         <- deathMeasures[-8]  # drops SMR
# # names(deathMeasures_Dropdown)  <- deathMeasuresNames[-8] # JC
# 
# # JC NOT SURE WHY THIS IS NEEDED.
# # deathMeasures_Revalue          <- deathMeasuresNames # JC
# # names(deathMeasures_Revalue)   <- deathMeasures # JC
# 
# # RANK BY CAUSE TAB
# shortdeathList <- c(1,3,5,7,8)
# dM_short       <- deathMeasures[shortdeathList]
# dMNames_short  <- deathMeasuresNames[shortdeathList]
# 
# #dMDropdown_short <- deathMeasures_Dropdown[shortdeathList]
# # dMRevalue_short  <- deathMeasures_Revalue[shortdeathList] # JC
# # dMRevalue_short  <- deathMeasuresNames[shortdeathList] # JC
# 
# #This order is needed to label the variables within the oshpdPlot function--need to define a “data dictionary” vector, in the form:
# #Labels <- c(facet_label1 = “New label1”, facet_label2 = “New label2”) etc. If defined the opposite way (eg “New label1” = facet_label1) it won’t work properly. 
# deathMeasures_Dropdown_noADJ         <- deathMeasures_Dropdown[-c(3,6)]  # drops SMR






