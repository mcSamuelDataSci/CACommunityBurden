# =============================================================================
# "Global.R" file     
#
#   Core file for Shiny Application
#
#   designates constants and folders locations for application
#   loads all packages needed for application                                                           
#   reads in shape and data files, and loads functions                                                          
#   read key "info" files                                             
#   creates vectors and contants used for Shiny app   
#
#   has set ups for local sites
# 
#   Michael Samuel
#   2018
#
# =============================================================================

#-- Key Constants -----------------------------------------------------------

# DATA Constants
whichData         <- "real" #changed to fake so I (CD) can run app on my computer
myPlace           <- getwd()
STATE             <- "CALIFORNIA"
yearGrp           <- "2014-2018"
maxYear           <- 2018

viewType <- "Present"

# TEXT Constants
VERSION           <- "Version P2.5"
criticalNumber    <- 11
mTitle            <- "California Community Burden of Disease and Cost Engine"
figureAttribution <- "California Department of Public Health"

#SUBSITE Constants
subSite     <- FALSE
#subsiteList <- c("Calaveras", "Fresno", "Kings", "Madera","Merced", "San Joaquin","Stanislaus","Tulare")
#subsiteName <- "San Joaquin Public Health Consortium Community Burden of Disease" 
subsiteList <- c("Stanislaus")
subsiteName <- "Stanislaus County CBD"


# eliminates "Rplots.pdf" error generated only on CDPH Shiny Server, from tmap leaflet map
pdf(NULL) 


# DISPLAY Constants


myTitleSize <- 24
myLegendSize <- 24
myAxisSize  <- 22

myTextSize2 <- 12
myWrapNumber <- 70
myTitleColor <- "darkblue"

myCex1           <- 2  # 1.5  #line labels
myCex2           <- 1.2  #currently used only in education trend
myLineLabelSpace <- 0.3


myLineSize  <- 2
myPointSize <- 5 # line markers
myPointShape <- 18




#-- Load Packages ------------------------------------------------------------

library(shiny)  
library(shinyjs)
library(shinyWidgets)

library(dplyr)
library(tidyr)
library(magrittr)
library(readxl)
library(readr) 

library(leaflet) 
library(tmap)
library(sf)

library(classInt)  
library(RColorBrewer)
library(plotly)  # Note: also loads ggplot2
library(fs)
library(markdown)
library(directlabels)  # Used to directly label lines in Trend plots
library(scales)

library(visNetwork)
library(stringr)
library(shinydashboard)

# --- IHME work -----------------------

source(paste0(myPlace,"/IHMEwork/arrows_Global.Part.R"))
source(paste0(myPlace,"/IHMEwork/riskByCause_Global.Function.R"))


# --- CBD Key Inputs ---------------------------------------------------------

# Shapes file: ------------------------------- 

# USE consistent map projection system throughout all app code !
proj1 <- "+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
proj2 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# Read shape files as simple features objects (st_read is from sf package)
shape_Tract        <- st_read(path(myPlace,"/myData/shape_Tract.shp"),stringsAsFactors=FALSE)
shape_Comm         <- st_read(path(myPlace,"/myData/shape_Comm.shp"),stringsAsFactors=FALSE)
shape_County       <- st_read(path(myPlace,"/myData/shape_County.shp"),stringsAsFactors=FALSE)

# Prior Approaches to reading "shape" files, kept for refereance
# shape_County   <- readOGR(paste0(myPlace,"/myData/shape_County.shp"),p4s=proj1) 
# shape_County   <- st_read(paste0(myPlace,"/myData/shape_County.rds")) # --> very large!
# shape_County   <- readShapePoly(paste0(myPlace,"/myData/shape_County"),proj4string=CRS(proj1)) 

shape_Tract$GEOID  <- as.character(shape_Tract$GEOID)    
shape_Tract$county <- as.character(shape_Tract$county)   

# Data: ----------------------------------------- 

datTract        <- readRDS(path(myPlace,"/myData/",whichData,"datTract.RDS"))
datComm         <- readRDS(path(myPlace,"/myData/",whichData,"datComm.RDS"))
datCounty       <- readRDS(path(myPlace,"/myData/",whichData,"datCounty.RDS"))
datCounty_RE    <- readRDS(path(myPlace,"/myData/",whichData,"datCounty_RE.RDS")) #this was written as "datCounty_RE.RDS", but the file is actually saved as "datCounty.RE.REDS"
datCounty_3year <- readRDS(path(myPlace,"/myData/",whichData,"datCounty_3year.RDS")) #this file doesn't exist in the fake (or real) data folder, so currently commented out to allow app to run. 
datCounty_AGE_3year <- readRDS(path(myPlace,"/myData/",whichData,"datCounty_AGE_3year.RDS")) #this file doesn't exist in the fake (or real) data folder, 

datCounty_5year <- readRDS(path(myPlace,"/myData/",whichData,"datCounty_5year.RDS")) #this file doesn't exist in the fake (or real) data folder, so currently commented out to allow app to run. 


datCounty_EDU <- readRDS(path(myPlace,"/myData/",whichData,"datCounty_EDU.RDS"))

eduMap        <- as.data.frame(read_csv(paste0(myPlace,"/myInfo/Education Codes and Names.csv")))
datCounty_EDU <- left_join(datCounty_EDU,eduMap,by="eduCode")



#FIX THIS --- OSHPD
mdc_drg            <- readRDS(path(myPlace,"/myData/",whichData,"mdc_drg.rds"))
calculated_metrics <- readRDS(file = path(myPlace, "myData/",whichData,"countyOSHPD.rds"))
full_oshpd_summary <- readRDS(file = path(myPlace, "myData/", whichData, "full_oshpd_summary.rds"))
any_primary_diff <-   readRDS(file = path(myPlace, "myData/", whichData, "any_primary_stackedbar.rds"))



# age_adjusted_hosp_rates  <- readRDS(path(myPlace,"/myData/",whichData,"ageadj_hospratesOSHPD.rds")) %>% rename(ahospRate = measure) %>% select(-type)


load(path(myPlace,"/myData/","sdohTract.R"))
load(path(myPlace,"/myData/","sdohComm.R"))
load(path(myPlace,"/myData/","sdohCounty.R"))

if (subSite){
  mTitle <- subsiteName  
  shape_County <- filter(shape_County, county %in% subsiteList)
  shape_Comm   <- filter(shape_Comm,   county %in% subsiteList)
  shape_Tract  <- filter(shape_Tract,  county %in% subsiteList)
  
  datCounty    <- filter(datCounty,    county %in% subsiteList)
  datComm      <- filter(datComm,      county %in% subsiteList)
  datTract     <- filter(datTract,     county %in% subsiteList)
  
  mdc_drg      <- filter(mdc_drg,      county %in% subsiteList)
  
}


#-- Load Info Files and Functions ---------------------------------------------

gbdMap0    <- as.data.frame(read_excel( path(myPlace,"myInfo/gbd.ICD.Map.xlsx"), sheet="main"))

#Saved OSHPD MDC_DRG file in myCBD/myInfo

hdCodes   <- read.delim(paste0(myPlace, "/myInfo/MDC_DRG.txt"), header = FALSE, sep = "=") 
hdCodes <- hdCodes %>% rename(mdc_drg_codes = V1, names = V2) %>% mutate(mdc_drg_codes = as.character(mdc_drg_codes), names = as.character(names))

full_CAUSE_mdcdrg_list <- read.csv(paste0(myPlace, "/myInfo/fullCAUSE_mdcdrgicd.csv"), header = TRUE, sep = ",")

source(paste0(myPlace,"/myFunctions/make_MAPS.R"))
source(paste0(myPlace,"/myFunctions/make_rank_CAUSE_chart.R")) 
source(paste0(myPlace,"/myFunctions/make_cause_TABLE.R"))
source(paste0(myPlace,"/myFunctions/make_rank_GEOGRAPHY_chart.R"))
source(paste0(myPlace,"/myFunctions/make_TREND_chart.R"))
source(paste0(myPlace,"/myFunctions/make_TREND-RACE_chart.R"))
source(paste0(myPlace,"/myFunctions/make_TREND-AGE_chart.R"))
source(paste0(myPlace,"/myFunctions/make_RACE-DISPARITY_chart.R"))

source(paste0(myPlace,"/myFunctions/make_TREND-EDUCATION_chart.R"))


source(paste0(myPlace,"/myFunctions/make_LIFE-EXPECTANCY_chart.R"))


source(paste0(myPlace,"/myFunctions/make_SDOH_scatter_chart.R"))
source(paste0(myPlace, "/myFunctions/make_OSHPD_chart1.R"))
source(paste0(myPlace, "/myFunctions/make_OSHPD_chart2.R"))
source(paste0(myPlace, "/myFunctions/make_MDC_DRG_chart.R"))
source(paste0(myPlace, "/myFunctions/make_any_primary_OSHPD_chart.R"))
source(paste0(myPlace, "/myFunctions/make_OSHPD_map.R"))
#source(paste0(myPlace,"/myFunctions/rankCausesSex.R")) 

source(paste0(myPlace,"/myFunctions/helperFunctions/wrapLabels.R"))
source(paste0(myPlace,"/myFunctions/helperFunctions/dottedSelectInput.R"))


source(paste0(myPlace,"/myData/appText/AppText.txt"))
source(paste0(myPlace,"/myData/appText/newsUseText.txt"))


# --- Shiny Stuff and Constants -----------------------------------------------



chartYearMap    <-  read_excel(paste0(myPlace,"/myInfo/Year to Year-Group Linkage.xlsx"))  

deathMeasures <- c("Ndeaths","cDeathRate","aRate","YLL","YLLper","YLL.adj.rate", "mean.age","SMR")

deathMeasuresNames <- c(
  "Number of deaths",
  "Crude Death Rate per 100,000 population",
  "Age-Adjusted Death Rate",
  "Years of Life Lost (YLL)",
  "YLL Rate per 100,000 population",
  "Age-Adjusted YLL Rate",
  "Mean Age at Death",
  "Standard Mortality Ratio")






deathMeasures_Dropdown         <- deathMeasures[-8]  # drops SMR
names(deathMeasures_Dropdown)  <- deathMeasuresNames[-8]

deathMeasures_Revalue          <- deathMeasuresNames
names(deathMeasures_Revalue)   <- deathMeasures

shortdeathList <- c(1,3,5,7,8)
dM_short       <- deathMeasures[shortdeathList]
dMNames_short  <- deathMeasuresNames[shortdeathList]

#dMDropdown_short <- deathMeasures_Dropdown[shortdeathList]
dMRevalue_short  <- deathMeasures_Revalue[shortdeathList]



#This order is needed to label the variables within the oshpdPlot function--need to define a “data dictionary” vector, in the form:
#Labels <- c(facet_label1 = “New label1”, facet_label2 = “New label2”) etc. If defined the opposite way (eg “New label1” = facet_label1) it won’t work properly. 


hospMeasures <- c("n_hosp", "cHospRate", "ahospRate","avg_los", "charges", "cChargeRate", "avgcharge", "avgcharge_per_day", "medcharge", "medcharge_per_day")

hospMeasuresNames <- c("Number of Hospitalizations", "Crude Hospitalization Rate", "Age-Adjusted Hospitalization Rate", "Average Length of Stay (Days)", "Total Charges",
                       "Crude Charge Rate", "Average Charges", "Average Charges per Day", "Median Charges", "Median Charges per Day")

hospMeasures_Revalue <- hospMeasuresNames #used in function to rename from short to long names
names(hospMeasures_Revalue) <- hospMeasures


shorthospList <- c(-2, -3, -6, -7, -8, -10)
shortMDCList <- c(-2,-3,-4,-6,-8, -10)
hM_short <- hospMeasures[shorthospList] 
hMNames_short <- hospMeasuresNames[shorthospList]#Used in shiny app dropdown menu

MDC_DRG_ICD <- c("icd10_cm", "mdc", "drg")
MDC_DRG_ICD_names <- c("Global Burden", "Major Diagnostic Code", "Diagnostic Related Groups")

MDC_DRG_ICD_Dropdown <- MDC_DRG_ICD
names(MDC_DRG_ICD_Dropdown) <- MDC_DRG_ICD_names

#for mdc_drg


hMDCRevalue_short <- hospMeasures_Revalue[shortMDCList]
hMDCNames_short <- hospMeasuresNames[shortMDCList]

hMDCDrop_down <- c("Number of Hospitalizations", "Total Charges (in thousands)", "Average Charges (in thousands)", "Median Charges (in thousands)")

MDC_DRG <- c("mdc", "drg")
MDC_DRGNames <- c("Major Diagnostic Code", "Diagnostic Related Groups")

MDCDRG_Dropdown <- MDC_DRG 
names(MDCDRG_Dropdown) <- MDC_DRGNames



fullCauseList     <- gbdMap0[!is.na(gbdMap0$causeList),c("LABEL","causeList","nameOnly")] %>% arrange(LABEL)
fullList          <- fullCauseList[,"LABEL"]
names(fullList)   <- fullCauseList[,"causeList" ]

phList            <- fullCauseList[nchar(fullCauseList$LABEL) <= 3,]
phCode            <- phList[,"LABEL"]
names(phCode)     <- phList[,"causeList" ]

bigList           <- fullCauseList[nchar(fullCauseList$LABEL) == 1,]
bigCode           <- bigList[,"LABEL"]
names(bigCode)    <- bigList[,"causeList"]






#-- Social Determinants of Health Measures and Names


sdohVec  <- c("hpi2score", 
              "insured", 
              "est_edu",
              "inpreschool",
              "est_pov",
              "est_rent30up",
              "est_rent50up",
              "houserepair", 
              "parkaccess",
              "est_net")

sdohVecL <- c(
  "Healthy Places Index score",                                   
  "Percentage of adults aged 18 to 64 years currently insured",
  "Percent of the Population over 25 with a Bachelor's degree or greater",
  "Percentage of 3 and 4 year olds enrolled in school",                    
  "Percent of the population with an income below federal poverty level", # ORexceeding 200% of federal poverty level",
  "Percentage of renters paying more that 30% of household income for rent",
  "Percentage of renters paying more that 50% of household income for rent",
  "Percent of households with kitchen facilities and plumbing",
  "Percentage of the population living within a half-mile of a park, beach, or open space greater than 1 acre",
  "Percent with an Internet subscription" 
  )

names(sdohVec) <- sdohVecL

lList         <- sort(as.character(unique(datCounty$county)))
lListNoState  <- lList[lList != STATE]




raceCodeFull <- c("-missing","White-NH","Black-NH","AIAN-NH","Asian-NH","NHPI-NH","Other-NH","Multi-NH","Unk-NH","Hisp")
raceNameFull <- c("missing","White","Black","Native American","Asian","Nat. Haw./PI.","Other","**Multirace**","unknown","Hispanic")


# move these...
raceNote         <- "* Note: All race/ethnic groups except 'Hispanic' are NON-Hispanic; 'Black'='Black/African American',\n 'Native American' include Alaska Natives, 'Nat. Haw./PI' is 'Native Hawaiian/Pacific Islander'"
multiRaceWarning <- "** Note: Multirace data are NOT RELIABLE due to changing data collection practices"


# --- END ---------------------------------------------------------------------
# ----------------------------------------------------------------------------


library(fs)
CBD     <- dir_info(recursive = TRUE,type="file")
CBDinfo <- cbind(as.vector(path_dir(CBD$path)),as.vector(path_file(CBD$path)))


# NOTES:
# myYear <- 2013
# myLHJ  <- "Colusa"
# myLev <- 1
# myCause <- 104
# myCause  <- "Diabetes mellitus"

# library(shinythemes)
# library(shinymaterial) 

# library(maptools)   
# library(rgdal)      
# library(maps)
