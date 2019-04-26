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
whichData         <- "real"
myPlace           <- getwd()
STATE             <- "CALIFORNIA"
yearGrp           <- "2013-2017"

# TEXT Constants
VERSION           <- "Version P1.1"
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

#-- Load Packages ------------------------------------------------------------

library(shiny)  
library(shinyjs)

library(dplyr)
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

datTract     <- readRDS(path(myPlace,"/myData/",whichData,"datTract.RDS"))
datComm      <- readRDS(path(myPlace,"/myData/",whichData,"datComm.RDS"))
datCounty    <- readRDS(path(myPlace,"/myData/",whichData,"datCounty.RDS"))
datCounty.RE <- readRDS(path(myPlace,"/myData/",whichData,"datCounty.RE.RDS"))


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
}


#-- Load Info Files and Functions ---------------------------------------------

gbdMap0    <- as.data.frame(read_excel( path(myPlace,"myInfo/gbd.ICD.Map.xlsx"), sheet="main"))    

source(paste0(myPlace,"/myFunctions/make_MAPS.R"))
source(paste0(myPlace,"/myFunctions/make_rank_CAUSE_chart.R")) 
source(paste0(myPlace,"/myFunctions/make_cause_TABLE.R"))
source(paste0(myPlace,"/myFunctions/make_rank_GEOGRAPHY_chart.R"))
source(paste0(myPlace,"/myFunctions/make_TREND_chart.R"))
source(paste0(myPlace,"/myFunctions/make_TREND-RACE_chart.R"))
source(paste0(myPlace,"/myFunctions/make_SDOH_scatter_chart.R"))
source(paste0(myPlace, "/myFunctions/make_OSHPD_chart.R"))
#source(paste0(myPlace,"/myFunctions/rankCausesSex.R")) 

source(paste0(myPlace,"/myFunctions/helperFunctions/wrapLabels.R"))

source(paste0(myPlace,"/myData/appText/AppText.txt"))
source(paste0(myPlace,"/myData/appText/newsUseText.txt"))

# --- Shiny Stuff and Constants -----------------------------------------------

lMeasures <- c("Ndeaths","cDeathRate","aRate","YLL","YLLper","YLL.adj.rate", "mean.age","SMR")


#  (SOME DAY) edit measure names to standards (and similar nomenclature for confidence intervals):
#  1  YLL                     yll                   
#  2  YLLper                  yll.rate              
#  3  YLL.adj.rate  e         yll.adjusted.rate     
#  4  Ndeaths                 ndeaths               
#  5  cDeathRate              death.rate
#  6  aRate                   death.adjusted.rate
#  7  mean.age                mean.age.at.death
#  8  SMR                     SMR 



lMeasuresC <- c(
  "Number of deaths",
  "Crude Death Rate per 100,000 population",
  "Age-Adjusted Death Rate",
  "Years of Life Lost (YLL)",
  "YLL Rate per 100,000 population",
  "Age-Adjusted YLL Rate",
  "Mean Age at Death",
  "Standard Mortality Ratio")

names(lMeasures) <- lMeasuresC
lMeasuresShort   <- lMeasures[c(4,2,6,7,8)] 


hospDiscMeasures <- c(n_hosp = "Number of Hospitalizations",
                      cHospRate = "Crude Hosp Rate",
                      ahospRate = "Age-Adjusted Hosp Rate",
                      charges = "Total Charges",
                      cChargeRate = "Crude Charge Rate",
                      avgcharge = "Average Charges")

#hospMeasuresShort <- hospDiscMeasures[c(-6)]


fullCauseList     <- gbdMap0[!is.na(gbdMap0$causeList),c("LABEL","causeList","nameOnly")] %>% arrange(LABEL)
fullList          <- fullCauseList[,"LABEL"]
names(fullList)   <- fullCauseList[,"causeList" ]

phList            <- fullCauseList[nchar(fullCauseList$LABEL) <= 3,]
phCode            <- phList[,"LABEL"]
names(phCode)     <- phList[,"causeList" ]

bigList           <- fullCauseList[nchar(fullCauseList$LABEL) == 1,]
bigCode           <- bigList[,"LABEL"]
names(bigCode)    <- bigList[,"causeList"]

sdohVec  <- c("hpi2score", "insured", "inpreschool", "bachelorsed", "abovepoverty", "parkaccess","houserepair")

sdohVecL <- c(
  "Healthy Places Index score",                                   
  "Percentage of adults aged 18 to 64 years currently insured",
  "Percentage of 3 and 4 year olds enrolled in school",                    
  "Percentage of population over age 25 with a bachelor's education or higher",      
  "Percent of the population with an income exceeding 200% of federal poverty level",
  "Percentage of the population living within a half-mile of a park, beach, or open space greater than 1 acre",
  "Percent of households with kitchen facilities and plumbing")

names(sdohVec) <- sdohVecL

lList         <- sort(as.character(unique(datCounty$county)))
lListNoState  <- lList[lList != STATE]




raceCodeFull <- c("-missing","White-NH","Black-NH","AIAN-NH","Asian-NH","NHPI-NH","Other-NH","Multi-NH","Unk-NH","Hisp")
raceNameFull <- c("missing","White","Black","Native American","Asian","Hawaiian","Other","Multirace","unknown","Hispanic")


raceNote     <- "* Note: All race/ethnic groups except 'Hispanic' are NON-Hispanic; 'Black'='Black/African American',\n 'Native American' include Alaska Natives, 'Hawaiian' is 'Native Hawaiian/Pacific Islander'"








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
