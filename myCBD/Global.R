# =============================================================================
# "Global.R" file     
#
#   Core file for Shiny Application
#
#   designates constants and folders locations for applcation
#   loads all packages needed for application                                                           
#   reads in shape and data files, and loads functions                                                          
#   read key "info" files                                             
#   creates vectors and contants used for Shiny app   
#
#   has set ups for local sites
#     "San Joaquin Public Health Consortium"       
# 
#   Michael Samuel
#   2018
#
# =============================================================================


#-- Key Constants -----------------------------------------------------------

 whichData <- "real"
 VERSION   <- "Version B1.0"
 myPlace   <- getwd()   
 STATE     <- "CALIFORNIA"
 yearGrp   <- "2013-2017"
 mTitle    <- "California Community Burden of Disease and Cost Engine"
 
 pdf(NULL) # eliminates "Rplots.pdf" error generated only on CDPH Shiny Server, from tmap leaflet map

#-- Load Packages ------------------------------------------------------------

 library(shiny)  
 library(shinyjs)
# library(shinythemes)
# library(shinymaterial) 
 
 library(dplyr)
 library(readxl)
 library(readr) 
 
# library(maptools)   
# library(rgdal)      
# library(maps)
 library(leaflet) 
 library(tmap)
 library(sf)
 
 library(classInt)  
 library(RColorBrewer)
 library(epitools)
 library(plotly)
 library(fs)
 library(markdown)
 
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
 
datTract  <- readRDS(path(myPlace,"/myData/",whichData,"datTract.RDS"))
datComm   <- readRDS(path(myPlace,"/myData/",whichData,"datComm.RDS"))
datCounty <- readRDS(path(myPlace,"/myData/",whichData,"datCounty.RDS"))

load(path(myPlace,"/myData/","sdohTract.R"))
load(path(myPlace,"/myData/","sdohComm.R"))
load(path(myPlace,"/myData/","sdohCounty.R"))

#-- Load Info Files and Functions ---------------------------------------------
  
  gbdMap0    <- as.data.frame(read_excel( path(myPlace,"myInfo//gbd.ICD.Map.xlsx/"), sheet="main"))    #extra "/" as examples
  
  source(paste0(myPlace,"/myFunctions/helperFunctions/wrapSentence.R"))
  source(paste0(myPlace,"/myFunctions/helperFunctions/wrapLabels.R"))
  source(paste0(myPlace,"/myFunctions/helperFunctions/compass.R"))

  source(paste0(myPlace,"/myFunctions/make_MAPS.R"))
  source(paste0(myPlace,"/myFunctions/make_rank_CAUSE_chart.R")) 
  source(paste0(myPlace,"/myFunctions/rankCausesSex.R")) 
  source(paste0(myPlace,"/myFunctions/make_cause_TABLE.R"))
  source(paste0(myPlace,"/myFunctions/make_rank_GEOGRAPHY_chart.R"))
  source(paste0(myPlace,"/myFunctions/make_TREND_chart.R"))
  source(paste0(myPlace,"/myFunctions/make_SDOH_scatter_chart.R"))

  source(paste0(myPlace,"/myData/appText/AppText.txt"))
  source(paste0(myPlace,"/myData/appText/newsUseText.txt"))

# --- "SUB-SITE" LINES here when making subsite
# --- San Joaquin Public Health Consortium lines stored at bottom of this file

# --- Shiny Stuff and Constants -----------------------------------------------

# med.age, m.YLL  
lMeasures <- c("YLL","YLLper","YLL.adj.rate","Ndeaths","cDeathRate","aRate", "mean.age","SMR")


#   yll                     yll                        YLL
#   yllRate                 yll.rate                   YLL.rate
#   yllAdjustedRate         yll.adjusted.rate          YLL.adjusted.rate
#   deaths                  ndeaths                    Ndeaths
#   deathRate               death.rate
#   deathAdjustedRate       death.adjusted.rate
#   meanAge                 mean.age.at.death
#   SMR                     SMR 

#   similar nomenclature for confidence intervals


lMeasuresC <- c("Years of Life Lost (YLL)",
                "YLL Rate per 100,000 population",
                "Age-Adjusted YLL Rate",
                "Number of deaths",
                "Crude Death Rate per 100,000 population",
                "Age-Adjusted Death Rate",
                "Mean Age at Death",
                "Standard Mortality Ratio")

names(lMeasures) <- lMeasuresC

lMeasuresShort <- lMeasures[c(4,2,6,7,8)] # fix later

fullCauseList       <- gbdMap0[!is.na(gbdMap0$causeList),c("LABEL","causeList","nameOnly")] %>% arrange(LABEL)
causeNum36        <- fullCauseList[,"LABEL"]
names(causeNum36) <- fullCauseList[,"causeList" ]

phList   <- fullCauseList[nchar(fullCauseList$LABEL) <= 3,]
phCode   <- phList[,"LABEL"]
names(phCode) <- phList[,"causeList" ]

bigList  <- fullCauseList[nchar(fullCauseList$LABEL) == 1,]
bigCode  <- bigList[,"LABEL"]
names(bigCode) <- bigList[,"causeList"]

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


# if (sjcSite) {lList <- lList[lList %in% sjconsortium]}

nC       <- 5
myColor1 <- rev(brewer.pal(nC,"RdYlBu"))


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

# ----------------------------------------------------------------------------
# --- Create "Sub-Set" Site: San Joaquin Public Health Consortium--------------

sjconsortium <- c("Calaveras", "Fresno", "Kings", "Madera","Merced", "San Joaquin","Stanislaus","Tulare")
sjcSite      <- FALSE

if (sjcSite){
  mTitle <- "San Joaquin Public Health Consortium Community Burden of Disease"  
  shape_County <- shape_County[shape_County$county %in% sjconsortium,]
  shape_Comm   <- shape_Comm[  shape_Comm $county  %in% sjconsortium,]
  shape_Tract  <- shape_Tract[ shape_Tract$county  %in% sjconsortium,]
  
  datCounty <- datCounty[datCounty$county %in% sjconsortium,]
  datComm   <- datComm[datComm$county %in% sjconsortium,]
  datTract  <- datTract[datTract$county %in% sjconsortium,]  }
# ----------------------------------------------------------------------------


