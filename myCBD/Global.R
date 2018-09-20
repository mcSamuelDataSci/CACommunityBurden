# =====================================================================================
# "Global.R" file                                                                     |
#            designate folder locations                                               |
#            load packages                                                            |
#            read in shape and data files                                             |
#            creates sub-site for "San Joaquin Public Health Consortium"              |
#            load functions                                                           |
#            read key "info" files                                                    |
#            creates vectors and contants used for Shiny app                          |
#                                                                                     |   
# =====================================================================================


#-- Set Locations and Data Source ----------------------------------------------------------

 whichData <-  "real"
 myPlace   <- getwd()   
 STATE     <- "CALIFORNIA"
 
 # temporary for testing:
 source(paste0(myPlace,"/myData/appText/appTextWork.txt"))
 
 pdf(NULL) # eliminates "Rplots.pdf" error generated only on CDPH Shiny Server, from tmap leaflet map

 #-- Load Packages --------------------------------------------------------------------------

 library(shiny)  
 library(dplyr)
 library(readxl)
 library(readr) 
 
 library(maptools)   #needed? 
 library(rgdal)      #needed?
 library(leaflet) 
 library(tmap)
 library(sf)
 
 library(classInt);  
 library(RColorBrewer);
 library(epitools)
 library(plotly)
 library(fs)
 
# library(scatterD3);
# library(maps);  
# library(shinythemes)
# library(shinymaterial)

 # --- CBD Key Inputs --------------------------------------------------------------------------------------

# Shapes file: ------------------------------- 
 
# USE consistent map projection system throughout all app code !
proj1 <- "+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
proj2 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
 
 
# check each of below with:  class(shape_County); object.size(shape_County)
# shape_County   <- readOGR(paste0(myPlace,"/myData/shape_County.shp"))  # -->    822,048 bytes "SpatialPolygonsDataFrame"
# shape_County   <- readOGR(paste0(myPlace,"/myData/shape_County.rds"))  # -->      Error
# shape_County   <- read_rds(paste0(myPlace,"/myData/shape_County.rds")) # --> 11,174,336 bytes "sf"  "data.frame"
# shape_County   <- st_read(paste0(myPlace,"/myData/shape_County.shp"))  # -->    674,488 bytes "sf"  "data.frame"
# shape_County   <- st_read(paste0(myPlace,"/myData/shape_County.rds"))  # -->      Error


# THESE DO NOT WORK IN THE APP:
 # shape_Tract        <- st_read(path(myPlace,"/myData/shape_Tract.shp"))
 # shape_Comm         <- st_read(path(myPlace,"/myData/shape_Comm.shp"))
 # shape_County       <- st_read(path(myPlace,"/myData/shape_County.shp"))
 # 
# THESE DO: 
 shape_County   <- readOGR(paste0(myPlace,"/myData/shape_County.shp")) 
 shape_Comm     <- readOGR(paste0(myPlace,"/myData/shape_Comm.shp")) 
 shape_Tract    <- readOGR(paste0(myPlace,"/myData/shape_Tract.shp"))  
 
 shape_Tract$GEOID  <- as.character(shape_Tract$GEOID)    
 shape_Tract$county <- as.character(shape_Tract$county)   
 
# Data: ----------------------------------------- 
 
datTract  <- readRDS(path(myPlace,"/myData/",whichData,"datTract.RDS"))
datComm   <- readRDS(path(myPlace,"/myData/",whichData,"datComm.RDS"))
datCounty <- readRDS(path(myPlace,"/myData/",whichData,"datCounty.RDS"))

load(path(myPlace,"/myData/","sdohTract.R"))
load(path(myPlace,"/myData/","sdohComm.R"))
load(path(myPlace,"/myData/","sdohCounty.R"))

#-- Load Info Files and Functions ------------------------------------------------------------------------
  
  gbdMap0    <- as.data.frame(read_excel( path(myPlace,"myInfo//NEWgbd.ICD.Map.xlsx/"), sheet="main"))    #extra "/" as examples
  
  source(paste0(myPlace,"/myFunctions/helperFunctions/wrapSentence.R"))
  source(paste0(myPlace,"/myFunctions/helperFunctions/wrapLabels.R"))
  source(paste0(myPlace,"/myFunctions/helperFunctions/compass.R"))

  source(paste0(myPlace,"/myFunctions/cbdMap-tmap.R"))
  source(paste0(myPlace,"/myFunctions/rankCausesSelectGeo.R")) 
  source(paste0(myPlace,"/myFunctions/rankCausesSelectGeoTable.R"))
  source(paste0(myPlace,"/myFunctions/rankGeosSelectCause.R"))
  source(paste0(myPlace,"/myFunctions/trend.R"))
  source(paste0(myPlace,"/myFunctions/scatterSDOH.R"))

  source(paste0(myPlace,"/myData/appText/appTextWork.txt"))

  version <- "0.5.0"
  
  mTitle       <- "California Community Burden of Disease and Costs"
  
# --- Create "Sub-Set" Site: San Joaquin Public Health Consortium------------------------------------------

  sjconsortium <- c("Calaveras", "Fresno", "Kings", "Madera","Merced", "San Joaquin","Stanislaus","Tulare")
  sjc          <- FALSE
  
  if (sjc){
    mTitle <- "San Joaquin Public Health Consortium Community Burden of Disease"  
    shape_County <- shape_County[shape_County$county %in% sjconsortium,]
    shape_Comm   <- shape_Comm[  shape_Comm $county  %in% sjconsortium,]
    shape_Tract  <- shape_Tract[ shape_Tract$county  %in% sjconsortium,]
    
    datCounty <- datCounty[datCounty$county %in% sjconsortium,]
    datComm   <- datComm[datComm$county %in% sjconsortium,]
    datTract  <- datTract[datTract$county %in% sjconsortium,]  }
  
# --- Shiny Stuff and Constants ---------------------------------------------------------------------------

# med.age, m.YLL  
lMeasures <- c("YLL","YLLper","YLL.adj.rate","Ndeaths","cDeathRate","aRate", "mean.age","SMR")

lMeasuresC <- c("Years of Life Lost (YLL)",
                "YLL per 100,000 population",
                "Age-Adjusted YLL Rate",
                "Number of deaths",
                "Crude Death Rate",
                "Age-Adjusted Death Rate",
                "Mean Age at Death",
                "Standard Mortality Ratio")

names(lMeasures) <- lMeasuresC

lMeasuresShort <- lMeasures[c(4,2,6,7,8)] # fix later

causeList36       <- gbdMap0[!is.na(gbdMap0$causeList),c("LABEL","causeList","nameOnly")] %>% arrange(LABEL)
causeNum36        <- causeList36[,"LABEL"]
names(causeNum36) <- causeList36[,"causeList" ]

phList   <- causeList36[nchar(causeList36$LABEL) <= 3,]
phCode   <- phList[,"LABEL"]
names(phCode) <- phList[,"causeList" ]

bigList  <- causeList36[nchar(causeList36$LABEL) == 1,]
bigCode  <- bigList[,"LABEL"]
names(bigCode) <- bigList[,"causeList"]

sdohVecL  <- c("Less than Bachelors Degree","Below Federal Poverty",'HPI Raw Score')
sdohVec   <- c("lessBachelor","belowPov","hpiScore") 
names(sdohVec) <- sdohVecL

lList         <- sort(as.character(unique(datCounty$county)))
lListNoState  <- lList[lList != STATE]

if (sjc) {lList <- lList[lList %in% sjconsortium]}

nC       <- 5
myColor1 <- rev(brewer.pal(nC,"RdYlBu"))
yG       <- "2011-2015"

# myYear <- 2013
# myLHJ  <- "Colusa"
# myLev <- 1
# myCause <- 104
# myCause  <- "Diabetes mellitus"

# --- END --------------------------------------------------------------------------------------------------


# OLD stuff for GIS notes:
# shape_County  <- readShapePoly(paste0(myPlace,"/myData/shape_County"),proj4string=CRS(proj1)) 
# shape_County  <- readOGR(paste0(myPlace,"/myData/shape_County.shp"),p4s=proj1) 


