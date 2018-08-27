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

 myPlace   <- getwd()   
 whichData <-  "fake"
 
#-- Load Packages --------------------------------------------------------------------------

 library(shiny)  
 library(dplyr)
 
 library(readxl)
 library(readr) 
 
 library(maptools); 
 library(rgdal)
 library(leaflet); 

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

 # MIGRATE all mapping to tmap? 
 
# USE consistent map projection system throughout all app code !
proj1 <- "+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
proj2 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
 
 #  myProj             <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"                            # Key line for Happy GIS
 #  shape_County       <- readShapePoly(paste0(myPlace,"/myData/shape_County"),proj4string=CRS(myProj)) 
 #  shape_Comm         <- readShapePoly(paste0(myPlace,"/myData/shape_Comm"),proj4string=CRS(myProj))   # Read Shape Files
 #  
 # shape_Tract        <- readShapePoly(paste0(myPlace,"/myData/shape_Tract"),proj4string=CRS(myProj))  

 # shape_County       <- readOGR(paste0(myPlace,"/myData/shape_County.shp"),p4s=proj1) 
 # shape_Comm         <-readOGR(paste0(myPlace,"/myData/shape_Comm.shp"),p4s=proj1)   # Read Shape Files
 # shape_Tract        <- readOGR(paste0(myPlace,"/myData/shape_Tract.shp"),p4s=proj1)  
 
 
 shape_County       <- readOGR(paste0(myPlace,"/myData/shape_County.shp")) 
 shape_Comm         <-readOGR(paste0(myPlace,"/myData/shape_Comm.shp"))   # Read Shape Files
 shape_Tract        <- readOGR(paste0(myPlace,"/myData/shape_Tract.shp"))  
 
  #simple feature objects
# shape_Tract        <- read_rds(path(myPlace,"/myData/shape_Tract.rds"))
# shape_Comm         <- read_rds(path(myPlace,"/myData/shape_Comm.rds"))
# shape_County       <- read_rds(path(myPlace,"/myData/shape_County.rds"))
 
 shape_Tract$GEOID  <- as.character(shape_Tract$GEOID)    
 shape_Tract$county <- as.character(shape_Tract$county)   
 
  #not quite...
  #library(sf)
  #shape_Tract <-  st_read(paste0(myPlace,"/myData/shape_Tract.shp"),stringsAsFactors = FALSE)  
 
 
  load(path(myPlace,"/myData/",whichData,"datTract.R"))
  load(path(myPlace,"/myData/",whichData,"datComm.R"))
  load(path(myPlace,"/myData/",whichData,"datCounty.R"))
  load(path(myPlace,"/myData/",whichData,"datState.R"))
 
    
  load(path(myPlace,"/myData/","sdohTract.R"))
  load(path(myPlace,"/myData/","sdohComm.R"))
  load(path(myPlace,"/myData/","sdohCounty.R"))


#-- Load Info Files and Functions ------------------------------------------------------------------------
  
  # USE THIS 
  
  
  # new appraoch from Zev for simplifying path names, etc 
  # don't have to keep tract of leading or following "/" !
  # check to make sure this is supported on CDPH Shiny Server?
  
  gbdMap0    <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/gbd.ICD.Map.xlsx"), sheet="main"))  # OLD
  gbdMap0    <- as.data.frame(read_excel( path(myPlace,"myInfo////gbd.ICD.Map.xlsx/"), sheet="main"))    # NEW, with extra "/" as examples
  
  source(paste0(myPlace,"/myFunctions/helperFunctions/wrapSentence.R"))
  source(paste0(myPlace,"/myFunctions/helperFunctions/wrapLabels.R"))
  source(paste0(myPlace,"/myFunctions/helperFunctions/compass.R"))
  
  source(paste0(myPlace,"/myFunctions/cbdCutPoint0.R"))
  
  source(paste0(myPlace,"/myFunctions/cbdMap0.R"))
  source(paste0(myPlace,"/myFunctions/cbdMap-tmap.R"))
  source(paste0(myPlace,"/myFunctions/cbdMap0Leaflet.R"))
  
  source(paste0(myPlace,"/myFunctions/rankCausesSelectGeo.R")) 
  source(paste0(myPlace,"/myFunctions/rankCausesSelectGeoTable.R"))
  source(paste0(myPlace,"/myFunctions/rankGeosSelectCause.R"))
  
  source(paste0(myPlace,"/myFunctions/trend.R"))
  
  source(paste0(myPlace,"/myFunctions/scatterSDOH.R"))

  
  version <- "0.4.0"
  
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
lMeasures <- c("YLL","YLLper","Ndeaths","cDeathRate","aRate", "mean.age","SMR")

lMeasuresC <- c("Years of Life Lost (YLL)",
                "YLL per 100,000 population",
                "Number of deaths",
                "Crude Death Rate",
                "Age-Adjusted Death Rate",
                "Mean Age at Death",
                "Standard Mortality Ratio")

names(lMeasures) <- lMeasuresC

causeList36       <- gbdMap0[!is.na(gbdMap0$list36),c("gbdCode","nameOnly")]
causeList36       <- causeList36[order(causeList36[,2]),]
causeNum36        <- causeList36[,1]
names(causeNum36) <- causeList36[,2]#measVecN <- 1:3


sdohVecL  <- c("Less than Bachelors Degree","Below Federal Poverty",'HPI Raw Score')
sdohVec   <- c("lessBachelor","belowPov","hpiScore") 
names(sdohVec) <- sdohVecL

lList  <- sort(as.character(unique(datCounty$county)))
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