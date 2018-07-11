# test

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

#-- Set Locations Etc----------------------------------------------------------------------

# myDrive  <- "E:"  
# myPlace  <- paste0(myDrive,"/0.CBD/myCBD")  
# myLoc1   <- myPlace                             # used by some of myFunctions  --- no longer?                     
 
 myPlace  <- getwd()   # for Shiny.io
 
  
#-- Load Packages --------------------------------------------------------------------------

 #library(shinythemes)
 
 library(shiny)  
 library(dplyr)
 #library(maptools); 
 #library(maps);
 library(rgdal)
 library(leaflet); 
 library(classInt); #library(scatterD3); 
 library(RColorBrewer);
 library(epitools)
 library(readxl)

 library(plotly)

# library(shinymaterial)

# MIGRATE all mapping to tmap? 
 
# USE consistent map projection system throughout all app code !
proj1 <- "+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
proj2 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
 
# USE THIS 
# new appraoch from Zev for simplifying path names, etc 
# don't have to keep tract of leading or following "/" !
# check to make sure this is supported on CDPH Shiny Server?
 library(fs)
 gbdMap0    <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/gbd.ICD.Map.xlsx"), sheet="main"))  # OLD
 gbdMap0    <- as.data.frame(read_excel( path(myPlace,"myInfo////gbd.ICD.Map.xlsx/"), sheet="main"))    # NEW, with extra "/" as examples
  
 
 
 
 
 
# --- CBD Key Inputs --------------------------------------------------------------------------------------

 #  myProj             <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"                            # Key line for Happy GIS
 #  shape_County       <- readShapePoly(paste0(myPlace,"/myData/shape_County"),proj4string=CRS(myProj)) 
 #  shape_Comm         <- readShapePoly(paste0(myPlace,"/myData/shape_Comm"),proj4string=CRS(myProj))   # Read Shape Files
 #  
 # shape_Tract        <- readShapePoly(paste0(myPlace,"/myData/shape_Tract"),proj4string=CRS(myProj))  

 shape_County       <- readOGR(paste0(myPlace,"/myData/shape_County.shp"),p4s=proj1) 
 shape_Comm         <-readOGR(paste0(myPlace,"/myData/shape_Comm.shp"),p4s=proj1)   # Read Shape Files
 
 shape_Tract        <- readOGR(paste0(myPlace,"/myData/shape_Tract.shp"),p4s=proj1)  
 
 shape_Tract$GEOID  <- as.character(shape_Tract$GEOID)    
 shape_Tract$county <- as.character(shape_Tract$county)   
 
  #not quite...
  #library(sf)
  #shape_Tract <-  st_read(paste0(myPlace,"/myData/shape_Tract.shp"),stringsAsFactors = FALSE)  
 
  whichData  <-  "fake"
 
  load(path(myPlace,"/myData/",whichData,"datTract.R"))
  load(path(myPlace,"/myData/",whichData,"datComm.R"))
  load(path(myPlace,"/myData/",whichData,"datCounty.R"))
  load(path(myPlace,"/myData/",whichData,"datState.R"))
  
# --- Create "Sub-Set" Site: San Joaquin Public Health Consortium------------------------------------------

  mTitle       <- "California Community Burden of Disease and Costs 0.3.0"
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

    
#-- Load Info Files and Functions ------------------------------------------------------------------------
  
  gbdMap0    <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/gbd.ICD.Map.xlsx"), sheet="main"))

  source(paste0(myPlace,"/myFunctions/helperFunctions/wrapSentence.R"))
  source(paste0(myPlace,"/myFunctions/helperFunctions/wrapLabels.R"))
  source(paste0(myPlace,"/myFunctions/helperFunctions/compass.R"))
  
  source(paste0(myPlace,"/myFunctions/cbdCutPoint0.R"))
  
  source(paste0(myPlace,"/myFunctions/cbdMap0.R"))
  source(paste0(myPlace,"/myFunctions/cbdMap0Leaflet.R"))
  source(paste0(myPlace,"/myFunctions/rankCausesSelectGeo.R")) 
  source(paste0(myPlace,"/myFunctions/rankCausesSelectGeoTable.R"))
  source(paste0(myPlace,"/myFunctions/trend.R"))
  source(paste0(myPlace,"/myFunctions/rankGeosSelectCause.R"))
  
  
  source(paste0(myPlace,"/myFunctions/makeSES.R"))
  
  # --- Shiny Stuff and Constants ---------------------------------------------------------------------------

#lMeasures <- c("YLL","m.YLL","YLLper","Ndeaths","cDeathRate","aRate","med.age","SMR")
lMeasures <- c("YLL","m.YLL","YLLper","Ndeaths","cDeathRate","aRate",          "SMR")

lMeasuresC <- c("Years of Life Lost (YLL)",
                      "Mean YLL","Years of Life Lost per 100,000 population",
                      "Number of deaths","Crude Death Rate",
                      "Age-Adjusted Death Rate",
                      #   "Median Age",
                      "Standard Mortality Ratio")

names(lMeasures) <- lMeasuresC

causeList36       <- gbdMap0[!is.na(gbdMap0$list36),c("gbdCode","nameOnly")]
causeList36       <- causeList36[order(causeList36[,2]),]
causeNum36        <- causeList36[,1]
names(causeNum36) <- causeList36[,2]

lList  <- sort(as.character(unique(datCounty$county)))
if (sjc) {lList <- lList[lList %in% sjconsortium]}



nC        <- 5
myColor1  <- rev(brewer.pal(nC,"RdYlBu"))

yG <- "2011-2015"

 #myYear <- 2013
 myLHJ  <- "Colusa"; myLev <- 1; myCause <- 104   # myCause  <- "Diabetes mellitus"

# --- END --------------------------------------------------------------------------------------------------
