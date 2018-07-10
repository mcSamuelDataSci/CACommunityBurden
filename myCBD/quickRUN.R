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
 
 

  load(paste0(myPlace,"/myData/datTract.R"))
  load(paste0(myPlace,"/myData/datComm.R"))
  load(paste0(myPlace,"/myData/datCounty.R"))
  load(paste0(myPlace,"/myData/datState.R"))

  
  
  
  
countyVP <- filter(datCounty, causeName %in% c("All CAUSES","Self-harm","Interpersonal violence"))%>%
            select(-m.YLL,-Level)
  


tractAllCause <- filter(datTract, CAUSE %in% c(0))%>%
  select(-m.YLL,-Level)


  
write.csv(tractAllCause,file="tractAllCause.csv")  
  
  