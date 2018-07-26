# -- INSTALL PACKAGES ----------------------------------------------------------------------
# -----------------------------------------------------------------------------------------
# CBD including Upstream requires installation of the following packages 
 
  install.packages("tidyverse") # OR install individual packages (much quicker)
 #install.packages("dplyr")   # data manipulation
 #Reading Data
 #install.packages("readr")   # read .csv and others
 #install.packages(haven)     # read SAS files
 #install.packages(readxl)    # read Excel
 #Graphing
 #install.packages(ggplot2)

  # MAPPING/GIS
 install.packages("maptools"); 
 #install.packages("maps");
  install.packages("sf")         # to read and write shape files
  install.packages("tmap")
  install.packages("leaflet") ;
  install.packages("tidycensus")
  install.packages("tigris")
  install.packages("rmapshaper")
 #install.packages("acs") # still used?
 
#Analysis and Graphing Tools 
  install.packages("classInt"); 
 #install.packages("scatterD3") ; # not used?
  install.packages("epitools")
  install.packages("sqldf")
  install.packages("plotly")
 
  install.packages("fs")         # for path() function
  
     
#SHINY
  install.packages(shiny)
  install.packages(shinythemes)
  install.packages(shinyWidgets)
 
 
 
 
 
 
 

 # materials
 
 
 
 # -----------------------------------------------------------------------------------------

 # this part is required for publication of the app to shiny.io and will be documented seperately
 # install.packages("rsconnect")
 # library(rsconnect)
 #... connect shinyio via token...