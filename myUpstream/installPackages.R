# -- INSTALL PACKAGES --------------------------------------------------------------------------

#test

# The CBD, including the "Upstream" system, requires installation of the following packages 
 

#-OTHER/Misc.------------------------------------------------------------------------------------
  install.packages("tidyverse")  # data "wrangling" and other stuff
  install.packages("fs")         # for path() function

#-DATA MANIPULATION------------------------------------------------------------------------------
 #install.packages("dplyr")

#-READING Data ----------------------------------------------------------------------------------
 #install.packages("readr")   # read .csv and others -- # installed with tidyverse
 #install.packages(haven)     # read SAS files -- # installed with tidyverse
 #install.packages(readxl)    # read Excel -- # installed with tidyverse

#-MAPPING/GIS-------------------------------------------------------------------------------------
  install.packages("maptools"); 
  install.packages("sf")         # to read and write shape files
  install.packages("tmap")
  install.packages("leaflet") ;
  install.packages("tidycensus")
  install.packages("tigris")
  install.packages("rmapshaper")
 #install.packages("maps");
 #install.packages("acs") # still used?
 
#-Analysis and Graphing Tools----------------------------------------------------------------------- 
  install.packages("classInt"); 
  install.packages("epitools")
  install.packages("sqldf")
  install.packages("plotly")
 #install.packages(ggplot2)      # installed with tidyverse
 #install.packages("scatterD3")  # not used? 

#-For SHINY----------------------------------------------------------------------------------------- 
  install.packages("shiny")
  install.packages("shinythemes")
  install.packages("shinyWidgets")
 #materials
 
 # this part is required for publication of the app to shiny.io and will be documented seperately
 # install.packages("rsconnect")
 # library(rsconnect)
 # ... connect shinyio via token...