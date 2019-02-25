# -- INSTALL PACKAGES --------------------------------------------------------------------------

# The CBD, including the "Upstream" system, requires installation of the following packages 

# consider using the "librarian" package to install packages and load libraries
# https://github.com/DesiQuintans/librarian
# or perhaps anyLib, https://cran.r-project.org/web/packages/anyLib/anyLib.pdf




#-OTHER/Misc.------------------------------------------------------------------------------------
#  install.packages("tidyverse")  # data "wrangling" and other stuff
   install.packages("fs")         # for path() function -- # installed with tidyverse

#-DATA MANIPULATION------------------------------------------------------------------------------
  install.packages("dplyr")
  install.packages("data.table")

#-READING Data ----------------------------------------------------------------------------------
 install.packages("readr")   # read .csv and others -- # installed with tidyverse
 install.packages("haven")     # read SAS files       -- # installed with tidyverse
 install.packages("readxl")    # read Excel           -- # installed with tidyverse

#-MAPPING/GIS-------------------------------------------------------------------------------------
#  install.packages("maptools"); 
  install.packages("sf")         # to read and write shape files
  install.packages("tmap")
  #install.packages("leaflet") # installed by tmap
  install.packages("tidycensus")
  install.packages("tigris",dependencies = TRUE)
  install.packages("rmapshaper")
  install.packages("directlabels") # adding labels to lines in ggplot2
 #install.packages("maps");
 #install.packages("acs") # still used?
 
#-Analysis and Graphing Tools----------------------------------------------------------------------- 
  install.packages("classInt"); 
  install.packages("epitools")
  install.packages("sqldf")
  install.packages("plotly")
  install.packages("ggplot2")      # installed with tidyverse
 #install.packages("scatterD3")  # not used? 

#-For SHINY----------------------------------------------------------------------------------------- 
  install.packages("shiny")
  install.packages("shinythemes")
  install.packages("shinyWidgets")
  install.packages("shinyjs")
 #materials
 
 # this part is required for publication of the app to shiny.io and will be documented seperately
 # install.packages("rsconnect")
 # library(rsconnect)
 # ... connect shinyio via token...