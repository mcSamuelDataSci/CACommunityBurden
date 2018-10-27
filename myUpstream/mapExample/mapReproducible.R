# Make California Tract "Shape" file
library(tigris)
options(tigris_class = "sf")  
caTract  <- tracts(state = "CA", cb = TRUE)  

# Read file that links tracts to "communities"
cbdLinkCA  <- read.csv("https://raw.githubusercontent.com/mcSamuelDataSci/CACommunityBurden/master/myCBD/myInfo/cbdLinkCA.csv",colClasses = "character") 


# merge Map and link file
library(dplyr)
caTract <- caTract %>% 
           left_join(cbdLinkCA, by="GEOID")

# new "Shape" file based on communites
shape_Comm  <- caTract %>% 
               group_by(county,comName) %>% 
               summarize() %>% 
               ungroup()

# Filter to one county for example
shape_Comm  <- filter(shape_Comm,county=="Alameda")


# Make dumb map with community labels
library(tmap)
   tm_shape(shape_Comm) + 
    tm_polygons(col="county") + 
     tm_text("comName")


# == TEXT WRAPPING FUNCTION I GOT FROM STACK OVERFLOW AND USE OFTEN ===========

#http://stackoverflow.com/questions/20241065/r-barplot-wrapping-long-text-labels

# Core wrapping function
wrap.it <- function(x, len)
{ 
  sapply(x, function(y) paste(strwrap(y, len), 
                              collapse = "\n"), 
         USE.NAMES = FALSE)
}

# Call this function with a list or vector
wrap.labels <- function(x, len)
{
  if (is.list(x))
  {
    lapply(x, wrap.it, len)
  } else {
    wrap.it(x, len)
  }
}

# =============================================================================

# try to make map with wrapped labels
# nope!
 tm_shape(shape_Comm) + 
  tm_polygons(col="county") + 
   tm_text(wrap.labels("comName",15))














