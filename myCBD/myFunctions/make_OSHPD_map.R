# ==========================================================================================

# All these first lines are just for ease of building the function and exploration
# Change datComm and datCounty to the data set(s) you are working with...
# once the function is working, these lines should be deleted or commented out, since 
#   any needed data and shape file will be read in in "Global"

library(sf)
library(dplyr)
library(tmap)
library(classInt)
library(RColorBrewer)

myPlace       <- "d:/0.CBD/myCBD"
whichData     <- "fake/"
#datComm       <- readRDS(paste0(myPlace,"/myData/",whichData,"datComm.RDS")) No community level data for OSHPD at this time
age_adjusted_hosp_rates  <- readRDS(paste0(myPlace,"/myData/",whichData,"ageadj_hospratesOSHPD.rds")) %>% rename(ahospRate = measure) %>% select(-type)
#shape_Comm    <- st_read(paste0(myPlace,"/myData/shape_Comm.shp"),stringsAsFactors=FALSE) No community level data for OSHPD at this time
shape_County  <- st_read(paste0(myPlace,"/myData/shape_County.shp"),stringsAsFactors=FALSE)
STATE         <- "CALIFORNIA"
yearGrp       <- "2014-2018"

# Core wrapping function
wrap.it <- function(x, len)
{ sapply(x, function(y) paste(strwrap(y, len), collapse = "\n"), USE.NAMES = FALSE)}


# Call this function with a list or vector
wrap.labels <- function(x, len)
{if (is.list(x))   { lapply(x, wrap.it, len)
  } else {wrap.it(x, len)}
}


# ==========================================================================================

# don't execute these lines when first building the function ------
cbdMap  <- function(myLHJ,      # county or whole state = "CALIFORNIA"
                    myCause,    # not relevant for life expectancy 
                    myMeasure,  # not relvant for life expectancy -- for OSHPD use age adjusted rate
                    myYear,
                    mySex,
                    myGeo       # county or community (no "community" with OSHPD data--we should explore zip code at some point)
                    ) {
# -----------------------------------------------------------------

  

# do execute these lines when building/exploring the function -----
  if (1==2){
    myLHJ     = "CALIFORNIA"
    myCause     ="A"
    myMeasure = "ahospRate"
    myYear = 2016
    mySex     = "Total"
    myGeo     = "County"
    myStateCut  = TRUE      # not used in this map "blank"
    myLabName = FALSE       # not used in this map "blank"
    myCutSystem ="fisher"   # not used i nthis map "blank"
  }
# ---------------------------------------------------------------  
  
  # if myLHJ = "CALIFORNIA" then don't "zoom", otherwise do "zoom" -  see zoom's use a couple time below
  if (myLHJ != STATE) {        cZoom <- TRUE
                      } else { cZoom <-FALSE}
  
  
  # preparation for map title ----------------------------------
  geoLab <- ""
  if (cZoom) geoLab <- paste(" in",myLHJ)
  
  sexLab <- ""
  if (mySex != "Total") sexLab <- paste0(", among ",mySex,"s")
  # -------------------------------------------------------------
    
  if (myGeo == "County"){
    dat.1        <- filter(age_adjusted_hosp_rates,year==myYear,sex==mySex, CAUSE==myCause)  %>% mutate(geoLab = county)
    map.1        <- left_join(shape_County, dat.1, by=c("county")) # merge "shape file" with data 
    map.1$name   <- map.1$county     # this is used in "id" of tm_polygon below, but probably better way.... 
    myTitleYear  <- myYear           # preparation for map title
    myTitleGeo   <- " by County"     # preparation for map title
    }
    

  if (myGeo == "Community") {
    dat.1      <- filter(datComm,yearG5==yearGrp,sex==mySex, CAUSE==myCause,  comID != "Unknown") %>% 
                          mutate(geoLab = wrap.labels(comName,15))
    map.1      <- left_join(shape_Comm, dat.1, by=c("county","comID")) 
    map.1$name <- map.1$comName        # as above
    myTitleYear  <- yearGrp            # as above
    myTitleGeo   <- " by Community"    # as above
    }  
  
 # map title  
 myTitle  <- paste0("TEST", #deathMeasuresNames[deathMeasures == myMeasure]
                    " from ",#fullCauseList[fullCauseList[,"LABEL"]== myCause,"nameOnly"],
                    " in ",myTitleYear,myTitleGeo,sexLab,geoLab)
 myTitle  <- wrap.labels(myTitle,80)
  
 
 
 
 # if "zooming" to county, then just use data for that county
 if (cZoom) {map.1 <- map.1[map.1$county == myLHJ,]}

 
 # "no data/error" checking
 if (nrow(dat.1)==0) stop("Sorry friend, but thank goodness there are none of those OR all data are suppressed because of small numbers")

 # more funky "no data/error" checking
 temp  <- unique((map.1 %>% st_set_geometry(NULL))[,myMeasure]) 
 if ( length(temp)==1) (if (is.na(temp)) stop("Sorry, either no values or only suppressed values to map") )

 # long story here, but getting set to make "cut points" for the data/map/legend
 myRange   <- (map.1 %>% st_set_geometry(NULL))[,myMeasure]
 myBreaks  <- classIntervals(myRange,style="fisher",breaks=NULL,n=5)$brks


# tmaptools::palette_explorer()  # nice tool to see colors for tmap

 
myPal <- brewer.pal(5,"Blues") # rev(brewer.pal(5,"RdYlBu"))

if (myMeasure == "mean.age") myPal <- rev(myPal)

tt.map <-  tm_shape(map.1) + 
                   tm_polygons(col=myMeasure, palette = myPal, 
                               style="fixed", breaks=myBreaks,
                               colorNA="white",
                               title = "TEMP title" ,                  # deathMeasuresNames[deathMeasures == myMeasure]
                               textNA = "0 deaths/or suppressed",
                               interval.closure="right",
                               legend.reverse=T,
                             # popup.vars=c("Population: " = "pop",           # edit as appropiate for data set
                             #              "Measure Value: "= myMeasure),
                               title.col=NA,
                               id="name"
                              
                               ) +
                    tm_layout(frame=F,
                              main.title= myTitle,
                              main.title.size = 1.5,
                              fontface = 2,
                              inner.margins =c(0.1,0.02,0.02,0.02),  
                              legend.outside = TRUE,
                              legend.outside.position = "right", 
                              legend.title.size = 1, 
                              legend.text.size = 1,
                              legend.hist.height = .3 
                     ) 


 tmap_mode("view")
 
 tt.map <- tt.map + tm_view(basemaps = c("OpenStreetMap","Esri.WorldGrayCanvas","Esri.WorldTopoMap","Stamen.Watercolor"))
 
 #  tmap_leaflet(tt.map)
 tt.map
 
 }
  
#Testing function--need to change legend title issues
cbdMap(myLHJ     = "CALIFORNIA",
       myCause     ="C",
       myMeasure = "ahospRate",
       myYear = 2016,
       mySex     = "Total",
       myGeo     = "County")
