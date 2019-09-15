
cbdMapX <- function(myLHJ,      # county or whole state = "CALIFORNIA"
                    myCause,    # not relevant for life expectancy 
                    myMeasure,  # not relvant for life expactancy -- for OSHPD use age adjusted rate
                    myYear,
                    mySex,
                    myGeo       # county or community (or maybe zip code with OSHPD data)
                    ) {

  if (1==2){
    myLHJ     = "Alameda"
    myCause     ="A"
    myMeasure = "YLLper"
    myYear = 2015
    mySex     = "Total"
    myGeo     = "Community"
    myStateCut  = TRUE      # not used in this map "blank"
    myLabName = FALSE       # not used in this map "blank"
    myCutSystem ="fisher"   # not used i nthis map "blank"
  }
 
  
  # if myLHJ = "CALIFORNIA" then don't "zoom", otherwise do "zoom" -  see zoom a couple time below
  if (myLHJ != STATE) {        cZoom <- TRUE
                      } else { cZoom <-FALSE}
  
  
  # preparation for map title ----------------------------------
  geoLab <- ""
  if (cZoom) geoLab <- paste(" in",myLHJ)
  
  sexLab <- ""
  if (mySex != "Total") sexLab <- paste0(", among ",mySex,"s")
  # -------------------------------------------------------------
    
  if (myGeo == "County"){
    dat.1        <- filter(datCounty,year==myYear,sex==mySex, CAUSE==myCause)  %>% mutate(geoLab = county)
    map.1        <- left_join(shape_County, dat.1, by=c("county")) # merge "shape file" with data 
    map.1$name   <- map.1$county     # this is used in "id" of tm_polygon below, but probably better way.... 
    myTitleYear  <- myYear           # preparation for map title
    myTitleGeo   <- " by County"     # preparation for map title
    }
    

  if (myGeo == "Community") {
    dat.1      <- filter(datComm,yearG5=="2014-2018",sex==mySex, CAUSE==myCause,  comID != "Unknown") %>% 
                          mutate(geoLab = wrap.labels(comName,15))
    map.1      <- left_join(shape_Comm, dat.1, by=c("county","comID")) 
    map.1$name <- map.1$comName        # as above
    myTitleYear  <- yearGrp            # as above
    myTitleGeo   <- " by Community"    # as above
    }  
  
 # map title  
 myTitle  <- paste0(deathMeasuresNames[deathMeasures == myMeasure],
                    " from ",fullCauseList[fullCauseList[,"LABEL"]== myCause,"nameOnly"],
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
                             # popup.vars=c("Population: " = "pop",
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
 
 tmap_leaflet(tt.map)

 }
  