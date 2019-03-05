
# MAIN mapping function used by both Interactive and Static mapping functions

cbdMapX <- function(myLHJ     = "Alameda", myCause     = "A",   myMeasure = "YLLper",       myYear = 2015,
                    mySex     = "Total",   myStateCut  = TRUE,  myGeo     = "Census Tract", 
                    myLabName = FALSE,     myCutSystem ="fisher") {

  if (1==2){
    myLHJ     = "Alpine";myCause     ="A";myMeasure = "YLLper";  myYear = 2015;
    mySex     = "Total";myStateCut  = TRUE; myGeo     = "Community";
    myLabName = FALSE;  myCutSystem ="fisher" 
  }
  
  if( myGeo %in% c("Community","Census Tract") & myMeasure == "SMR" ) stop('Sorry kid, SMR calculated only for County level')
  
  if (myLHJ != STATE) {        cZoom <- TRUE
                      } else { cZoom <-FALSE}
  
  geoLab <- ""
  if (cZoom) geoLab <- paste(" in",myLHJ)
    
  sexLab <- ""
  if (mySex != "Total") sexLab <- paste0(", among ",mySex,"s")
    
  if (myGeo == "County"){
    dat.State  <- filter(datCounty,(myYear>= 2012 & myYear <= 2017),sex==mySex, CAUSE==myCause)  %>% mutate(geoLab = county)  
    dat.1      <- filter(datCounty,year==myYear,sex==mySex, CAUSE==myCause)  %>% mutate(geoLab = county)
    map.1      <- left_join(shape_County, dat.1, by=c("county")) 
    map.1$name <- map.1$county
    myTitYear  <- myYear
    myTitGeo   <- " by County"
    }
  
  if (myGeo == "Census Tract") { 
    dat.1      <- filter(datTract,yearG==yearGrp,sex==mySex, CAUSE==myCause)  %>% mutate(geoLab = GEOID)
    map.1      <- left_join(shape_Tract, dat.1, by=c("county","GEOID"))
    map.1$name <- map.1$GEOID
    myTitYear  <- yearGrp
    myTitGeo   <- " by Tract"
    }

  if (myGeo == "Community") {
    dat.1      <- filter(datComm,yearG==yearGrp,sex==mySex, CAUSE==myCause,  comID != "Unknown") %>% 
                          mutate(geoLab = wrap.labels(comName,15))
    map.1      <- left_join(shape_Comm, dat.1, by=c("county","comID")) 
    map.1$name <- map.1$comName
    myTitYear  <- yearGrp
    myTitGeo   <- " by Community"
    }  
  
 myTit  <- paste0(lMeasuresC[lMeasures==myMeasure],
                    " from ",fullCauseList[fullCauseList[,"LABEL"]== myCause,"nameOnly"],
                    " in ",myTitYear,myTitGeo,sexLab,geoLab)
 myTit  <- wrap.labels(myTit,80)
  
 if (cZoom) {map.1 <- map.1[map.1$county == myLHJ,]}

 if (nrow(dat.1)==0) stop("Sorry friend, but thank goodness there are none of those OR all data are suppressed because of small numbers")

 nCut <- 5

 if (myStateCut & myGeo == "county") {myRange <- dat.State[,myMeasure]}
 if (myStateCut & myGeo != "county") {myRange <- dat.1[,myMeasure]}
 if (!myStateCut)                    {myRange <- (map.1 %>% st_set_geometry(NULL))[,myMeasure]}

# fix NAs ?

temp  <- unique((map.1 %>% st_set_geometry(NULL))[,myMeasure]) 
if ( length(temp)==1) (if (is.na(temp)) stop("Sorry, either no values or only suppressed values to map") )

myBreaks    <- classIntervals(myRange,style=myCutSystem,breaks=NULL,n=nCut)$brks

samVec <- c(0,5,15,35,65,85,95)/100

add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}

#tmaptools::palette_explorer()

myPal <- brewer.pal(5,"Blues") # rev(brewer.pal(5,"RdYlBu"))
myPal <- add.alpha(myPal,.7)

if (myMeasure == "mean.age") myPal <- rev(myPal)


 tm_shape(map.1) + tm_polygons(col=myMeasure, palette = myPal, 
                               style="fixed", breaks=myBreaks,
                               colorNA="white",
                               title = lMeasuresC[lMeasures==myMeasure],
                               textNA = "0 deaths/or suppressed",
                               interval.closure="right",
                              #legend.hist=T,
                               legend.reverse=T,
                              #legend.format=text.less.than,
                               title.col=NA,id="name", 
                               popup.vars=c("Population: " = "pop",
                                            "Measure Value: "= myMeasure)
                               ) +
                    tm_layout(frame=F,
                              main.title= myTit,
                              main.title.size = 1.5,
                              fontface = 2,
                              inner.margins =c(0.1,0.02,0.02,0.02),  
                              legend.outside = TRUE,
                              legend.outside.position = "right", 
                              legend.title.size = 1, 
                              legend.text.size = 1,
                              legend.hist.height = .3 
                     ) 
 }


# Funtion used for Static Map ------------------------------------------------------------------------
cbdMapXStat <- function(myLHJ= "Amador", myCause="A", myMeasure = "YLLper", myYear=2015, mySex="Total",
                        myStateCut=TRUE,myGeo="Census Tract", myLabName=FALSE,myCutSystem="fisher") {

 tmap_mode("plot")
  
 tt.map <- cbdMapX(myLHJ, myCause,myMeasure, myYear, mySex, myStateCut,myGeo,myLabName,myCutSystem)   
    
 if (myLabName) tt.map <- tt.map + tm_text("geoLab") 
  
 tt.map <- tt.map + 
           tm_credits(figureAttribution,position=c("center","BOTTOM")) +
           tm_compass(type="arrow")
           tt.map 
  }


# Funtion used for Interactive Map --------------------------------------------------------------------

cbdMapXLeaf <- function(myLHJ= "Amador", myCause="A", myMeasure = "YLLper", myYear=2015, mySex="Total",
                        myStateCut=TRUE,myGeo="Census Tract", myLabName=FALSE,myCutSystem="fisher") {

 tmap_mode("view")
 
 tt.map <-  cbdMapX(myLHJ, myCause,myMeasure, myYear, mySex, myStateCut,myGeo,myLabName,myCutSystem)

 tt.map <- tt.map + tm_view(basemaps = c("OpenStreetMap","Esri.WorldGrayCanvas","Esri.WorldTopoMap","Stamen.Watercolor"))
 
 tmap_leaflet(tt.map)
}
  

# NOTES etc --------------------------------------------------------------------------------------------


# basemaps
# # tm_basemap("Stamen.Watercolor")  


# restore current mode
#tmap_mode("plot")

