
cbdMap <- function(myLHJ     = "Alameda", myCause     = "A01",   myMeasure = "YLLper",       myYear = 2015,
                    mySex     = "Total",   myStateCut  = TRUE,  myGeo     = "Community", 
                    myLabName = FALSE,     myCutSystem ="fisher") {

  
  if (1==2){
    myLHJ     = "Aamador";myCause     ="A01";myMeasure = "YLLper";  myYear = 2015;
    mySex     = "Total";myStateCut  = TRUE; myGeo     = "County";
    myLabName = FALSE;  myCutSystem ="fisher" 
  }
  
  
  # "blank" map to use below
  countyPop <- datCounty %>% filter(year == 2017,sex=="Total", CAUSE=="0") %>%
                  select(county,population)
  
  if( myGeo %in% c("Community","Census Tract") & myMeasure == "SMR" ) stop('Sorry kid, SMR calculated only for County level')
  
  if (myLHJ != STATE) {        cZoom <- TRUE
                      } else { cZoom <-FALSE}
  
  geoLab <- ""
  if (cZoom) geoLab <- paste(" in",myLHJ)
    
  sexLab <- ""
  if (mySex != "Total") sexLab <- paste0(", among ",mySex,"s")
    
  if (myGeo == "County"){
    dat.State  <- filter(datCounty,(myYear>= 2012 & myYear <= 2017),sex==mySex, CAUSE==myCause)  %>% mutate(geoLab = county)  
    dat.1      <- filter(datCounty,year==myYear,sex==mySex, CAUSE==myCause)  %>% mutate(geoLab = county) %>% select(-population)
    dat.1      <- left_join(countyPop,dat.1,by="county")
    map.1      <- left_join(shape_County, dat.1, by=c("county")) 
    map.1$name <- map.1$county
    myTitYear  <- myYear
    myTitGeo   <- " by County"
    }
  
  if (myGeo == "Census Tract") { 
    dat.1      <- filter(datTract,yearG5==yearGrp,sex==mySex, CAUSE==myCause)  %>% mutate(geoLab = GEOID)
    map.1      <- left_join(shape_Tract, dat.1, by=c("county","GEOID"))
    map.1$name <- map.1$GEOID
    myTitYear  <- yearGrp
    myTitGeo   <- " by Tract"
    }

  if (myGeo == "Community") {
    dat.1      <- filter(datComm,yearG5==yearGrp,sex==mySex, CAUSE==myCause,  comID != "Unknown") %>% 
                          mutate(geoLab = wrap.labels(comName,15))
    map.1      <- left_join(shape_Comm, dat.1, by=c("county","comID")) 
    map.1$name <- map.1$comName
    myTitYear  <- yearGrp
    myTitGeo   <- " by Community"
    }  
  
 myTit  <- paste0(deathMeasuresNames[deathMeasures == myMeasure],
                    " from ",fullCauseList[fullCauseList[,"LABEL"]== myCause,"nameOnly"],
                    " in ",myTitYear,myTitGeo,sexLab,geoLab)
 myTit  <- wrap.labels(myTit,80)
  
 if (cZoom) {map.1 <- map.1[map.1$county == myLHJ,]}

 
# HELP why doesn't this one line stop the function when Amador and TB????
# CRASHES!
# message is generated, but code still runs somewhere???
if (nrow(dat.1)==0) stop("Sorry friend, but thank goodness there are none of those OR all data are suppressed because of small numbers")

 nCut <- 5

 if (myStateCut & myGeo == "county") {myRange <- dat.State[,myMeasure]}
 if (myStateCut & myGeo != "county") {myRange <- dat.1[,myMeasure]}
 if (!myStateCut)                    {myRange <- (map.1 %>% st_set_geometry(NULL))[,myMeasure]}

# fix NAs ?
## WORKING ON FIX
# HELP
# with these two lines below in
# app CRASHES when using trend fuction with TB and Amador

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
myPal <- add.alpha(myPal,.8)

if (myMeasure == "mean.age") myPal <- rev(myPal)

map.1 <- mutate(map.1,myMeasure=ifelse(is.na(myMeasure),0,myMeasure))

mapX <-  tm_shape(map.1) + tm_polygons(col=myMeasure, palette = myPal, 
                               style="fixed", breaks=myBreaks,
                               colorNA="white",
                               title = deathMeasuresNames[deathMeasures == myMeasure],
                               textNA = "0 deaths/or suppressed",
                               interval.closure="right",
                              #legend.hist=T,
                               legend.reverse=T,
                              #legend.format=text.less.than,
                               title.col=NA,id="name", 
                               popup.vars=c("Population: " = "population",
                                            "Measure Value: "= myMeasure)
                               ) +
                    tm_layout(frame=F,
                              main.title= myTit,
                              main.title.size = 1.5,
                              main.title.color = "green", #myTitleColor,
                              title.color = "yellow",
                              fontface = 2,
                              inner.margins =c(0.1,0.02,0.02,0.02),  
                              legend.outside = TRUE,
                              legend.outside.position = "right", 
                              legend.title.size = 1, 
                              legend.text.size = 1,
                              legend.hist.height = .3 
                     ) 
 

 # Interative Map to Display -------------------------------
 tmap_mode("view")
 tt.map <-  mapX + 
             tm_view(control.position = c("left","bottom"),
                     basemaps = c("OpenStreetMap","Esri.WorldGrayCanvas","Esri.WorldTopoMap","Stamen.Watercolor"))
 tplot <- tmap_leaflet(tt.map)
 
 # Static Map to Download ----------------------------------
 tmap_mode("plot")
 if (myLabName) mapX <- mapX + tm_text("geoLab") 
 tt.map.STAT <- mapX + 
                 tm_credits(figureAttribution,position=c("center","BOTTOM")) +
                 tm_compass(type="arrow")
 
 # --------------------------------------------------------
 
 # FIX: "year",
 varsIn  <- c("GEOID"="name","county","sex",myMeasure) 
 tabDat  <- map.1 %>%  st_drop_geometry()    %>%   select(varsIn)
 
 list(leafPlot = tplot, plotL = tt.map.STAT, dataL = tabDat)
 
}
  
