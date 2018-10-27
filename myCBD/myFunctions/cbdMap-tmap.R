
cbdMapX <- function(myLHJ     = "Amador", myCause     = "A",  myMeasure = "YLLper",       myYear = 2015,
                    mySex     = "Total",  myStateCut  = TRUE, myGeo     = "Census Tract", 
                    myLabName = FALSE,    myCutSystem ="fisher") {

    if( myGeo %in% c("Community","Census Tract") & myMeasure == "SMR" ) stop('Sorry kid, SMR calculated only for County level')
  
  
  
    dat.State   <- filter(datCounty,year %in% 2013:2017, sex==mySex, CAUSE==myCause, county !="CALIFORNIA")

   
    if (myLHJ != STATE) {        cZoom <- TRUE
                        } else { cZoom <-FALSE}
    
    geoLab <- ""
    if (cZoom) geoLab <- paste(" in",myLHJ)
    
    sexLab <- ""
    if (mySex != "Total") sexLab <- paste0(", among ",mySex,"s")
    
    if (myGeo == "County"){
    dat.1   <- filter(datCounty,year==myYear,sex==mySex, CAUSE==myCause)  %>% mutate(geoLab = county)
    map.1   <- left_join(shape_County, dat.1, by=c("county")) 
    map.1$name <- map.1$county
    myTitYear <- myYear
    myTitGeo  <- " by County"
    }
  
    if (myGeo == "Census Tract") { 
    dat.1    <- filter(datTract,yearG==yearGrp,sex==mySex, CAUSE==myCause)  %>% mutate(geoLab = GEOID)
    map.1    <- left_join(shape_Tract, dat.1, by=c("county","GEOID"))
    map.1$name <- map.1$GEOID
    myTitYear <- yearGrp
    myTitGeo  <- " by Tract"
    }

    if (myGeo == "Community") {
    dat.1    <- filter(datComm,yearG==yearGrp,sex==mySex, CAUSE==myCause,  comID != "Unknown") %>% mutate(geoLab = comName)
    map.1    <- left_join(shape_Comm, dat.1, by=c("county","comID")) 
    map.1$name <- map.1$comName
    myTitYear <- yearGrp
    myTitGeo  <- " by Community"
    }  
  
  myTit    <- paste0(lMeasuresC[lMeasures==myMeasure]
                     
                     
                     ," from ",causeList36[causeList36[,"LABEL"]== myCause,"nameOnly"]," in ",myTitYear,myTitGeo,sexLab,geoLab)
  
  if (cZoom) {map.1 <- map.1[map.1$county == myLHJ,]}

  if (nrow(dat.1)==0) stop("Sorry friend, but thank goodness there are none of those; could be some other error")

#palette(myColor1)
#tmap_style("white")

# sexLabel <- ""
# if (mySex != "Total") sexLabel <- paste0("among ",mySex,"s")

nCut <- 5

if ( myStateCut) {myRange <- dat.State[,myMeasure]}
if (!myStateCut) {myRange <- dat.1[,myMeasure]}
# fix NAs ?

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
myPal <- rev(brewer.pal(5,"RdYlBu"))
myPal <- add.alpha(myPal,.7)
#myPal <- c("#D7191C","#FDAE61","#FFFFBF","#ABD9E9","#2C7BB6")
 #  , aes.palette=list(div="RdYlBu")
#myPal <- rev(get_brewer_pal("RdYlB", n = 5, contrast = c(0, 0.7)))


if (myMeasure == "mean.age") myPal <- rev(myPal)



# https://stackoverflow.com/questions/8161836/how-do-i-replace-na-values-with-zeros-in-an-r-dataframe
replace_missings <- function(x, replacement) {
  is_miss <- is.na(x)
  x[is_miss] <- replacement
  
  message(sum(is_miss), " missings replaced by the value ", replacement)
  x
}


map.1 <- replace_missings(map.1, replacement = 0)










#https://rdrr.io/cran/tmap/man/tm_symbols.html

 tm_shape(map.1) + tm_polygons(col=myMeasure,palette = myPal, style="fixed",breaks=myBreaks,colorNA="white",
                               title = lMeasuresC[lMeasures==myMeasure],
                               textNA = "0 deaths/or supressed",
                               interval.closure="right",
                               legend.hist=T,
                               legend.reverse=T,
                               #legend.format=text.less.than,
                               title.col=NA,id="name", 
                               popup.vars=c("Population: " = "pop",
                                            "Measure Value: "= myMeasure)
                                            ) +
   tm_layout(frame=F,main.title= myTit,main.title.size = 1.5,fontface = 2,
           legend.outside = TRUE,
           legend.outside.position = "right", 
           legend.title.size = 1, legend.text.size = 1,legend.hist.height = .3
          )  
  
  
 
 
}


cbdMapXStat <- function(myLHJ= "Amador", myCause="A",myMeasure = "YLLper", myYear=2015,mySex="Total",myStateCut=TRUE,myGeo="Census Tract",myLabName=FALSE,myCutSystem="fisher") {
  tmap_mode("plot")
  
  tt.map <- cbdMapX(myLHJ, myCause,myMeasure, myYear, mySex, myStateCut,myGeo,myLabName,myCutSystem)   
    
      if (myLabName) tt.map <- tt.map + tm_text(wrap.labels("geoLab",10)) 
    
  tt.map 
  }

cbdMapXLeaf <- function(myLHJ= "Amador", myCause="A",myMeasure = "YLLper", myYear=2015,mySex="Total",myStateCut=TRUE,myGeo="Census Tract",myLabName=FALSE,myCutSystem="fisher") {
   tmap_mode("view")
   tt.map <-  cbdMapX(myLHJ, myCause,myMeasure, myYear, mySex, myStateCut,myGeo,myLabName,myCutSystem)
   tmap_leaflet(tt.map)
}
  
 #?? tm_basemap function not there???
  # tm_basemap("Stamen.Watercolor") + 
# FOR TESTING ----------------------------------------------------------------------------------------------


#values that can be used for testing code "outside" shiny:
if (1==2){
  myLHJ= "Amador"
  myCause="0"
  myMeasure = "YLLper"
  myYear=2015
  mySex="Female"
  myStateCut=TRUE
  myGeo="County"
  cZoom=FALSE
  myLabName=FALSE
  myLabNum=FALSE
  myCutSystem="fisher"
  Level = "lev1"
}


# if (1==2){
# # devtools::install_github("statnmap/HatchedPolygons")
# library(HatchedPolygons);
# cal.gono #spatial polygon data frame;
# cal.gono.hatch<-hatched.SpatialPolygons(map.1,density=0.001,angle=45);
# proj4string(cal.gono.hatch)<-proj4string(cal.gono);
# tm_shape(cal.gono)+tm_polygon()+tm_shape(cal.gono.hatch)+tm_lines(col="grey");
# mapx <- cbdMapX()
# tmap_mode("plot")
# 
# # plot map
# mapx
# 
# # view map with default view options
# tmap_mode("view")
# mapx
# mapx + tm_view(alpha = 1, basemaps = "Stamen.Watercolor")

# restore current mode
tmap_mode("plot")

#}

# OLD STUFF - NOT USED FOR NOW --------------------------------------------------------------------------------

#if (myMeasure=="med.age") {myColor1 <- rev(myColor1)}

