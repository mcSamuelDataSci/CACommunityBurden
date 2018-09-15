library(tmap)

# values that can be used for testing code "outside" shiny:
if (1==2){
myLHJ= "Amador"
myCause="A"
myMeasure = "YLLper"
myYear=2015
mySex="Female"
myStateCut=TRUE
myGeo="County"
cZoom=TRUE
myLabName=FALSE
myLabNum=FALSE
myCutSystem="fisher"
}



# Level == "lev1",

cbdMapX <- function(myLHJ= "Amador", myCause="A",myMeasure = "YLLper", myYear=2015,mySex="Total",myStateCut=TRUE,myGeo="Census Tract",cZoom=FALSE,myLabName=FALSE,myCutSystem="fisher") {

    if( myGeo %in% c("Community","Census Tract") & myMeasure == "SMR" ) stop('Sorry kid, SMR calculated only for County level')
  
  #county data for just 2011-2015 needed when myStateCut = TRUE -- to be addressed soon
    dat.X   <- filter(datCounty,year %in% 2011:2015, sex==mySex, CAUSE==myCause, county !="CALIFORNIA")
   
    if (myGeo == "County"){
    dat.1   <- filter(datCounty,year==myYear,sex==mySex, CAUSE==myCause)  
    map.1   <- merge(shape_County, dat.1, by.x=c("county"), by.y = c("county"),all=TRUE) 
    yearLab <- myYear }
    
    if (myGeo == "Census Tract") { 
    dat.1    <- filter(datTract,yearG==yG,sex==mySex, CAUSE==myCause) 
    map.1    <- merge(shape_Tract, dat.1, by.x=c("county","GEOID"), by.y = c("county","GEOID"),all=TRUE) 
    yearLab  <- yG}

    if (myGeo == "Community") {
    dat.1    <- filter(datComm,yearG==yG,sex==mySex, CAUSE==myCause,  comID != "Unknown")
    map.1    <- merge(shape_Comm, dat.1, by.x=c("county","comID"), by.y = c("county","comID"),all=TRUE) 
    yearLab <- yG  
    }  
  
  if (cZoom) {map.1 <- map.1[map.1$county == myLHJ,]}

  if (nrow(dat.1)==0) stop("Sorry friend, but thank goodness there are none of those; could be some other error")

 #  myrange <- c(0,mydat)
 #  myCuts   <- classIntervals(myrange, n=min(length(mydat),5),style = "fisher") ###ADDED n=5
 #      
 #  if (myStateCut)   {
 #    myrange <- c(0,eval(parse(text=paste0("dat.X$",myMeasure))))
 #  #  myCuts   <- classIntervals(myrange, n = nC, style = "fisher",dataPrecision=0) 
 #    myCutsT <- myCuts
 #    myCuts   <- classIntervals(myrange, n=5,style = "fisher") ###ADDED n=5
 #    myCuts$brks[6] <- max(myCutsT$brks[length(myCutsT$brks)],myCuts$brks[length(myCuts$brks)])
 #  }

map.1$plotter <- eval(parse(text=paste0("map.1$",myMeasure)))
map.1$plotter[is.na(map.1$plotter)] <- 0
  

stateCutter <- eval(parse(text=paste0("dat.X$",myMeasure)))

palette(myColor1)
#tmap_style("white")

# temporary fix
if (myCutSystem == "numeric") myCutSystem <- "pretty" 

# coLab <- myLHJ
# if (!cZoom) coLab <- "California"

# can't use this -- "Bug"/throws error if only one value (e.g. cZoom and County geography)
#,legend.hist=T

sexLabel <- ""
if (mySex != "Total") sexLabel <- paste0("among ",mySex,"s")

myBreaks <- NULL

if (myStateCut) {
  myCutSystem <- "fixed"
  myBreaks    <- c(0,classIntervals(stateCutter)$brks)
}


 
 tm_shape(map.1) + tm_polygons(col="plotter",title=paste(lMeasuresC[lMeasures==myMeasure]),style=myCutSystem,breaks=myBreaks,colorNA="white")  + 
  tm_layout(main.title= paste(lMeasuresC[lMeasures==myMeasure],"from",causeList36[causeList36[,"LABEL"]== myCause,"nameOnly"],"in",yearLab,sexLabel),
            legend.outside = TRUE,
            legend.outside.position = "right"
            #,
            #legend.title.size = 1, legend.text.size = 1,legend.hist.height = .3
            )
}

#,causeList36[causeList36[,"gbdCode"]== myCause,"nameOnly"]



cbdMapXStat <- function(myLHJ= "Amador", myCause="A",myMeasure = "YLLper", myYear=2015,mySex="Total",myStateCut=TRUE,myGeo="Census Tract",cZoom=FALSE,myLabName=FALSE,myCutSystem="fisher") {
  tmap_mode("plot")
  cbdMapX(myLHJ, myCause,myMeasure, myYear, mySex, myStateCut,myGeo,cZoom,myLabName,myCutSystem)
  
}

cbdMapXLeaf <- function(myLHJ= "Amador", myCause="A",myMeasure = "YLLper", myYear=2015,mySex="Total",myStateCut=TRUE,myGeo="Census Tract",cZoom=FALSE,myLabName=FALSE,myCutSystem="fisher") {
   tmap_mode("view")
   tt.map <-  cbdMapX(myLHJ, myCause,myMeasure, myYear, mySex, myStateCut,myGeo,cZoom,myLabName,myCutSystem)
   tmap_leaflet(tt.map)
  
}
  
 #?? tm_basemap function not there???
  # tm_basemap("Stamen.Watercolor") + 
# FOR TESTING ----------------------------------------------------------------------------------------------


if (1==2){


# devtools::install_github("statnmap/HatchedPolygons")

library(HatchedPolygons);


cal.gono #spatial polygon data frame;

cal.gono.hatch<-hatched.SpatialPolygons(map.1,density=0.001,angle=45);


proj4string(cal.gono.hatch)<-proj4string(cal.gono);

tm_shape(cal.gono)+tm_polygon()+tm_shape(cal.gono.hatch)+tm_lines(col="grey");


mapx <- cbdMapX()
tmap_mode("plot")

# plot map
mapx

# view map with default view options
tmap_mode("view")
mapx
mapx + tm_view(alpha = 1, basemaps = "Stamen.Watercolor")

# restore current mode
tmap_mode("plot")

}

# OLD STUFF - NOT USED FOR NOW --------------------------------------------------------------------------------

#if (myMeasure=="med.age") {myColor1 <- rev(myColor1)}

