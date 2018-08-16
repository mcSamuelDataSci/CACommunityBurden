
# tmap:
# https://channel9.msdn.com/Events/useR-international-R-User-conferences/useR-International-R-User-2017-Conference/Exploring-and-presenting-maps-with-tmap


#for testing:
if (1==2){
myLHJ= "Amador"
myCause=0
myMeasure = "YLLper"
myYear=2015
myCon=TRUE
myGeo="Census Tract"
cZoom=TRUE
myLabName=FALSE
myLabNum=FALSE
}



cbdMapX <- function(myLHJ= "Amador", myCause=0,myMeasure = "YLLper", myYear=2015,myCon=TRUE,myGeo="Census Tract",cZoom=TRUE,myLabName=FALSE,myLabNum=FALSE) {

  #use these values to see the error bleow  
 # myLHJ = "Amador"; myCause=104; myYear=2015;myLabName=FALSE; myCon=TRUE;myGeo="Community";cZoom=TRUE;    myMeasure = "aRate";myLabNum=FALSE
  
  
    if( myGeo %in% c("Community","Census Tract") & myMeasure == "SMR" ) stop('Sorry kid, SMR calculated only for County level')
  
  
  if( myGeo == "County" & cZoom ) stop('Hey Buddy, use your noggin, you can not Zoom to County and keep the the Geo Level select as County')
  
  
  
    #county data for just 2011-2015
    dat.X   <- filter(datCounty,year %in% 2011:2015, CAUSE==myCause,Level == "gbd36",county !="CALIFORNIA STATE")
   
    if (myGeo == "County"){
    dat.1   <- filter(datCounty,year==myYear,CAUSE==myCause,Level == "gbd36")  #
    map.1   <- merge(shape_County, dat.1, by.x=c("county"), by.y = c("county"),all=TRUE) 
    yearLab <- myYear }
    
    if (myGeo == "Census Tract") { 
    dat.1    <- filter(datTract,yearG==yG,CAUSE==myCause,Level == "gbd36") 
    map.1    <- merge(shape_Tract, dat.1, by.x=c("county","GEOID"), by.y = c("county","GEOID"),all=TRUE) 
    yearLab  <- yG}

    if (myGeo == "Community") {
    dat.1    <- filter(datComm,yearG==yG,CAUSE==myCause, comID != "Unknown",Level == "gbd36")
    map.1    <- merge(shape_Comm, dat.1, by.x=c("county","comID"), by.y = c("county","comID"),all=TRUE) 
    yearLab <- yG  
    }  
  
  if (cZoom) {map.1   <- map.1[map.1$county == myLHJ,]}

  if (nrow(dat.1)==0) stop("Sorry friend, but thank goodness there are none of those; could be some other error")

 #  mydat <-     eval(parse(text=paste0("map.1$",myMeasure)))
 #  mydat[is.na(mydat)] <- 0
 # 
 # 
 # myrange <- c(0,mydat)
 #  myCuts   <- classIntervals(myrange, n=min(length(mydat),5),style = "fisher") ###ADDED n=5
 # # myCuts   <- classIntervals(myrange, n=5,style = "fisher") ###ADDED n=5
 #  
 # 
 #  
 #  if (myCon)   {
 #    myrange <- c(0,eval(parse(text=paste0("dat.X$",myMeasure))))
 #  #  myCuts   <- classIntervals(myrange, n = nC, style = "fisher",dataPrecision=0) 
 #    myCutsT <- myCuts
 #    myCuts   <- classIntervals(myrange, n=5,style = "fisher") ###ADDED n=5
 #    myCuts$brks[6] <- max(myCutsT$brks[length(myCutsT$brks)],myCuts$brks[length(myCuts$brks)])
 #  }
 #  
 #  
 
  
map.1$plotter <-  eval(parse(text=paste0("map.1$",myMeasure)))
map.1$plotter[is.na(map.1$plotter)] <- 0
  

#if (myMeasure=="med.age") {myColor1 <- rev(myColor1)}
palette(myColor1)

    
  library(tmap)
    #tmap_style("grey")
    tmap_style("classic")
    tm_shape(map.1) + tm_polygons("plotter",title=paste(myMeasure))  
    
    
  
}

#fortesting
if (1==2){

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

