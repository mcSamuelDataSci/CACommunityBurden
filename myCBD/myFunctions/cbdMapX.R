

cbdMapX <- function(myLHJ= "Amador", myCause=0,myMeasure = "YLLper", myYear=2015,myCon="Yes",myGeo="Census Tract",cZoom=TRUE,myLabName=FALSE,myLabNum=FALSE) {

  #use these values to see the error bleow  
 # myLHJ = "Amador"; myCause=104; myYear=2015;myLabName=FALSE; myCon=TRUE;myGeo="Community";cZoom=TRUE;    myMeasure = "aRate";myLabNum=FALSE
  
  
    if( myGeo %in% c("Community","Census Tract") & myMeasure == "SMR" ) stop('Sorry kid, SMR calculated only for County level')
  
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

  mydat <-     eval(parse(text=paste0("map.1$",myMeasure)))
  mydat[is.na(mydat)] <- 0

# if (myCon)   {myrange <- c(0,eval(parse(text=paste0("dat.X$",myMeasure))))
#   #  myCuts   <- classIntervals(myrange, n = nC, style = "fisher",dataPrecision=0) 
#   myCuts   <- classIntervals(myrange, n=5,style = "fisher") ###ADDED n=5
# }
#   
# if (!myCon)  {myrange <- c(0,mydat)
#        #     myCuts   <- classIntervals(myrange, n=min(length(mydat),5),style = "fisher") ###ADDED n=5
# 
#              myCuts   <- classIntervals(myrange, n=5,style = "fisher") ###ADDED n=5
# }

  
 myrange <- c(0,mydat)
  myCuts   <- classIntervals(myrange, n=min(length(mydat),5),style = "fisher") ###ADDED n=5
 # myCuts   <- classIntervals(myrange, n=5,style = "fisher") ###ADDED n=5
  
 
  
  if (myCon)   {
    myrange <- c(0,eval(parse(text=paste0("dat.X$",myMeasure))))
  #  myCuts   <- classIntervals(myrange, n = nC, style = "fisher",dataPrecision=0) 
    myCutsT <- myCuts
    myCuts   <- classIntervals(myrange, n=5,style = "fisher") ###ADDED n=5
    myCuts$brks[6] <- max(myCutsT$brks[length(myCutsT$brks)],myCuts$brks[length(myCuts$brks)])
  }
  
  
 
  
    map.1$plotter <-  eval(parse(text=paste0("map.1$",myMeasure)))
  
  
  

#if (myMeasure=="med.age") {myColor1 <- rev(myColor1)}
palette(myColor1)

#========================================

  
  Leg      <- findColours(myCuts,myColor1,between="-",under="<",over=">",cutlabels=FALSE)
  mCols    <- findInterval(mydat,myCuts$brks,rightmost.closed=TRUE)
  #=========================================  
  
# plot(map.1)
# plot(map.1, col=mCols,add=TRUE)  
  
  
 #  mapBoth1 <- function(myGeo="County",myMeasure="cDeathRate",cZoom=FALSE,myLHJ="Alameda",cutType,stateCut,nC)  {
    
  #  mapInputs(myGeo,myMeasure,cZoom,myLHJ,cutType,stateCut,nC) 
    
  library(tmap)
    tm_shape(map.1) + tm_polygons("plotter",title=paste(myMeasure)) + tm_style_grey()
    
    
  
}
