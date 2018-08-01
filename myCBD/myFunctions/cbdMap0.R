#need to change this to tmap!

#one more...Thing

####  check "Tract" versus "Census tract"
####  check myCon="Yes" verus myCon=TRUE


# THIS IS IN THE TEMP BANCH

cbdMap0 <- function(myLHJ= "Amador", myCause=0,myMeasure = "YLLper", myYear=2015,myCon="Yes",myGeo="Census Tract",cZoom=TRUE,myLabName=FALSE,myLabNum=FALSE) {

  #use these values to see the error bleow  
 #  myLHJ = "Colusa"; myCause=104; myYear=2015;myLabName=FALSE; myCon=TRUE;myGeo="Census Tract";cZoom=TRUE;    myMeasure = "Ndeaths";myLabNum=FALSE
  
  
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
    yearLab <- yG    }  
  
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
 # myCuts   <- classIntervals(myrange, n=min(length(mydat),5),style = "fisher") ###ADDED n=5
 myCuts   <- classIntervals(myrange, n=5,style = "fisher") ###ADDED n=5
  
 
  
  if (myCon)   {
    myrange <- c(0,eval(parse(text=paste0("dat.X$",myMeasure))))
  #  myCuts   <- classIntervals(myrange, n = nC, style = "fisher",dataPrecision=0) 
    myCutsT <- myCuts
    myCuts   <- classIntervals(myrange, n=5,style = "fisher") ###ADDED n=5
    myCuts$brks[6] <- max(myCutsT$brks[6],myCuts$brks[6])
  }
  
  
    
  
  
  
  
  
  

#if (myMeasure=="med.age") {myColor1 <- rev(myColor1)}
palette(myColor1)

#========================================

  
  Leg      <- findColours(myCuts,myColor1,between="-",under="<",over=">",cutlabels=FALSE)
  mCols    <- findInterval(mydat,myCuts$brks,rightmost.closed=TRUE)
  #=========================================  
  
plot(map.1)
plot(map.1, col=mCols,add=TRUE)  
  

# library(tmap)
# qtm(map.1,fill="YLLper")




if (!cZoom) {myLHJ <-"California"}
# NO IDEA WHY I NEED "as.numeric" around myCause...
mtext(paste(names(lMeasures[lMeasures==myMeasure]),"in",yearLab,causeList36[causeList36[,1]==myCause,2],"-",myLHJ),side=3,cex=1.3)


## THIS does not work when there are fewer unique mapped values than nC, and the colors plotted are wrong, using, apparently the first and last colors, not say the first and secound colors.
# 


col2hex <- function(cname)
{
  colMat <- col2rgb(cname)
  rgb(
    red=colMat[1,]/255,
    green=colMat[2,]/255,
    blue=colMat[3,]/255
  )
}

 legend("bottomleft",
        legend = (names(attr(Leg, "table"))),
        title = wrap.labels(names(lMeasures[lMeasures==myMeasure]),20),
        fill = col2hex(1:(length(names(attr(Leg, "table"))))),
        cex = 1.1,
        bty = "n") # border




if (myLabName) {
t.ll <- coordinates(map.1)
if (myGeo=="County")    {test <- map.1$county}
if (myGeo=="Community") {test <- comName$comName[comName$comID %in% map.1$comID]}
if (myGeo=="Census Tract")     {test <- map.1$TRACT_ID}
text(t.ll,wrap.labels(as.character(test),10),cex=0.8) }


# THIS could be added to UI and SERVER but is not currently -- way to give MSSA short "names" 
if(myLabNum) {
   t.ll <- coordinates(map.1)
   text(t.ll,as.character(map.1$comID),cex=.6) }

}
