cbdCutPoint0 <- function(myRangeIn,myVecIn){ 
  #myCuts   <- classIntervals(myRangeIn, n = nC,intervalClosure="left") 
  
     myCuts   <- classIntervals(myRangeIn, n = nC, style = "fisher",dataPrecision=0) 
       Leg    <- findColours(myCuts,myColor1,between="-",under="<",over=">",cutlabels=FALSE)
    
    mCols <- findInterval(myVecIn,myCuts$brks,rightmost.closed=TRUE)  #CHECK/FIX righmost bullshit...  see LA 2015 without this....
    output=list(mCols,Leg)
}





# myRangeIn <- runif(100,1,50)
# myVecIn   <- c(3,4,4,30,10,20,49)

# cbdCutPoint2(myDat,myRange)
 
 
 
 