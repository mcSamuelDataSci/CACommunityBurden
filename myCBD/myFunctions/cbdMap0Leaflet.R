cbdMap0Leaflet <- function(myLHJ, myCause=0, myMeasure = "YLLper", myYear=2015, myGeo="Census Tract",myPal="Numeric") {
    
   # county data for just 2011-2015
   # dat.X   <- filter(datCounty,year %in% 2011:2015, CAUSE==myCause,county !="CALIFORNIA STATE")
    
    if (myGeo == "County"){
      dat.1   <- filter(datCounty,year==myYear,CAUSE==myCause,Level == "gbd36")  
      map.1   <- merge(shape_County, dat.1, by.x=c("county"), by.y = c("county"),all=TRUE)
      map.1$lab <- map.1$county
      yearLab <- myYear }
   
    if (myGeo == "Community") {
      dat.1    <- filter(datComm,yearG==yG,CAUSE==myCause, comID != "Unknown",Level == "gbd36")
      map.1    <- merge(shape_Comm, dat.1, by.x=c("county","comID"), by.y = c("county","comID"),all=TRUE) 
      map.1$lab <- paste0("MSSA: ",map.1$comName," (ID=",map.1$comID,")")
      yearLab <- yG    }  
  
    if (myGeo == "Census Tract") { 
      dat.1    <- filter(datTract,yearG==yG,CAUSE==myCause,Level == "gbd36") 
      dat.1    <- dat.1[dat.1$pop > 200,]  # TEMP FIX
      map.1    <- merge(shape_Tract, dat.1, by.x=c("county","GEOID"), by.y = c("county","GEOID"),all=TRUE) 
      map.1$lab <- map.1$GEOID
      yearLab  <- yG    }
        
  #  if (cZoom) {map.1   <- map.1[map.1$county == myLHJ,]}
    
    if (nrow(dat.1)==0) stop("Sorry friend, but thank goodness there are none of those; could be some other error")
    
    map.1$WORK <-  eval(parse(text=paste0("map.1$",myMeasure)))
    map.1$WORK[is.na(map.1$WORK)] <- 0
   
    pal <- colorNumeric(rev(brewer.pal(6,"RdYlBu")),  domain = map.1[["WORK"]], 6)
    if (myPal=="Quantile") {pal <- colorQuantile(rev(brewer.pal(6,"RdYlBu")),  domain = map.1[["WORK"]], 6) }

    
    mA <-leaflet(map.1)  %>% 
            addTiles()  %>%
            addPolygons(color = "#444444", weight = 1, fillOpacity = 0.4, fillColor = ~ pal(WORK),
                 popup = paste(myMeasure,"=",round(map.1[["WORK"]],2),"<BR>",map.1[["lab"]],"<BR>") ) %>%
            addLegend("bottomleft", pal = pal, values = map.1[["WORK"]],title = names(lMeasures[lMeasures==myMeasure]), opacity=.7 )
              
 mA
 }

#labFormat = labelFormat(between = "--",digits=2),

# addPolygons(weight=2, fillColor = ~colorBin("YlGnBu", map.1[["WORK"]], 5, pretty = TRUE)(map.1[["WORK"]]),fillOpacity=.7    ) %>%
# addPolygons(weight=2, fillColor = pal, fillOpacity=.7) %>%
# addPolygons(weight=2, fillColor = map.1$xCol,fillOpacity=.7) %>%
# addPolygons(weight=2, fillColor = ~colorNumeric(rev(brewer.pal(7,"RdYlBu")), map.1[["WORK"]], 7)(map.1[["WORK"]]),fillOpacity=.7,


# if (myCon)   {myrange <- c(0,eval(parse(text=paste0("dat.X$",myMeasure))))}        
# if (!myCon)  {myrange <- c(0,mydat)}

#  if (myMeasure=="med.age") {myColor1 <- rev(myColor1)}
#  palette(myColor1)

#  myCuts   <- classIntervals(myrange, n = nC, style = "quantile") 
#  Leg      <- findColours(myCuts,myColor1,between="-",under="<",over=">",cutlabels=FALSE)
#  mCols    <- findInterval(mydat,myCuts$brks,rightmost.closed=TRUE)
# map.1$xCol <- mCols

# fillColor = ~colorBin("YlOrRd", ca.map[[m.var()]], 5, pretty = TRUE)(ca.map[[m.var()]] )
# pal <- colorNumeric(palette = "YlOrRd",domain = ca.map[[m.var()]], 5 )  -- john's legend...
# pal <- colorQuantile("YlGnBu", domain= map.1[["WORK"]], n = 5, na.color = "#808080", alpha = FALSE)
# pal <- colorNumeric(palette = rev(brewer.pal(7,"RdYlBu")), domain = map.1[["WORK"]],7)
# pal <- colorBin("YlGnBu", map.1[["WORK"]], 5, pretty = TRUE)(map.1[["WORK"]])



