# =====================================================================================
# "scatterSDOH.R" file                                                                |
#            Social Determiants of Health (SDOH) & Outcome Scatter plot plus          |
#                                                                                     |   
# =====================================================================================

myYear <- 2020
f      <- list(family = "Arial", size = 18, color = "blue")            
pal    <- c("red", "blue", "green")


scatterSDOH <- function(myCause="0", myMeasure = "aRate", mySex="Total", myGeo="Community", mySDOH="est_pov"){

   if(1==2){
  myCause="0"
  myMeasure = "aRate"
  mySex="Total"
  myGeo="Community"
  mySDOH="est_pov"
  }
  

if( myGeo %in% c("Community","Census Tract") & myMeasure == "SMR" ) stop('Sorry kid, SMR calculated only for County level')
  

SDOH_long_label     <-  names(which(sdohVec == mySDOH))
death_measure_label <-  deathMeasuresNames[which(deathMeasures== myMeasure)]
death_measure_label <-  names(which(deathMeasures== myMeasure))


cause_label <- deathCauseLink[deathCauseLink[,1]==myCause,2]


#temp <- paste0("dat.1$",myMeasure)


if (myGeo=="Census Tract") {
 
  sdohWork <- sdohTract
  dat.1 <- filter(datTract,sex==mySex,causeCode==myCause,county != "CALIFORNIA")  
  temp  <- dat.1[,c("GEOID",myMeasure)]
  sdohWork  <- merge(sdohWork,temp,by="GEOID")
  sdohWork <- sdohWork %>%
    left_join(select(commInfo, comID, comName), by = "comID") %>%
    select(selectedSDOH = {{ mySDOH }}, myMeasure = {{ myMeasure }}, pop, county, region, comName, GEOID) %>%
    mutate(sdohText = if(mySDOH == "hpi2score") round(selectedSDOH, 2) else scales::percent(selectedSDOH/100, accuracy = 0.1),
           plotText = paste('GEOID:', GEOID,
                            '<br>Community:', comName,  
                            '<br>County:',county,
                            '<br>Region:', region,
                            '<br>Population:',format(pop, big.mark = ","), 
                            '<br>',!!names(SDOH_long_label),":",sdohText,
                            '<br>',!!death_measure_label,":",round(myMeasure)))
}


 
 
  deaths_Comm <- filter(datComm, sex == mySex, causeCode == myCause, county != "CALIFORNIA")  
 # temp        <- deaths_Comm[,c("comID", "comName", myMeasure)]  # SELECT BELOW
  
  sdohWork_Comm  <- left_join(sdohComm, deaths_Comm[,c("comID", "comName", myMeasure)], by="comID")  %>%
    select(selectedSDOH = {{ mySDOH }}, myMeasure = {{ myMeasure }}, pop, county, region, comName, comID) %>%
    mutate(sdohText = if(mySDOH == "hpi2score") round(selectedSDOH, 2) else scales::percent(selectedSDOH/100, accuracy = 0.1),
           plotText = paste('Community:', comName,  
                            '<br>County:',county,
                            '<br>Region:', region,
                            '<br>Population:',format(pop, big.mark = ","), 
                            '<br>',!!names(SDOH_long_label),":", sdohText,
                            '<br>',!!death_measure_label,":",round(myMeasure)))

  
if (myGeo=="Community") {
  sdohWork <- sdohWork_Comm
  dat.1    <- deaths_Comm  
  }



if (myGeo=="County") {
  sdohWork <- sdohCounty
  dat.1 <- filter(datCounty,year==myYear,sex==mySex,causeCode==myCause,county != "CALIFORNIA")  
  temp  <- dat.1[,c("county",myMeasure)]
  sdohWork  <- merge(sdohWork,temp,by="county")
  sdohWork <- sdohWork %>%
    select(selectedSDOH = {{ mySDOH }}, myMeasure = {{ myMeasure }}, pop, county, region) %>%
    mutate(sdohText = if(mySDOH == "hpi2score") round(selectedSDOH, 2) else scales::percent(selectedSDOH/100, accuracy = 0.1),
           plotText = paste('<br>County:',county,
                            '<br>Region:', region,
                            '<br>Population:',format(pop, big.mark = ","), 
                            '<br>',!!names(SDOH_long_label),":",sdohText,
                            '<br>',!!death_measure_label,":",round(myMeasure)))
  
}

sdohWorkList <- as.list(sdohWork)  


  
if (nrow(sdohWork)==0) stop("Sorry friend, data are suppressed per the California Health and Human Services Agency Data De-Identification Guidelines, or there are no cases that meet this criteria.")



p <-plot_ly(
  data = sdohWork,
  x =    ~selectedSDOH,
  y =    ~myMeasure,
  type="scatter",
  mode="markers",
  colors=pal,
  color = ~region,
  size =  ~pop, sizes=c(10,15),
  hoverinfo = 'text',
  text   = ~plotText ) %>%
  layout(title=wrap.labels(paste('<b>','Association of',sdohVecL[SDOH_long_label],"and",death_measure_label,"for",cause_label,"by",myGeo,"in",myYear,'</b>'),100),
         
         xaxis = list(title=SDOH_long_label,              titlefont = f, showline = TRUE, linewidth = 2),
         
         yaxis=  list(title=death_measure_label, titlefont = f, showline = TRUE, linewidth = 2))

sdohWork$sdoh5 <- cut_number(sdohWork$selectedSDOH,5)


tempSize <- 10

sdoh_theme  <- function(){ 
  font <- "Georgia"   #assign font family up front
  
  theme_bw() %+replace%    #replace elements we want to change
    
   
  theme(plot.title   = element_text(size = tempSize, color=myTitleColor, face = 'bold'),
        axis.title   = element_text(size = tempSize, face="bold"), # was myTextSize2, changed to myAxisSize
        axis.text.y  = element_text(size = tempSize),
        axis.text.x  = element_text(size = tempSize), 
        legend.text = element_text(size = tempSize), 
        legend.title = element_text(size = tempSize)
  )
}


vio1 <- ggplot(sdohWork,  aes(x=sdoh5, y=myMeasure)) + 
           labs(x=SDOH_long_label, y=death_measure_label) +
           geom_violin(adjust = 0.7) +
           geom_boxplot(width=0.1, fill = "red") + 
           sdoh_theme()


myBinN <- 100


tEMP1 <- geom_histogram(aes(y=..density..), col="blue",fill="lightblue", bins=myBinN) 
tEMP2 <- geom_density(col="red", size=1) 


hist1 <- ggplot(data= sdohWork, aes(x=myMeasure))    + tEMP1 + tEMP2 + labs(x=death_measure_label, title=cause_label) + sdoh_theme()
hist2 <- ggplot(data= sdohWork, aes(x=selectedSDOH)) + tEMP1 + tEMP2 + labs(x="percent", title=SDOH_long_label)       +   sdoh_theme()


map.1      <- left_join(shape_Comm, sdohWork_Comm, by=c("county","comID")) 


map1 <-  tm_shape(map.1) + 
          tm_borders() + 
          tm_fill("myMeasure") +
          tm_layout(frame=F)


map2 <-  tm_shape(map.1) + 
  tm_borders() + 
  tm_fill("selectedSDOH") +
  tm_layout(frame=F)



list(p = p, violin1 = vio1, hist1 = hist1, hist2 = hist2, map1 = map1, map2 = map2)



}

