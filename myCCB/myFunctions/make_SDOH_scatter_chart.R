# =====================================================================================
# "scatterSDOH.R" file                                                                |
#            Social Determiants of Health (SDOH) & Outcome Scatter plot plus          |
#                                                                                     |   
# =====================================================================================

myYear <- 2020
f      <- list(family = "Arial", size = 18, color = "blue")  # not used          
pal    <- c("red", "blue", "green")

# mySex=c("Male","Female"),
scatterSDOH <- function(myCause="0", myMeasure = "aRate",  myGeo="Community", mySDOH="est_pov"){

   if(1==2){
  myCause="0"
  myMeasure = "aRate"
  mySex = c("Male","Female")
  mySex="Total"
  myGeo="County"
  myGeo="Community"
  mySDOH="est_pov"
  }
  

mySex = c("Male","Female")  
  
  
if( myGeo %in% c("Community","Census Tract") & myMeasure == "SMR" ) stop('Sorry kid, SMR calculated only for County level')
  

SDOH_long_label     <-  names(which(sdohVec == mySDOH))
SDOH_short_label    <-                         mySDOH
death_measure_label <-  deathMeasuresNames[which(deathMeasures== myMeasure)]
death_measure_label <-  names(which(deathMeasures== myMeasure))


cause_label <- deathCauseLink[deathCauseLink[,1]==myCause,2]


yearLabel <- ifelse(myGeo=="County", myYear, "20xx-20xx")



if (myGeo=="Census Tract") {
 
  sdohWork <- sdohTract
  dat.1 <- filter(datTract,sex %in% mySex,causeCode==myCause,county != "CALIFORNIA")  
  temp  <- dat.1[,c("GEOID", "sex", myMeasure)]
  sdohWork  <- merge(sdohWork,temp,by="GEOID")
  sdohWork <- sdohWork %>%
    left_join(select(commInfo, comID, comName), by = "comID") %>%
    select(selectedSDOH = {{ mySDOH }}, myMeasure = {{ myMeasure }}, pop, county, region, sex, comName, GEOID) %>%
    mutate(sdohText = if(mySDOH == "hpi2score") round(selectedSDOH, 2) else scales::percent(selectedSDOH/100, accuracy = 0.1),
           plotText = paste('GEOID:', GEOID,
                            '<br>Community:', comName,  
                            '<br>County:',county,
                            '<br>Region:', region,
                            '<br>Population:',format(pop, big.mark = ","), 
                            '<br>',!!names(SDOH_long_label),":",sdohText,
                            '<br>',!!death_measure_label,":",round(myMeasure)))
}


 
 
  deaths_Comm <- filter(datComm, sex %in% mySex, causeCode == myCause, county != "CALIFORNIA")  
 # temp        <- deaths_Comm[,c("comID", "comName", myMeasure)]  # SELECT BELOW
  
  sdohWork_Comm  <- left_join(sdohComm, deaths_Comm[,c("comID", "comName", "sex", myMeasure)], by="comID")  %>%
    select(selectedSDOH = {{ mySDOH }}, myMeasure = {{ myMeasure }}, pop, county, region, sex, comName, comID) %>%
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
 dat.1 <- filter(datCounty,year==myYear,sex %in% mySex,causeCode==myCause,county != "CALIFORNIA")  
    temp  <- dat.1[,c("county","sex",myMeasure)]
  sdohWork  <- merge(sdohWork,temp,by="county")
  sdohWork <- sdohWork %>%
    select(selectedSDOH = {{ mySDOH }}, myMeasure = {{ myMeasure }}, pop, county, region,sex) %>%
    mutate(sdohText = if(mySDOH == "hpi2score") round(selectedSDOH, 2) else scales::percent(selectedSDOH/100, accuracy = 0.1),
           plotText = paste('<br>County:',county,
                            '<br>Region:', region,
                            '<br>Population:',format(pop, big.mark = ","), 
                            '<br>',!!names(SDOH_long_label),":",sdohText,
                            '<br>',!!death_measure_label,":",round(myMeasure)))
  
}

sdohWorkList <- as.list(sdohWork)  


  
if (nrow(sdohWork)==0) stop("Sorry friend, data are suppressed per the California Health and Human Services Agency Data De-Identification Guidelines, or there are no cases that meet this criteria.")

t1 <- list(
  family = "Arial",
  size = 16, 
  color = "black")
  
#title= list(text = "Playing with Fonts",font = t1), font=t, 




p <-plot_ly(
  data = sdohWork,
  x =    ~selectedSDOH,
  y =    ~myMeasure,
  type="scatter",
  mode="markers",
  colors=pal,
  color = ~sex,
  size =  ~pop, sizes=c(10,15),
  hoverinfo = 'text',
  text   = ~plotText ) %>%
  layout(xaxis = list(title=list(text=paste(SDOH_short_label, "- continuous"), font =t1), showline = TRUE, linewidth = 2),
         yaxis=  list(title=list(text=death_measure_label, font =t1), showline = TRUE, linewidth = 2))

sdohWork$sdoh5 <- cut_number(sdohWork$selectedSDOH,5)


tempSize <- 14

sdoh_theme  <- function(){ 
  font <- "Georgia"   #assign font family up front
  
  theme_bw() %+replace%    #replace elements we want to change
    
   
  theme(plot.title   = element_text(size = tempSize+4, color=myTitleColor, face = 'bold'),
        axis.title   = element_text(size = tempSize, face="bold"), # was myTextSize2, changed to myAxisSize
        axis.text.y  = element_text(size = tempSize),
        axis.text.x  = element_text(size = tempSize), 
        legend.text = element_text(size = tempSize), 
        legend.title = element_text(size = tempSize),
                                    plot.background = element_blank(),
                                    panel.grid.major = element_blank(),
                                   # panel.grid.minor = element_blank(),
                                    panel.border = element_blank(),
        axis.line = element_line(color = 'black'))
        #draws x and y axis line
  
}


#, axis.line = element_line(color = 'black'))                 

vio1 <- ggplot(sdohWork,  aes(x=sdoh5, y=myMeasure)) + 
           labs(x=paste(SDOH_short_label, "- quintiles"), y=death_measure_label) +
           geom_violin(adjust = 0.7) +
           geom_boxplot(width=0.1, fill = "red") + 
           sdoh_theme()


myBinN <- 100


hist_part1 <- geom_histogram(aes(y=..density..), col="blue",fill="lightblue", bins=myBinN)
hist_part2 <- geom_density(col="red", size=1) 


hist1 <- ggplot(data= sdohWork, aes(x=myMeasure))    + hist_part1 + hist_part2 + labs(x=death_measure_label) + sdoh_theme()
hist2 <- ggplot(data= sdohWork, aes(x=selectedSDOH)) + hist_part1 + hist_part2 + labs(x="percent")       +   sdoh_theme()


if (myGeo %in% c("Community", "Census Tract")) {
map.1      <- left_join(shape_Comm, sdohWork_Comm, by=c("county","comID")) 
}

if(myGeo == "County")  {
map.1      <- left_join(shape_County, sdohWork, by=c("county")) 
}




map1 <-  tm_shape(map.1) + 
          tm_borders() + 
          tm_fill("myMeasure", title=death_measure_label) +
          tm_layout(frame=F) 


map2 <-  tm_shape(map.1) + 
  tm_borders() + 
  tm_fill("selectedSDOH", title=SDOH_short_label) +
  tm_layout(frame=F) 



list(p = p, violin1 = vio1, hist1 = hist1, hist2 = hist2, map1 = map1, map2 = map2)



}

