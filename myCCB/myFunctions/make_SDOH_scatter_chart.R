# =====================================================================================
# "scatterSDOH.R" file                                                                |
#            Social Determiants of Health (SDOH) & Outcome Scatter plot plus          |
#                                                                                     |   
# =====================================================================================

myYear <- 2017
f      <- list(family = "Arial", size = 18, color = "blue")            
pal    <- c("red", "blue", "green")


scatterSDOH <- function(myCause="0", myMeasure = "aRate",mySex="Total",myGeo="Community",t.x="est_pov"){
 if(1==2){
  myCause="0"
  myMeasure = "aRate"
  mySex="Total"
  myGeo="Community"
  t.x="est_pov"
}
  
  
if( myGeo %in% c("Community","Census Tract") & myMeasure == "SMR" ) stop('Sorry kid, SMR calculated only for County level')
  

t.y <- 11  # Dumb quick fix to select measure column
xL <-  which(sdohVec == t.x)
# xM <-  which(deathMeasures== myMeasure)
xM <-  deathMeasuresNames[which(deathMeasures== myMeasure)]

temp <- paste0("dat.1$",myMeasure)


if (myGeo=="Census Tract") {
  sdohWork <- sdohTract
  dat.1 <- filter(datTract,sex==mySex,causeCode==myCause,county != "CALIFORNIA")  
  temp  <- dat.1[,c("GEOID",myMeasure)]
  sdohWork  <- merge(sdohWork,temp,by="GEOID")
  sdohWork <- sdohWork %>%
    left_join(select(commInfo, comID, comName), by = "comID") %>%
    select(mySDOH = {{ t.x }}, myMeasure = {{ myMeasure }}, pop, county, region, comName, GEOID) %>%
    mutate(sdohText = if(t.x == "hpi2score") round(mySDOH, 2) else scales::percent(mySDOH/100, accuracy = 0.1),
           plotText = paste('GEOID:', GEOID,
                            '<br>Community:', comName,  
                            '<br>County:',county,
                            '<br>Region:', region,
                            '<br>Population:',format(pop, big.mark = ","), 
                            '<br>',!!names(xL),":",sdohText,
                            '<br>',!!xM,":",round(myMeasure)))
}


  
  
  
 
  deaths_Comm <- filter(datComm, sex == mySex, causeCode == myCause, county != "CALIFORNIA")  
 # temp        <- deaths_Comm[,c("comID", "comName", myMeasure)]  # SELECT BELOW
  
  sdohWork_Comm  <- left_join(sdohComm, deaths_Comm[,c("comID", "comName", myMeasure)], by="comID")  %>%
    select(mySDOH = {{ t.x }}, myMeasure = {{ myMeasure }}, pop, county, region, comName, comID) %>%
    mutate(sdohText = if(t.x == "hpi2score") round(mySDOH, 2) else scales::percent(mySDOH/100, accuracy = 0.1),
           plotText = paste('Community:', comName,  
                            '<br>County:',county,
                            '<br>Region:', region,
                            '<br>Population:',format(pop, big.mark = ","), 
                            '<br>',!!names(xL),":", sdohText,
                            '<br>',!!xM,":",round(myMeasure)))

  
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
    select(mySDOH = {{ t.x }}, myMeasure = {{ myMeasure }}, pop, county, region) %>%
    mutate(sdohText = if(t.x == "hpi2score") round(mySDOH, 2) else scales::percent(mySDOH/100, accuracy = 0.1),
           plotText = paste('<br>County:',county,
                            '<br>Region:', region,
                            '<br>Population:',format(pop, big.mark = ","), 
                            '<br>',!!names(xL),":",sdohText,
                            '<br>',!!xM,":",round(myMeasure)))
  
}

sdohWorkList <- as.list(sdohWork)  


  
if (nrow(sdohWork)==0) stop("Sorry friend, data are suppressed per the California Health and Human Services Agency Data De-Identification Guidelines, or there are no cases that meet this criteria.")



p <-plot_ly(
  data = sdohWork,
  x =    ~mySDOH,
  y =    ~myMeasure,
  type="scatter",mode="markers",
  colors=pal,
  color = ~region,
  size =  ~pop, sizes=c(20,400),
  hoverinfo = 'text',
  text   = ~plotText ) %>%
  layout(title=wrap.labels(paste('<b>','Association of',sdohVecL[xL],"and",xM,"for",deathCauseLink[deathCauseLink[,1]==myCause,2],"by",myGeo,"in",myYear,'</b>'),100),
         
         xaxis = list(title=sdohVecL[xL],      titlefont = f, showline = TRUE, linewidth = 2),
         yaxis=  list(title=deathMeasuresNames[xM],titlefont = f, showline = TRUE,linewidth = 2))



sdohWork$sdoh5 <- cut_number(sdohWork$myMeasure,5)

# temp <- sdohWork %>% mutate(sdoh5 = cut_number(myMeasure,5))


vio1 <- ggplot(sdohWork,  aes(x=sdoh5, y=myMeasure)) + 
  geom_violin(adjust = 0.5) +
  geom_boxplot(width=0.1, fill = "red") 



myBinN <- 100



hist1 <- ggplot(data= sdohWork, aes(x=myMeasure)) + geom_histogram(aes(y=..density..), col="blue",fill="lightblue", bins=myBinN)  + geom_density(col="red", size=1) + labs(x=myMeasure) 
hist2 <- ggplot(data= sdohWork, aes(x=mySDOH))    + geom_histogram(aes(y=..density..), col="blue",fill="lightblue", bins=myBinN) + geom_density(col="red", size=1) + labs(x=t.x) 


map.1      <- left_join(shape_Comm, sdohWork_Comm, by=c("county","comID")) 
#map.1$name <- map.1$comName


map1 <-  tm_shape(map.1) + 
          tm_borders() + 
          tm_fill("myMeasure") +
          tm_layout(frame=F)


map2 <-  tm_shape(map.1) + 
  tm_borders() + 
  tm_fill("mySDOH") +
  tm_layout(frame=F)



#, map1 = mapX

list(p = p, violin1 = vio1, hist1 = hist1, hist2 = hist2, map1 = map1, map2 = map2)








}

# t.x <- 4
# t.y <- 5


# https://plotly-book.cpsievert.me/scatter-traces.html




# sdohWork <- sdohWork[,c(sdohVec,myMeasure,"pop","county","region")]


# https://plot.ly/r/axes/

# add_annotations(x = c(100),
#                   y = c(10,15,20),
#                   text = c("ab","c","d"),showarrow = FALSE,
#                   colors=pal,
#                   color=c(1,2,3)) %>% 




# https://plot.ly/r/legend/

# if (myGeo=="Census Tract") {
#                           sdohWork <- sdohTract
#                           dat.1 <- filter(datTract,sex==mySex,causeCode==myCause,county != "CALIFORNIA")  
#                           temp  <- dat.1[,c("GEOID",myMeasure)]
#                           sdohWork  <- merge(sdohWork,temp,by="GEOID")
#                            }
# 
# if (myGeo=="Community") {
#                        sdohWork <- sdohComm
#                        dat.1 <- filter(datComm,sex==mySex,causeCode==myCause,county != "CALIFORNIA")  
#                        temp  <- dat.1[,c("comID",myMeasure)]
#                        sdohWork  <- merge(sdohWork,temp,by="comID")}
# if (myGeo=="County") {
#                    sdohWork <- sdohCounty
#                    dat.1 <- filter(datCounty,year==myYear,sex==mySex,causeCode==myCause,county != "CALIFORNIA")  
#                    temp  <- dat.1[,c("county",myMeasure)]
#                    sdohWork  <- merge(sdohWork,temp,by="county")
#              
# }





# p <-plot_ly(
#   data = sdohWork,
#   x =    sdohWork[,t.x],
#   y =    sdohWork[,t.y],
#   type="scatter",mode="markers",
#   colors=pal,
#   color = sdohWork[,"region"],
#   size =  sdohWork[,"pop"], sizes=c(20,400),
#   hoverinfo = 'text',
#   text   = paste('County:',sdohWork[,"county"],
#                 '<br> Population:',format(sdohWork[,"pop"], big.mark = ","),
#                 '<br>',names(xL),":",round(sdohWork[,t.x]),"%",
#                 '<br>',names(xM),":",round(sdohWork[,t.y])) ) %>%
#   layout(title=wrap.labels(paste('<b>','Association of',sdohVecL[xL],"and",deathMeasuresNames[xM],"for",deathCauseLink[deathCauseLink[,1]==myCause,2],"by",myGeo,"in",myYear,'</b>'),100),
# 
#                  xaxis = list(title=sdohVecL[xL],      titlefont = f, showline = TRUE, linewidth = 2),
#                  yaxis=  list(title=deathMeasuresNames[xM],titlefont = f, showline = TRUE,linewidth = 2))




# 
# 
# p <- plot_ly(
#      data = sdohWork,
#      x =  ~  sdohWorkList[[t.x]],
#      y =  ~ sdohWorkList[[t.y]],
#      type="scatter",mode="markers",
#      colors=pal,
#      color = ~sdohWorkList[["region"]],
#      size = ~ sdohWorkList[["pop"]], sizes=c(20,400)
#     # ,
#     #   hoverinfo = 'text',
#     #    text = ~paste('</br> County',sdohWorkList[["county"]],
#     #                  '</br> Population:',format(sdohWorkList[["pop"]], big.mark = ","), 
#     #                  '</br>',sdohVecL[xL],":",round(sdohWorkList[[t.x]],1),"%",
#     #                  '</br>',myMeasure,":",round(sdohWorkList[[t.y]],1)) 
#     ) %>%
# #    hide_colorbar() %>% 
# layout(title=paste('Association of',sdohVecL[xL],"and",lMeasuresC[xM],"for",deathCauseLink[deathCauseLink[,1]==myCause,2],"by",myGeo),
#        xaxis = list(title=sdohVecL[xL],      titlefont = f, showline = TRUE,linewidth = 2),
#        yaxis=  list(title=lMeasuresC[xM],titlefont = f, showline = TRUE,linewidth = 2))
