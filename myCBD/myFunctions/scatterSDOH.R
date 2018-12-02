# =====================================================================================
# "scatterSDOH.R" file                                                                |
#            Social Determiants of Health (SDOH) & Outcome Scatter plot plus          |
#                                                                                     |   
# =====================================================================================

myYear <- 2017

f   <- list(family = "Arial", size = 18, color = "blue")            
pal <- c("red", "blue", "green")

# t.x <- 4
# t.y <- 5


# https://plotly-book.cpsievert.me/scatter-traces.html
         

scatterSDOH <- function(myCause="0", myMeasure = "aRate",mySex="Total",myGeo="Community",t.x="abovepoverty"){
 if(1==2){
  myCause="0"
  myMeasure = "aRate"
  mySex="Total"
  myGeo="Community"
  t.x="abovepoverty"
}
  
if( myGeo %in% c("Community","Census Tract") & myMeasure == "SMR" ) stop('Sorry kid, SMR calculated only for County level')
  
  
  
t.y <- 8  
xL <-  which(sdohVec == t.x)
xM <-  which(lMeasures== myMeasure)


temp <- paste0("dat.1$",myMeasure)

if (myGeo=="Census Tract") {
                          sdohWork <- sdohTract
                          dat.1 <- filter(datTract,sex==mySex,CAUSE==myCause,county != "CALIFORNIA")  
                          temp  <- dat.1[,c("GEOID",myMeasure)]
                          sdohWork  <- merge(sdohWork,temp,by="GEOID")
                           }

if (myGeo=="Community") {
                       sdohWork <- sdohComm
                       dat.1 <- filter(datComm,sex==mySex,CAUSE==myCause,county != "CALIFORNIA")  
                       temp  <- dat.1[,c("comID",myMeasure)]
                       sdohWork  <- merge(sdohWork,temp,by="comID")}
if (myGeo=="County") {
                   sdohWork <- sdohCounty
                   dat.1 <- filter(datCounty,year==myYear,sex==mySex,CAUSE==myCause,county != "CALIFORNIA")  
                   temp  <- dat.1[,c("county",myMeasure)]
                   sdohWork  <- merge(sdohWork,temp,by="county")
             
}


sdohWork <- sdohWork[,c("hpi2score", "insured", "inpreschool", "bachelorsed", "abovepoverty","parkaccess","houserepair",myMeasure,"pop","county","region")]

sdohWorkList <- as.list(sdohWork)  


# https://plot.ly/r/axes/
  



p <- plot_ly(
  data = sdohWork,
  x =    sdohWork[,t.x],
  y =    sdohWork[,t.y],
  type="scatter",mode="markers",
  colors=pal,
  color = sdohWork[,"region"],
  size =  sdohWork[,"pop"], sizes=c(20,400),
  hoverinfo = 'text',
  text   = paste('County:',sdohWork[,"county"],
                '<br> Population:',format(sdohWork[,"pop"], big.mark = ","), 
                '<br>',names(xL),":",round(sdohWork[,t.x]),"%",
                '<br>',names(xM),":",round(sdohWork[,t.y])) ) %>%
  layout(title=paste('<b>','Association of',sdohVecL[xL],"and",lMeasuresC[xM],"for",fullCauseList[fullCauseList[,1]==myCause,2],"by",myGeo,'</b>'),
                 
                 xaxis = list(title=sdohVecL[xL],      titlefont = f, showline = TRUE,linewidth = 2),
                 yaxis=  list(title=lMeasuresC[xM],titlefont = f, showline = TRUE,linewidth = 2))



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
# layout(title=paste('Association of',sdohVecL[xL],"and",lMeasuresC[xM],"for",fullCauseList[fullCauseList[,1]==myCause,2],"by",myGeo),
#        xaxis = list(title=sdohVecL[xL],      titlefont = f, showline = TRUE,linewidth = 2),
#        yaxis=  list(title=lMeasuresC[xM],titlefont = f, showline = TRUE,linewidth = 2))
p
}


# add_annotations(x = c(100),
#                   y = c(10,15,20),
#                   text = c("ab","c","d"),showarrow = FALSE,
#                   colors=pal,
#                   color=c(1,2,3)) %>% 




# https://plot.ly/r/legend/

