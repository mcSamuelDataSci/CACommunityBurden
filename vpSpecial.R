library(dplyr)
library(tmap)
library(fs)
library(rgdal)
library(sf)

myPlace   <- "e:/0.CBD/myCBD"  
whichData <- "real"

datComm   <- readRDS(path(myPlace,"/myData/",whichData,"datComm.RDS"))
datCounty <- readRDS(path(myPlace,"/myData/",whichData,"datCounty.RDS"))


vpState <- datCounty %>% filter(county=="CALIFORNIA",year==2014,sex=="Total",CAUSE %in% c("E02","E03"))


stateHa  <- vpState[vpState$CAUSE=="E03","aRate"]
stateHse <- vpState[vpState$CAUSE=="E03","aSE"]

communityHomicide <- datComm %>% filter(sex=="Total",CAUSE %in% c("E03")) %>% 
                                 select(county,comID,comName,oDeaths,pop,aRate,aSE,aLCI,aUCI) %>%
                                 mutate(myZ    = (aRate-stateHa) / sqrt(aSE^2 + stateHse^2)) %>%
                                 mutate(sig    = ifelse(myZ>1.96, 1, ifelse(myZ < -1.96,3,2)))


stateSa  <- vpState[vpState$CAUSE=="E02","aRate"]
stateSse <- vpState[vpState$CAUSE=="E02","aSE"]

communitySuicide  <- datComm %>% filter(sex=="Total",CAUSE %in% c("E02")) %>% 
                                 select(county,comID,comName,oDeaths,pop,aRate,aSE,aLCI,aUCI) %>%
                                 mutate(myZ    = (aRate-stateSa) / sqrt(aSE^2 + stateSse^2)) %>%
                                 mutate(sig    = ifelse(myZ>1.96, 1, ifelse(myZ < -1.96,3,2)))

#write.csv(communityHomocide,(paste0(upPlace,"/tempOutput/communityHomocide.csv")))
#write.csv(communitySuicide,(paste0(upPlace,"/tempOutput/communitySuicide.csv")))


shape_Comm     <- readOGR(paste0(myPlace,"/myData/shape_Comm.shp")) 

# vpMap <- function(myCounty,myDeath)  {
  
myCounty <- "Los Angeles"
myDeath  <- "Homicide"
  
if (myDeath == "Homicide") {xDat <- filter(communityHomicide,county==myCounty)
                          myPal <- c("chartreuse4","chartreuse3","chartreuse1")}
if (myDeath == "Suicide")  {xDat <- filter(communitySuicide,county==myCounty)
                          myPal <- c("darkorchid4","darkorchid3","darkorchid1")}

map.1  <- merge(shape_Comm, xDat, by.x=c("county","comID"), by.y = c("county","comID"),all=TRUE) 
map.1  <- map.1[map.1$county == myCounty,]

#myPal <- c("#D7191C","#FDAE61","#FFFFBF")


jpeg(paste0("tempfig/",myCounty,myDeath,".jpg"),2000,2000,quality=100)
#png(paste0("e:/0.CBD",myCounty,myDeath,".png"),2000,2000)


tm_shape(map.1) + tm_polygons(col="sig",palette=myPal,colorNA="white",labels = c("Above","Same","Below"),title="Legend Title Here")  +
tm_layout(frame=F,main.title= paste(myDeath,"Rates in Communities in",myCounty,"County, 2013-2017"),main.title.size = 3,scale=4,main.title.position = c("center","top"))


#          legend.title.size = 1, legend.text.size = 1)             

dev.off()
# }

#main.title.size = 8,

#vpMap("Fresno","Suicide")



# Homicide:  Alameda, Sacramento, Los Angeles, San Bernadino
# Suicide: Alameda, Sacramento, Los Angeles, San Diego


