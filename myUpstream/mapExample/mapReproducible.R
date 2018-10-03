library(tigris)
library(readr)
options(tigris_class = "sf")  # Read shape files as Simiple Features objects
caCounty  <- counties(state = "CA", cb = TRUE)  

caDat     <- filter(datCounty,year==2017,sex=="Total",CAUSE=="A01",county != "CALIFORNIA") %>%
                  select(county,Ndeaths,SMR,aRate)

write_csv(caDat,"caDat.csv")

caDat <- read.csv("caDat.csv")
library(rio)
junk <- import("https://github.com/mcSamuelDataSci/CACommunityBurden/blob/master/myUpstream/mapExample/caDat.csv")


myMapper <- left_join(caCounty,caDat,by=c("NAME"= "county"))


cutSystem <- c("fisher", "quantile")
measures  <- c("Ndeaths","aRate")

mapEx <- function(measure,cutSystem){

  tm_shape(myMapper) + tm_polygons(col=measure, style=cutSystem)
}

mapEx(measures[1],cutSystem[1])  #works! :)
mapEx(measures[2],cutSystem[1])  #works! :)  
mapEx(measures[1],cutSystem[2])  # error :( 
