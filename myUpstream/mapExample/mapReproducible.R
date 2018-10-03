
library(tigris)
# make California County Map File
options(tigris_class = "sf")  
caCounty  <- counties(state = "CA", cb = TRUE)  

library(readr)
# get some (real!) California data
caDat     <- read_csv("https://raw.githubusercontent.com/mcSamuelDataSci/CACommunityBurden/master/myUpstream/mapExample/caDat.csv")

library(dplyr)
# merge the map with data
myMapper  <- left_join(caCounty,caDat,by=c("NAME"= "county"))

library(tmap)
# simple mapping function
# first argument is the variable for the coloring
# second argument if is "style" for the break points
mapEx <- function(measure,cutSystem){
                 tm_shape(myMapper) + tm_polygons(col=measure, style=cutSystem)
         }

tmap_mode("plot")
mapEx("aRate","quantile")   #  :)
mapEx("Ndeaths","fisher")   #  :)  
mapEx("Ndeaths","quantile") #  :)

tmap_mode("view")
mapEx("aRate","quantile")   #  :)
mapEx("Ndeaths","fisher")   #  :)  
mapEx("Ndeaths","quantile") #  :(

#  Error in `levels<-`(`*tmp*`, value = as.character(levels)) : 
#    factor level [2] is duplicated

# --------------------------------------------------------------------------------------------------------------



caDat     <- filter(datCounty,year==2017,sex=="Total",CAUSE=="A01",county != "CALIFORNIA") %>%
                  select(county,Ndeaths,SMR,aRate)

write_csv(caDat,"caDat.csv")
caDat <- read.csv("caDat.csv")