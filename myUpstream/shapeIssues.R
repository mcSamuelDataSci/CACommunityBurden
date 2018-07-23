library(tigris)       # for reading shape files tracts() 
library(dplyr)        # for data manipulation
library(sf)           
library(rmapshaper)   # for ms_filter_islands
library(tmap)         # for mapping


options(tigris_class = "sf")
CA1  <- tracts(state = "CA", cb = TRUE)  
CA2  <- tracts(state = "CA")


#---------------------------------------------------------


junk <- filter(CA1,COUNTYFP=="075")
junk <- ms_filter_islands(junk,min_area = 100000000)  
tm_shape(junk) + tm_polygons("STATEFP")

junk <- ms_filter_islands(CA1,min_area = 100000000)
junk <- filter(junk,COUNTYFP=="075")
tm_shape(junk) + tm_polygons("STATEFP")

#---------------------------------------------------------

LA   <- "037" # Los Angeles
SF   <- "075" # San Francisco

myplot <- function(inFile,co,area){
  inFile %>% filter(COUNTYFP==co) %>% 
             ms_filter_islands(min_area = area) %>% 
             tm_shape() + tm_polygons("STATEFP")
}


junk <- filter(CA1,COUNTYFP=="075")
junk <- ms_filter_islands(junk,min_area = 100000000)  
tm_shape(junk) + tm_polygons("STATEFP")


myplot(CA1,SF,0)
myplot(CA1,SF,1)
myplot(CA1,SF,1000000)
myplot(CA1,SF,100000000)



myplot(CA1,LA,0)
myplot(CA1,LA,100000000)
myplot(CA1,LA,1000000000)
myplot(CA1,LA,10000000000000000000000000000000000000000000000000000000000000)
