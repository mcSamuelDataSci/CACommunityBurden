library(tigris) 
library(dplyr)
library(sf)         
library(rmapshaper) # to remove islands (not yet working)

options(tigris_class = "sf")  # Read shape files as Simiple Features objects
shape_Tract  <- tracts(state = "CA", cb = TRUE)  # 8043 tracts  # Obtain tracts boundry tiger files from Census

library(tmap)
# "075" San Francisco;  "037" Los Angeles
tm_shape(filter(shape_Tract,COUNTYFP == "075")) + tm_polygons(col="ALAND")

shape_Tract  <- ms_filter_islands(shape_Tract,min_area = 10000000)                       
# I tried a number of purumtations:
# shape_Tract  <- ms_filter_islands(shape_Tract)
# shape_Tract  <- ms_filter_islands(shape_Tract,min_area = 1) 

tm_shape(filter(shape_Tract,COUNTYFP == "075")) + tm_polygons(col="ALAND")

# change to Los Angeles... No island removed...