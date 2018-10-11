
library(tigris)
library(tmap)







work <- left_join(shape_Tract,junk,by =c("GEOID"="GEOID"))





options(tigris_class = "sf")  
shapeX  <- counties(state = "CA", cb = TRUE)
tm_shape(shapeX) + tm_polygons(col="ALAND")

tmap_mode("view")
tm_shape(work) + tm_polygons(col="ZERO")
