
library(tigris)
library(tmap)

options(tigris_class = "sf")  
shapeX  <- counties(state = "CA", cb = TRUE)
tm_shape(shapeX) + tm_polygons(col="ALAND")


