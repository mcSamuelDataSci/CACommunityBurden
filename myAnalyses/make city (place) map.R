


library(tigris)
library(sf)
library(dplyr)
options(tigris_class = "sf") 

shape_places   <- places(state = "CA", cb = TRUE)  
st_write(shape_places,"shape_places.shp",delete_layer=TRUE)




# shape_city   <- st_read("CA_City_shape.shp",stringsAsFactors=FALSE)
# tm_shape(shape_city) + tm_polygons("ALAND")
