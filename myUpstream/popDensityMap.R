
myDrive  <- "d:"                            
myPlace  <- paste0(myDrive,"/0.CBD/myCBD")  
upPlace  <- paste0(myDrive,"/0.CBD/myUpstream")  

library(tigris)     # Other geographies: block_groups, blocks, counties, county_subdivisions, places, pumas, school_districts, states, zctas
library(dplyr)
library(sf)         # simple features GIS
library(fs)         # just for path function
library(rmapshaper) # to remove islands (not yet working)
library(readr)


#-- Read Info file --------------------------------------------------------------------------------

cbdLinkCA  <- read.csv(path(myPlace,"/myInfo/Tract to Community Linkage.csv"),colClasses = "character")  # colClasses... essential for keeping leading "0"   

options(tigris_class = "sf")  # Read shape files as Simiple Features objects

tr_ca   <- tracts(state = "CA", cb = TRUE)  # 8043 tracts  # Obtain tracts boundry tiger files from Census
cnty_ca <- counties(state = "CA", cb = TRUE)

proj1 <- "+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# Project to teale meters
tr_ca    <- st_transform(tr_ca, crs = proj1)
cnty_ca  <- st_transform(cnty_ca, crs = proj1)

# Join "Community IDs (MSSAs)" to tract file, remove "bad" tracts, select variables/columns needed

shape_Tract <- tr_ca %>% geo_join(cbdLinkCA, by="GEOID") %>%   
                            select(GEOID,comID,COUNTYFP,county,geometry) # 8034 tracts

shape_Comm  <- shape_Tract %>% group_by(county,comID) %>% summarize() %>% ungroup()







st_write(shape_Comm,path(myPlace,"/myData/shape_Comm.shp"),delete_layer=TRUE)



#-- Create County shape file based same approach as above----------------------------------------------------------------

shape_County <- shape_Tract %>% group_by(county) %>% summarize()

st_write(shape_County,path(myPlace,"/myData/shape_County.shp"),delete_layer=TRUE)





# NONE of below Currently Used:
# MAKE SMALLER SHAPE FILES!? ================================================================================================
 
library(tmap)

tM <- function(sIn) {tm_shape(sIn) +  tm_polygons("county", title="County") }
tM(shape_Tract)

 shape_S     <- st_simplify(shape_Tract,  dTolerance = 1000, preserveTopology = TRUE) ; tM(shape_S)

library(rmapshaper)
# object_size package
#format(object.size(s_Tract), "MB") #12.4 Mb too big
s1 <- ms_simplify(shape_Tract); tM(s1)

s2<- ms_simplify(shape_Tract,keep=.001);tM(s2)

# ERROR
shapeX <- gSimplify(shape_Tract,tol = 0.05, topologyPreserve = TRUE)
# writeSpatialShape(shapeX,paste0(myPlace,"/myData/shapeX"))

# OLD APPROACH SAVE IN TRUE ARCHIVES ===================================================================

library(rgeos)
library(maptools) 

writeSpatialShape(shape_TractX,paste0(myPlace,"/myData/shape_Tract"))

shape_Comm      <- unionSpatialPolygons(shape_Tract, shape_Tract$comID)      # base spatial polygons on community

# work around to be able to use/access Community ID varibles
n.ID <-shape_Comm@polygons[[1]]@ID
for (i in 2:length(shape_Comm)){  n.ID <-c(n.ID,shape_Comm@polygons[[i]]@ID)}  # make accessible label for shapes
shape_Comm$comID <- as.character(n.ID)

# to associate County names with MSSAs and then link back to file -- more elegnat approach?
linkTemp    <- as.data.frame(cbdLinkCA %>% group_by(county,comID) %>% summarize(junk=n()))[,1:2] # removes "junk"
shape_Comm  <- merge(shape_Comm,linkTemp,by="comID")
#shape_Comm  <- geo_join(shape_Comm,linkTemp,by="comID")
writeSpatialShape(shape_CommX,paste0(myPlace,"/myData/shape_Comm"))


# NOTES  ===============================================================================================

#   the "unionSpatialPolygons" function below is an important piece of this code
#   it is part of the "maptools" package
#   and maptools needs to use "gpclib"  -- not sure what this is -- someone could research
#   and, apparently "you can't give maptools the permission to use gpclib unless you have the package gpclib installed"
#   but, gpclib will not install without "Rtools", which needs to be "installed" from 
#       https://cran.r-project.org/bin/windows/Rtools/
#   then
#   install.packages("gpclib")
#   will work, hoepfully!
# http://stackoverflow.com/questions/21093399/how-to-turn-gpclibpermit-to-true
#   and, there may be some importance to the order of the next three libarary statemenets (see )
# library(gpclib); library(rgeos); library(maptools)
# library(rgdal)   -- this might play an alterntive role in the issue above, but not sure