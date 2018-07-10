# =====================================================================================
# "shapeProcessor.R" file                                                             |
#            Set locations and loads libraries                                        |
#            Read CA census tract shape via with tigris package                       |
#            Process and save shape_Tract, _Community, and _County files              |
#                                                                                     |   
# =====================================================================================

myDrive  <- "e:"                            
myPlace  <- paste0(myDrive,"/0.CBD/myCBD")  

library(tigris) # Other general area functions: block_groups, blocks, counties, county_subdivisions, places, pumas, school_districts, states, zctas
library(dplyr)
library(sf)
library(fs) # just for path function


#-- Read Info file --------------------------------------------------------------------------------

# cbdLinkCA links census tracts ("GEOID") to "comID" (community ID; currently adjusted MSSA_ID) - 8036 tracts
cbdLinkCA  <- read.csv(path(myPlace,"/myInfo/cbdLinkCA.csv"),colClasses = "character")  # colClasses... essential for keeping leading "0"   

# these (9) census tracts are in the shape files (8043 elements) but not in our CA data; all/mostly water?
bad <- c(c("06081990100","06083990000","06111990100","06037990300","06001990000","06061990000","06017990000","06037137000"),"06075980401")

# -- Read, process, and write CA Tract File -------------------------------------------------------------------------------

options(tigris_class = "sf")
shape_Tract <- tracts(state = "CA", cb = TRUE)  # 8043 tracts

teale       <- "+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
shape_Tract <- st_transform(shape_Tract,crs= teale)

shape_Tract <- shape_Tract %>% full_join(cbdLinkCA, by="GEOID") %>%   # consider tigris "geo_join"?
                               filter(!(GEOID %in% bad)) %>% 
                               select(GEOID,comID,county,geometry) # 8034 tracts

# write out as uncompressed RDS
st_write(shape_Tract,path(myPlace,"/myData/shape_Tract.shp"),delete_layer=TRUE)

#-- Create new Community shape file based on MSSAs --------------------------------------------------------------------------

#st_disolove? 
shape_Comm  <- shape_Tract %>% group_by(county,comID) %>% summarize() %>% ungroup()
st_write(shape_Comm,path(myPlace,"/myData/shape_Comm.shp"),delete_layer=TRUE)

#-- Create County shape file based same approach as above----------------------------------------------------------------

shape_County <- shape_Tract %>% group_by(county) %>% summarize()
st_write(shape_County,path(myPlace,"/myData/shape_County.shp"),delete_layer=TRUE)

# compare to:  shapeX_County <- counties(state = "CA", cb = TRUE) 


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

# OLD APPROACH ===============================================================================================

library(rgeos)
library(maptools) 

writeSpatialShape(shape_Tract,paste0(myPlace,"/myData/shape_Tract"))

shape_Comm      <- unionSpatialPolygons(shape_Tract, shape_Tract$comID)      # base spatial polygons on community

# work around to be able to use/access Community ID varibles
n.ID <-shape_Comm@polygons[[1]]@ID
for (i in 2:length(shape_Comm)){  n.ID <-c(n.ID,shape_Comm@polygons[[i]]@ID)}  # make accessible label for shapes
shape_Comm$comID <- as.character(n.ID)

# to associate County names with MSSAs and then link back to file -- more elegnat approach?
linkTemp    <- as.data.frame(cbdLinkCA %>% group_by(county,comID) %>% summarize(junk=n()))[,1:2] # removes "junk"
shape_Comm  <- merge(shape_Comm,linkTemp,by="comID")
#shape_Comm  <- geo_join(shape_Comm,linkTemp,by="comID")
writeSpatialShape(shape_Comm,paste0(myPlace,"/myData/shape_Comm"))

shape_County  <- unionSpatialPolygons(shape_Comm, shape_Comm$county)    
n.ID <-shape_County@polygons[[1]]@ID
for (i in 2:length(shape_County)){  n.ID <-c(n.ID,shape_County@polygons[[i]]@ID)}  # make accessible label for shapes
shape_County$county <- as.character(n.ID)

writeSpatialShape(shape_County,paste0(myPlace,"/myData/shape_County"))


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