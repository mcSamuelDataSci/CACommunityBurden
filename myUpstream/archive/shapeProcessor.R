# =====================================================================================
# "shapeProcessor.R" file                                                             |
#            Set locations and loads libraries                                        |
#            Read CA census tract shape via with tigris package                       |
#            Process and save shape_Tract file                                        |
#            Process and save shape_Community file                                    |
#            Process and save shape_County file                                       |
#            notes                                                                    |
#                                                                                     |   
# =====================================================================================

myDrive  <- "e:"                            
myPlace  <- paste0(myDrive,"/0.CBD/myCBD")  

library(rgeos)    # complex issues here -- need to load this BEFORE maptools -- may be other related issues to solve (see notes if unionSpatialPolygons does not work)
library(tigris)
library(maptools) # requires sp package -- and may be only thing I am now actually using here is unionSpatialPolygons from sp?
library(dplyr)

#-- Read Info file --------------------------------------------------------------------------------

# cbdLinkCA links census tracts ("GEOID") to "comID" (community ID; currently adjusted MSSA_ID)
cbdLinkCA  <- read.csv(paste0(myPlace,"/myInfo/cbdLinkCA.csv"),colClasses = "character")  # colClasses... essential for keeping leading "0"   
# 8036 tracts

# these (9) census tracts are in the shape files (8043 elements) but not in our CA data; all/mostly water?
bad <- c(c("06081990100","06083990000","06111990100","06037990300","06001990000","06061990000","06017990000","06037137000"),"06075980401")

# -- Read, process, and write CA Tract File -------------------------------------------------------------------------------

shape_Tract   <- tracts(state = "CA", cb = TRUE)                 # tigris
# 8043 tracts
shape_Tract   <- merge(shape_Tract, cbdLinkCA, by="GEOID")   # merge much "cleaner" than tigris "geo_join"  -- explore?
shape_Tract   <- shape_Tract[!(shape_Tract$GEOID %in% bad),]     # remove 'bad' tracts
# 8034 tracts

writeSpatialShape(shape_Tract,paste0(myPlace,"/myData/shape_Tract"))

# ERROR
# shapeX <- gSimplify(shape_Tract,tol = 0.05, topologyPreserve = TRUE)
# writeSpatialShape(shapeX,paste0(myPlace,"/myData/shapeX"))

#-- Create new Community shape file based on MSSAs --------------------------------------------------------------------------

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


#-- Create County shape file based same approach as above-----------------------------------------------------------------------

shape_County  <- unionSpatialPolygons(shape_Comm, shape_Comm$county)    
n.ID <-shape_County@polygons[[1]]@ID
for (i in 2:length(shape_County)){  n.ID <-c(n.ID,shape_County@polygons[[i]]@ID)}  # make accessible label for shapes
shape_County$county <- as.character(n.ID)

writeSpatialShape(shape_County,paste0(myPlace,"/myData/shape_County"))

# could compare to
# shapeX_County <- counties(state = "CA", cb = TRUE) 
# ... simplier object...





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

