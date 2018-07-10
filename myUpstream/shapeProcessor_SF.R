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

#library(rgeos)    # complex issues here -- need to load this BEFORE maptools -- may be other related issues to solve (see notes if unionSpatialPolygons does not work)
library(tigris)
#library(maptools) # requires sp package -- and may be only thing I am now actually using here is unionSpatialPolygons from sp?
library(dplyr)
library(sf)

#-- Read Info file --------------------------------------------------------------------------------

# cbdLinkCA links census tracts ("GEOID") to "comID" (community ID; currently adjusted MSSA_ID)
cbdLinkCA  <- read.csv(paste0(myPlace,"/myInfo/cbdLinkCA.csv"),colClasses = "character")  # colClasses... essential for keeping leading "0"  
# 8036 tracts

# these (9) census tracts are in the shape files (8043 elements) but not in our CA data; all/mostly water?
bad <- c(c("06081990100","06083990000","06111990100","06037990300","06001990000","06061990000","06017990000","06037137000"),"06075980401")

# -- Read, process, and write CA Tract File -------------------------------------------------------------------------------

options(tigris_class = "sf")

shape_Tract   <- tracts(state = "CA", cb = TRUE)                 # tigris
# 8043 tracts
shape_Tract   <- merge(shape_Tract, cbdLinkCA, by="GEOID")   # merge much "cleaner" than tigris "geo_join"  -- explore?
shape_Tract   <- shape_Tract[!(shape_Tract$GEOID %in% bad),]     # remove 'bad' tracts
# 8034 tracts

shape_Tract <- select(shape_Tract,comID,county,geometry)



#-- Create new Community shape file based on MSSAs --------------------------------------------------------------------------


#  http://r-sig-geo.2731867.n2.nabble.com/How-does-sf-do-rgeos-gUnaryUnion-or-maptools-unionSpatialPolygons-td7591160.html 
#shape_Z <- st_union(shape_Tract)
#shape_X <- st_union(shape_Tract,by_feature=TRUE)
#shape_X <- aggregate(shape_Tract,by=shape_Tract$comID)


#https://github.com/r-spatial/sf/issues/290
sComm <- shape_Tract %>% group_by(comID) %>% summarize()




#OLD
#shape_Comm      <- unionSpatialPolygons(shape_Tract, shape_Tract$comID)      # base spatial polygons on community

# work around to be able to use/access Community ID varibles
#n.ID <-shape_Comm@polygons[[1]]@ID
#for (i in 2:length(shape_Comm)){  n.ID <-c(n.ID,shape_Comm@polygons[[i]]@ID)}  # make accessible label for shapes
#shape_Comm$comID <- as.character(n.ID)

# to associate County names with MSSAs and then link back to file -- more elegnat approach?
linkTemp    <- as.data.frame(cbdLinkCA %>% group_by(county,comID) %>% summarize(junk=n()))[,1:2] # removes "junk"
shape_Comm  <- merge(shape_Comm,linkTemp,by="comID")
#shape_Comm  <- geo_join(shape_Comm,linkTemp,by="comID")
writeSpatialShape(shape_Comm,paste0(myPlace,"/myData/shape_Comm"))

