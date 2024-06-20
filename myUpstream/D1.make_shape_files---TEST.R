# =====================================================================================
# "shapeProcessor.R" file                                                             |
#            Set locations and loads libraries                                        |
#            Read CA census tract shape via with tigris package                       |
#            Process and save shape_Tract, _Community, and _County files              |
#                                                                                     |   
# =====================================================================================
server <- F
if (!server) source("g:/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")
if (server) source("/mnt/projects/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")

myDrive  <- ifelse(server, "/mnt/projects/FusionData", "g:/FusionData")
myPlace  <- paste0(myDrive,"/0.CCB/myCCB")  
upPlace  <- paste0(myDrive,"/0.CCB/myUpstream")  

library(tigris)     # Other geographies: block_groups, blocks, counties, county_subdivisions, places, pumas, school_districts, states, zctas
library(dplyr)
library(sf)         # simple features GIS
library(fs)         # just for path function
library(readr)
library(tmap)


cbdLinkCA  <- read.csv(path(myPlace,"/Standards/Tract to Community Linkage.csv"),colClasses = "character")

# -- Read, process, and write CA Tract File -------------------------------------------------------------------------------

options(tigris_class = "sf")  # Read shape files as Simile Features objects

proj1 <- "+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# Get data with tigris



# res_ca      <- county_filter(cnty_ca, min_area = 1.01e+15, rowmap = FALSE)
# shape_Tract <- st_intersection(tr_ca, res_ca)


shape_County <- counties(state = "CA", cb = TRUE) %>% 
  select(county=NAME, geometry) %>%
  st_transform(crs = proj1)
  st_write(shape_County,path(myPlace,"/myData/shape_County.shp"),delete_layer=TRUE)


shape_Tract0  <- tracts(state = "CA", cb = TRUE, year= 2019) %>%   
                  select(GEOID, geometry, COUNTYFP) %>%
                  st_transform(crs = proj1)


shape_Tract <- shape_Tract0 %>% full_join(cbdLinkCA, by="GEOID") %>%   
               select(GEOID, comID, COUNTYFP, county, geometry) 


st_write(shape_Tract,path(myPlace,"/myData/shape_Tract.shp"),delete_layer=TRUE) 


# aggregate census tracts into community shapes -- Cool, huh?
shape_Comm  <- shape_Tract %>% group_by(county,comID) %>% summarize() %>% ungroup()
st_write(shape_Comm,path(myPlace,"/myData/shape_Comm.shp"),delete_layer=TRUE)

