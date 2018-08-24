#Read and merge shape, death, pop and other files

library(fs)
library(readr)
library(dplyr)
library(tigris) #Added this one per Zev's instructions below

myDrive  <- "H:"                            
myPlace  <- paste0(myDrive,"/0.CBD/myUpstream")  


#R should usually be able to detect character vs. numeric automatically based on first 1000 rows (?) 
#but maybe Michael added col types = character (c) or number(n) because deaths file has a lot of empty cells
#Changed from "ccnnn"
d.correct <- read_csv(path(myPlace,"censusTracts/myData","Deaths_County_Corrected.csv"),col_types = "ncncn")
#Don't need this since want all vars in spreadsheet anyway:     select(GEOID,COUNTY,deaths1,wrong_county1,wrong_deaths_co1)


#Use pipes to tell R that final tables (e.g., d.mssa00) should only have selected variables
d.mssa00  <- read_csv(path(myPlace,"censusTracts/myData","mssa00.csv"))  %>%
            select(GEOID,COUNTY,POP2000,POP2000CIV)
d.mssa00  <- read_csv(path(myPlace,"censusTracts/myData","mssa00.csv")) %>%
                  select(GEOID,COUNTY,POP2000,POP2000CIV)
d.mssa13 <- read_csv(path(myPlace,"censusTracts/myData","mssa13.csv")) %>%
                select(GEOID,COUNTY,POP2013,POP2013CIV)
d.pov <- read_csv(path(myPlace,"censusTracts/myData","pov_2006_10.csv")) %>%
                select(GEOID,COUNTY)
d.group <- read_csv(path(myPlace,"censusTracts/myData","SVI_CDC_group_living.csv")) %>%
                select(GEOID,COUNTY,tot_pop_grp,e_groupQ)
#Have to use col_types below or R gets confused by one very big area of water way down the list
d.shape <- read_csv(path(myPlace,"censusTracts/myData","tracts_tiger.csv"),col_types = "nnnnnc") %>%
                select(GEOID,COUNTY)



#MS: make geoid 11 digits with 0
#Michael's script was above the code below "d.raw..." but it would apply to all
#Postponing this for now since all GEOIDs in imported files should have same number of digits

#Guessing for now we're using the corrected deaths file with totals for all years as before, 
#but CCB shows for each year as in the raw file below, so why did we do that? 
                
d.raw      <- read_csv(path(myPlace,"censusTracts/myData","rawDeaths.csv"),col_types = "ncnc") %>%
                group_by(GEOID,county) %>%
                summarize(n=sum(Ndeaths))


#Starting adapting code below from Zev's link but not sure yet how to match on both county AND GEOID
#although I think we've fixed only GEOID's in wrong counties so wouldn't matter except for mssa00
#http://zevross.com/blog/2015/10/14/manipulating-and-mapping-us-census-data-in-r-using-the-acs-tigris-and-leaflet-packages-3/#do-the-merge-tigris
# 4) Do the merge (tigris)
#The package tigris has a nice little merge function to do the sometimes difficult merge between the spatial and tabular data.
merged<- geo_join(d.shape, d.correct, d.mssa00, d.mssa13, d.pov, d.group, "GEOID", "GEOID")
#merged<- geo_join(d.shape, d.correct, d.mssa00, d.mssa13, d.pov, d.group, "GEOID", "GEOID", "GEOID", "GEOID", "GEOID", "GEOID")
# there are some tracts with no land that we should exclude:
merged <- merged[merged$ALAND>0,]



