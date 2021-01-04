library(dplyr)
library(ggplot2)
library(readxl)

geoMap          <- as.data.frame(read_excel("/mnt/projects/FusionData/CCB Project/0.CCB/myCBD//myInfo/County Codes to County Names Linkage.xlsx")) %>%
                     select(FIPSCounty,county=countyName)

lifeTableCounty <- readRDS("/mnt/projects/FusionData/CCB Project/0.CCB/myCBD/myData/e0ciCounty.rds") %>%
                    mutate(FIPSCounty=substr(GEOID,3,5))  %>%
                    left_join(geoMap,by="FIPSCounty")  %>%
                    filter(county == "Contra Costa") %>%
                    select(county, year_3_midyear = year, sex, raceEthnic = race7, ex, exlow, exhigh )


write_csv(lifeTableCounty,"LE_Birth_Contra_Costa.csv")
