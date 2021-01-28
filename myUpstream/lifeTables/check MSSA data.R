server <- T
if (!server) source("g:/FusionData/Standards/FusionStandards.R")
if (server) source("/mnt/projects/FusionData/Standards/FusionStandards.R")
library(summarytools)

lMSSA <- readRDS(paste0(ccbData,"/e0ciMSSA1.RDS")) %>% filter(sex != "TOTAL", year == 2016) %>% 
            mutate(sex = ifelse(sex=="MALE","Male",ifelse(sex=="FEMALE","Female","other")))

dMSSA <- readRDS(paste0(ccbData,"/real/datComm.RDS")) %>% filter(causeCode == "0", sex != "Total") 


checkData <- full_join(lMSSA,dMSSA,c("comID","sex")) %>%
               select(county,comID,sex,ex,exlow,exhigh,Ndeaths,aRate,population, cDeathRate) %>%
               mutate(junkRate = round(100000/5*Ndeaths/population,1))

ggplot(data=checkData, aes(x=ex,y=junkRate,color=sex)) + geom_point()
ggplot(data=checkData, aes(x=ex,y=aRate,color=sex)) + geom_point()


temp <- filter(checkData,aRate > 1250)

# geoMap          <- as.data.frame(read_excel(paste0(ccbInfo,"/County Codes to County Names Linkage.xlsx"))) %>%
#   select(FIPSCounty,county=countyName)
# 
#   mutate(FIPSCounty=substr(GEOID,3,5))  %>%
#   left_join(geoMap,by="FIPSCounty") %>%
#   mutate(sex = str_to_title(sex))



# ACS tract data -- collapsed to MSSA and controlled to DOF county (P3-complete?) 
#  ....  P3-compete for < 2010
