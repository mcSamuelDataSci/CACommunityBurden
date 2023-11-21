library(sqldf)

server <- F
if (!server) source("g:/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")
if (server) source("/mnt/projects/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")


raceLink <- raceLink %>% select(raceCode, raceNameShort)


#---------------------------------------------------------------------------------------------




tDat <- readRDS(paste0(ccbData,"real/oshpd_PDD_any.rds")) %>% filter(sex=="Total", county == "CALIFORNIA")
