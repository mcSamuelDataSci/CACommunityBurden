# if(1==2){
#   myLHJ="CALIFORNIA" 
#   myYear = 2019
#   myMeasure = "aRate"
#   mySex   = "Total"
#   myLogTrans=FALSE
#   myMultiRace = FALSE
#   myCompare = "highest rate"
#   }
# 
# 
#  server <- T
#  if (!server) source("g:/FusionData/Standards/FusionStandards.R")
#  if (server) source("/mnt/projects/FusionData/Standards/FusionStandards.R")
# 
# datCounty <- read_rds(path(ccbData,"real/datCounty.RDS")) 
#  
# dat.1 <- filter(datCounty,county == myLHJ, year==myYear, sex == "Total", Level == "lev2") %>%
#               left_join(deathCauseLink, by="causeCode")
# 
# 
# library(treemap)
# 
# 
# tDat <- dat.1 %>% group_by(topLevName) %>% summarize(N= sum(Ndeaths))
# 
# treemap(tDat,
#         index="topLevName",
#         vSize="N",
#         type="index"
# )
# 
# 
# 
# 
# tDat <- dat.1 %>% group_by(topLevName,causeNameShort) %>% summarize(N= sum(Ndeaths))
# 
# x <- treemap(tDat,
#         index=c("topLevName","causeNameShort"),
#         vSize="N",
#         type="index",
#         bg.labels = c("white"),
#         align.labels=list(
#           c("center","center"),
#           c("right","bottom")
#         )
#         )
# 
# 
# library(d3treeR)
# 
# xx <- d3tree2(x)
# 
# 
# 
# 
# library(htmlwidgets)
# 
# saveWidget(xx,file = path("causeTreeMap.html"))
# 
