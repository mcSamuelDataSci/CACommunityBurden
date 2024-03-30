server <- F
if (!server) source("g:/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")
if (server) source("/mnt/projects/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")



# Shuo
# percent missing by county
# age and race distributions of geocoded versus not
# investigation of addresses for deaths with non-geocode


tractTest <- readRDS(paste0(securePlace,"myData/cbdDat0-INVESTIGATION-FILE.RDS")) 

tractSave <- tractTest %>% 
               filter(yearG5 == "2018-2022", sex == "Total") %>%
               select(county,GEOID, ageGroup, raceCode)

saveRDS(tractSave,"tractSave.RDS")
tractSave <- readRDS("tractSave.RDS")


tractExplore0 <- tractSave %>%  
  group_by(county,GEOID, ageGroup) %>%
  summarize(N = n()) %>% ungroup() %>%
  mutate(ageGroup = factor(ageGroup, levels= ageLink$ageName))


tractExplore <- tractExplore0 %>% tidyr::complete(GEOID, ageGroup, fill=list(N=0))


ggplot(filter(tractExplore), aes(x=ageGroup, y=N)) + geom_boxplot() 


ggplot(filter(tractExplore, GEOID !=""), aes(x=ageGroup, y=N)) + geom_boxplot() 


ggplot(filter(tractExplore, GEOID !=""), aes(x=ageGroup, y=N)) + geom_boxplot() + scale_y_log10()


# saveRDS(tractExplore,"tractExplore")
