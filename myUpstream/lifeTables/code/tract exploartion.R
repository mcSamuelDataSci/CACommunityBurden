# Shuo
# investigation of addresses for deaths with non-geocode
# percent missing by county
# age and race distributions of geocoded versus not


tractTest <- readRDS(paste0(securePlace,"myData/cbdDat0-INVESTIGATION-FILE.RDS")) 



# Jaspreet; "full mat" to merge with this:

tractExplore <- tractTest %>% filter(yearG5 == "2018-2022", sex == "Total") %>% 
                  group_by(county,GEOID, ageGroup) %>%
                  summarize(N = n())


# best factor use for age-group

ggplot(filter(tractExplore, GEOID !=""), aes(x=ageGroup, y=N)) + geom_boxplot() 


ggplot(filter(tractExplore, GEOID !=""), aes(x=ageGroup, y=N)) + geom_boxplot() + scale_y_log10()


saveRDS(tractExplore,"tractExplore")
