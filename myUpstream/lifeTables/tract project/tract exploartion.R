server <- F
if (!server) source("g:/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")
if (server) source("/mnt/projects/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")

popTract <- readRDS(paste0(ccbUpstream, "upData/popTract.RDS"))
popTract0 <- popTract %>% 
  filter(yearG5 == "2018-2022", sex == "Total", ageGroup != "Total") %>% 
  select(GEOID, county2 = county, ageGroup, pop)

# Shuo
# percent missing by county
# age and race distributions of geocoded versus not
# investigation of addresses for deaths with non-geocode

if (F) {
  tractTest <- readRDS(paste0(securePlace,"myData/cbdDat0-INVESTIGATION-FILE.RDS")) 
  
  tractSave <- tractTest %>% 
    filter(yearG5 == "2018-2022", sex == "Total") %>%
    select(county,GEOID, ageGroup, raceCode)
  
  saveRDS(tractSave,"tractSave.RDS")
}

tractSave <- readRDS("tractSave.RDS")

# tractExplore00 <- tractSave %>%  
#   group_by(county,GEOID) %>%
#   summarize(N = n()) %>% ungroup() %>%
#   filter(GEOID !="")


# tractExplore0 <- tractSave %>%  
#   group_by(county,GEOID, ageGroup) %>%
#   summarize(N = n()) %>% ungroup() %>%
#   mutate(ageGroup = factor(ageGroup, levels= ageLink$ageName))

tractExplore00 <- tractSave %>%  
  group_by(county, GEOID, ageGroup) %>%
  summarize(N = n()) %>% 
  ungroup()

tractExplore0 <- tractExplore00 %>% 
  tidyr::complete(GEOID, ageGroup, fill=list(N=0)) 

# Split into 2 -------

# 1. New rows from complete(); All have missing counties
# Can remove missing age groups
tPopTract <- popTract0 %>% 
  distinct(GEOID, county2) %>% 
  mutate(pop = NA)

newRows <- tractExplore0 %>% 
  filter(N == 0, !is.na(ageGroup)) %>% 
  left_join(tPopTract) %>% 
  select(-county) %>% 
  rename(county = county2)

# 2. Already present rows; 
notNew <- tractExplore0 %>% 
  filter(N > 0) %>% 
  left_join(select(popTract0, -county2))

tractExplore <- bind_rows(newRows, notNew) %>% 
  mutate(ageGroup = factor(ageGroup, levels = ageLink$ageName)) %>% 
  arrange(GEOID, county, ageGroup)

saveRDS(tractExplore, "tractAge.RDS")


# Exploration -----------------------------------------------------------

# % of deaths with missing tracts by age group
check <- tractExplore %>% 
  mutate(tract = ifelse(GEOID == "", "missing", "present")) %>% 
  group_by(tract, ageGroup) %>% 
  summarise(n = sum(N)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = tract, values_from = n) %>% 
  mutate(pMissing = missing / (missing+ present))

ggplot(filter(check, !is.na(ageGroup)), aes(x = ageGroup, y = pMissing)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Age Group", y = "% of Deaths with missing Tract")



ggplot(filter(tractExplore), aes(x=ageGroup, y=N)) + geom_boxplot() 


tractExplore <- filter(tractExplore, GEOID !="", !is.na(ageGroup)) %>%
                 group_by(GEOID) %>%
                 summarise(minN = min(N))

# Code below does not work??
ggplot(filter(tractExplore, GEOID !=""), aes(x=ageGroup, y=N)) + geom_boxplot() 


ggplot(filter(tractExplore, GEOID !=""), aes(x=ageGroup, y=N)) + geom_boxplot() + scale_y_log10()


# saveRDS(tractExplore,"tractExplore")
