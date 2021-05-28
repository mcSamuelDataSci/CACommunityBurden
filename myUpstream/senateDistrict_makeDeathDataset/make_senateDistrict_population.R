source("/mnt/projects/FusionData/SDOH/pullACS_function.R")

state legislative district (upper chamber)


mySDOH_vars <- readxl::read_excel("/mnt/projects/FusionData/SDOH/linkageSDOH.xlsx", sheet = "Age 5 Year Groups")
mySDOH_vars <- mySDOH_vars$measureLabel

testFunc <- pullACS(State=06, # 06 = California
                    Geography="state legislative district (upper chamber)", # Options: county, mssa, tract, zcta... for acs1, puma
                    Survey="acs5",
                    Year=2019,
                    moeLevel=90,
                    asList = F, # Want as a list, or as one data frame?
                    server = T,
                    sdohVars = mySDOH_vars[1])

testFunc1 <- pullACS(State=06, # 06 = California
                    Geography="tract", # Options: county, mssa, tract, zcta... for acs1, puma
                    Survey="acs5",
                    Year=2019,
                    moeLevel=90,
                    asList = F, # Want as a list, or as one data frame?
                    server = T,
                    sdohVars = mySDOH_vars[1])

tractToDistrict <- caLegTracts1 %>%
  full_join(testFunc1, by = "GEOID") 

%>%
  group_by(DISTRICTID, measureLabel) %>%
  summarise(numerator = sum(numerator), 
            denominator = sum(denominator))


# Check
check <- tractToDistrict %>%
  group_by(geoName) %>%
  summarise(count = n())

checkAgain <- caLegTracts1 %>%
  group_by(GEOID) %>%
  summarise(count = n()) %>%
  filter(count > 1) %>%
  left_join(caLegTracts1, by = "GEOID")

comp <- testFunc %>%
  select(GEOID, num = numerator, denom = denominator) %>%
  mutate(GEOID = str_sub(GEOID, start = 4, end = 5)) %>%
  full_join(tractToDistrict, by = c("GEOID" = "DISTRICTID")) %>%
  mutate(diff = denom - denominator)



tractToMSSA <- read_csv("/mnt/projects/FusionData/0.CCB/myCCB/myInfo/Tract to Community Linkage.csv") %>%
  group_by(GEOID) %>%
  summarise(count = n())
