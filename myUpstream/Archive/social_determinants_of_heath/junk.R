library(tidycensus)

census_api_key("b751f8899055dbb118c2aeb0f1f736d160a4816f")

sdoh.internet <- get_acs(geography = "county", 
        variables = c(internet = "S2801"), 
        state = "CA")


library(acs)
#internet
acs.lookup(endyear=2017,span=5,table.number="S2801")


#poverty
acs.lookup(endyear=2017,span=5,table.number="DP03",case.sensitive = F)
