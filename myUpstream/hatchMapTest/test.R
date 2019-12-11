hatchshp <- read_rds("f:/0.CBD/myAnalyses/hatchMapTest/hatchshp.rds")

dat.1      <- filter(datCounty,year==2018,sex=="Total", CAUSE=="C") 
map.1      <- left_join(shape_County, dat.1, by=c("county")) 

smallCounties <- dat.1$county[dat.1$pop < 20000]

map1 <- tm_shape(map.1) + tm_polygons("aRate")

addhatch <- function(.map, .counties, .hatch = hatchshp, .color="gray") {
                    hatch <- filter(.hatch, NAME %in% .counties)
                    map1 + tm_shape(hatch) + tm_lines(col = .color)
}

addhatch(map1, smallCounties)



tm_shape(hatchshp) + tm_lines()
