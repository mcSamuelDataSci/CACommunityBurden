# Create the hatch shape
library(sf)
library(tmap)
library(purrr)
county <- read_sf("myCBD/myData/shape_County.shp")
tract <- read_sf("myCBD/myData/shape_Tract.shp")
community <- read_sf("myCBD/myData/shape_Comm.shp")


#************************************************
# Grid function ----
#************************************************

# You can use st_make_grid but it adds nodes at all
# intersections. Here it's just one big grid

# hatchshp <- sf::st_make_grid(county, n = c(125, 125)) %>% 
#   st_cast("LINESTRING") %>% 
#   st_union() %>% 
#   st_sf()

simpler_grid <- function(shp, cnt_lines = 100, do_intersection = TRUE){

  # shp <- county
  if(st_is_longlat(shp)) warning("Careful, I'm not sure this works properly with an unprojected CRS")
  bbox <- sf::st_bbox(shp)
  
  v_xcoord <- seq(bbox['xmin'], bbox['xmax'], length = cnt_lines)
  v_xcoord <- rep(v_xcoord, each = 2)
  v_ycoord <- rep(c(bbox['ymin'], bbox['ymax']), cnt_lines)
  
  h_ycoord <- seq(bbox['ymin'], bbox['ymax'], length = cnt_lines)
  h_ycoord <- rep(h_ycoord, each = 2)
  h_xcoord <- rep(c(bbox['xmin'], bbox['xmax']), cnt_lines)
  
  
  xcoord <- c(v_xcoord, h_xcoord)
  ycoord <- c(v_ycoord, h_ycoord)
  
  vals <- rep(1:(cnt_lines*2), each = 2)
  lines <- map(1:(cnt_lines*2), function(i){
    matrix(c(xcoord[which(vals == i)], ycoord[which(vals == i)]), ncol = 2)
  })
  
 
 grid <- st_multilinestring(lines) %>% 
    st_sfc(crs = st_crs(shp)) 
  
 if(do_intersection){
   grid <- st_intersection(shp, grid)
     
 }

  grid
}

county_hatch <- simpler_grid(county, cnt_lines = 300)
tm_shape(county) + tm_polygons() + tm_shape(county_hatch %>% filter(county == "Alameda")) + tm_lines()
tm_shape(county %>% filter(county == "Alameda")) + tm_polygons() + tm_shape(county_hatch %>% filter(county == "Alameda")) + tm_lines()



# Even this size does not capture all tracts
tract_hatch <- simpler_grid(tract, cnt_lines = 300)
write_rds(tract_hatch, "~/junk/blah.rds")







#************************************************
# Michael's code ----
#************************************************

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
