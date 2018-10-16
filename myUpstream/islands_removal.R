library(dplyr)
library(tigris)
library(tmap)
library(sf)
library(rmapshaper)
options(tigris_class = "sf")  # Read shape files as Simiple Features objects

# Get data with tigris
tr_ca <- tracts(state = "CA", cb = TRUE)  # 8043 tracts  # Obtain tracts boundry tiger files from Census
cnty_ca <- counties(state = "CA", cb = TRUE)

# mass
#tr_ma <- tracts(state = "MA", cb = TRUE)  # 8043 tracts  # Obtain tracts boundry tiger files from Census
#cnty_ma <- counties(state = "MA", cb = TRUE)

proj1 <- "+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# Project to teale meters
tr_ca <- st_transform(tr_ca, crs = proj1)
cnty_ca  <- st_transform(cnty_ca, crs = proj1)




# Project to UTM 19 (eastern Mass)
#tr_ma <- st_transform(tr_ma, crs = 32619)
#cnty_ma <- st_transform(cnty_ma, crs = 32619)


#************************************************
# Massachusetts as test case ----
#************************************************

island_county_processing <- function(exploded, fips){
  # exploded <- a
  # fips <- cnty_removed[1]
  tmp <- filter(exploded, GEOID == fips)
  maxarea <- st_area(tmp) %>% max() 
  attr(maxarea, "class") <- NULL
  res <- ms_filter_islands(tmp, min_area = maxarea-1)
  res
}


county_filter <- function(.data, min_area = 2000000000, domap = TRUE, rowmap = TRUE){
  # Explode county shapefile
  exploded_counties <- .data %>% ms_explode()
  
  # Filter islands
  filtered_counties <- exploded_counties %>% ms_filter_islands(min_area = min_area)
  
  
  # Identify counties that no longer exist
  missing_counties <- unique(exploded_counties$GEOID)[!unique(exploded_counties$GEOID)%in%unique(filtered_counties$GEOID)]
  
  
  # Re-dissolve the filterd counties
  final_counties <- filtered_counties %>% group_by(GEOID) %>% summarise_all(first)
  
  if(length(missing_counties) != 0){
    missing_counties <- purrr::map(missing_counties, ~island_county_processing(exploded_counties, .))
    missing_counties <- do.call("rbind", missing_counties)
    final_counties <- rbind(final_counties, missing_counties)
  }
  
  
  
  
  # Return the missing counties
  
  
  if(domap){
    orig <- tm_shape(.data) + tm_polygons("GEOID", legend.show = FALSE)
    filt <- tm_shape(final_counties) + tm_polygons("GEOID", legend.show = FALSE)
    
    if(rowmap){
      nc = 1
      nr = 2
      
    }else{
      nc = 2
      nr = 1
    }
    print(tmap_arrange(orig, filt, ncol = nc, nrow = nr))
  }
  
  final_counties
}

# Note 1.01e+9 for channel islands state park
#res_ma <- county_filter(cnty_ma, min_area = 1.01e+9)
res_ca <- county_filter(cnty_ca, min_area = 1.01e+15, rowmap = FALSE)

tr_ca_clip <- st_intersection(tr_ca, res_ca)
#tr_ma_clip <- st_intersection(tr_ma, res_ma)

# LOS ANGELES COUNTY

# Before
before <- filter(tr_ca, COUNTYFP == "075") %>% 
  tm_shape() + 
  tm_polygons()

# After
after <- filter(tr_ca_clip, COUNTYFP == "075") %>% 
  tm_shape() + 
  tm_polygons()

tmap_arrange(before, after, nrow = 2)



# Before
before <- filter(res_ca, COUNTYFP == "075") %>% 
  tm_shape() + 
  tm_polygons()

# After
after <- filter(cnty_ca, COUNTYFP == "075") %>% 
  tm_shape() + 
  tm_polygons()

tmap_arrange(after, before, nrow = 2)







# Before
before <- filter(tr_ma, COUNTYFP %in% c("001", "007", "019")) %>% 
  tm_shape() + 
  tm_polygons()

# After
after <- filter(tr_ma_clip, COUNTYFP %in% c("001", "007", "019")) %>% 
  tm_shape() + 
  tm_polygons()

tmap_arrange(before, after, nrow = 1)



