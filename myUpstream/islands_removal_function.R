# library(dplyr)
# library(tigris)
# library(tmap)
# library(sf)
# library(rmapshaper)
# 
# 
# options(tigris_class = "sf")  # Read shape files as Simiple Features objects
# 


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
  
  # if(domap){
  #   orig <- tm_shape(.data) + tm_polygons("GEOID", legend.show = FALSE)
  #   filt <- tm_shape(final_counties) + tm_polygons("GEOID", legend.show = FALSE)
  #   
  #   if(rowmap){
  #     nc = 1
  #     nr = 2
  #     
  #   }else{
  #     nc = 2
  #     nr = 1
  #   }
  #   print(tmap_arrange(orig, filt, ncol = nc, nrow = nr))
  # }
  
  final_counties
}


