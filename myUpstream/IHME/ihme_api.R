library(dplyr)
options(stringsAsFactors=FALSE)
library(jsonlite)
options(max.print = 50)

# MCS IHME API KEY
ihmeKey <- "5a4fc200e1af720001c84cf91e34303eca334ffa8a35722aac008232"
keyText <- paste0("authorization=",ihmeKey)
APIroot <- "https://api.healthdata.org/healthdata/v1/"
subSet <- "data/gbd/cause/?age_group_id=22&location_id=527&"

# Set variables and make URL
make_url <- function() {

  age <- 22 # All ages
  location <- 527 # California
  # measure <- paste(as.character(c(3)), collapse = ",") # This variable now set in get_data function. 1 Deaths, 2 DALYs, 3 YLDs, 4 YLLs (CANT GET MORE THAN 1 MEASURE AT ONCE??)
  metric <- paste(as.character(c(1:3)), collapse = ",") # 1 Number, 2 Percent, 3 Rate
  sex <- paste(as.character(c(1:3)), collapse = ",") # 1 Male, 2 Female, 3 Both
  year <- paste(as.character(c(2001:2016)), collapse = ",") # 2001 through 2016
  # cause <- paste(as.character(c(294:300)), collapse = ",") # This variable now set in get_data function.
  # etiology <- paste(as.character(c(187)), collapse = ",") # This variable not used currently. 187 Influenza
  
  # Make url
  url <- paste0(APIroot, subSet, "age_group_id=", age, "&location_id=", location,
                "&metric_id=", metric, "&sex_id=", sex, "&year_id=", year, "&", keyText)
  return(url)
}

# Get data based on URL
get_data <- function(url){
  data = data.frame()  # Initialize value_data 
  for (cause_id in 294) {  # Loop through causes. Set to just 294 (all causes), but can be set to 294:953 for every individual cause
    for (measure_id in 1:4){  # Loop through all 4 measures
      this_url <- paste0(url, "&measure_id=", measure_id, "&cause_id=", cause_id)
      data <- rbind(data, as.data.frame(jsonlite::fromJSON(this_url)$data))
    }
  }
  return(data)
}

# Reformat the data
format_data <- function(data_frame){
  # 1 Measure 2 Year 3 Location 4 Sex 5 Age 6 Cause 7 Metric 8 Value 9 Minimum 10 Maximum
  value_data <- mutate(value_data, 
                       V1 = as.numeric(V1),
                       V2 = as.numeric(V2),
                       V3 = as.numeric(V3),
                       V4 = as.numeric(V4),
                       V5 = as.numeric(V5),
                       V6 = as.numeric(V6),
                       V7 = as.numeric(V7),
                       V8 = as.numeric(V8),
                       V9 = as.numeric(V9),
                       V10 = as.numeric(V10))
  colnames(data_frame) <- c("measure_id", "year", "location_id", "sex_id", "age_id",
                            "cause_id", "metric_id", "val", "min", "max")
  return(data_frame) 
}

# Merge with parent data for examining the hierarchy
merge_with_parent <- function(val_data){
  # "Cause set" metadata endpoint 
  causeURL0 <- paste0(APIroot,"metadata/cause_set","/?",keyText)
  
  #  One can recreate our cause hierarchy by examining this endpoint's response's "cause_id" and "parent_id" values. "All causes" (cause_id 294) is the root cause.
  causeURL  <- paste0(APIroot,"metadata/cause/?cause_set_id=3","&",keyText)
  parent_data <- jsonlite::fromJSON(causeURL)
  parent_meta <- parent_data$meta
  parent_data <- parent_data$data
  parent_data <- as.data.frame(parent_data)
  colnames(parent_data) <-c("cause_id", "cause_name", "acause", "sort_order", "level", "parent_id")
  
  # Merge data frames
  with_names <- merge(val_data, parent_data, by="cause_id")
  return(with_names)
}

# Run functions to make all_data data set
value_data <- get_data(make_url())
value_data <- format_data(value_data)
all_data <- merge_with_parent(value_data)

# A <- merge_with_parent(format_data(get_data(make_url())))  # Why doesn't this work also?

### Other exploration

# url <- paste0(APIroot, subSet, "age_group_id=", age, "&location_id=", location, "&measure_id=", measure,
#               "&metric_id=", metric, "&sex_id=", sex, "&year_id=", year, "&cause_id=", cause, "&", keyText)
# A <- rbind(value_data, as.data.frame(jsonlite::fromJSON(url)$data))
# A <- A$data
# A <- as.data.frame(A)
# 
# 
# causeURL  <- paste0(APIroot,"metadata/measure/?age_group_id=22&measure_id=2?","&",keyText)
# 
# print(causeURL)
