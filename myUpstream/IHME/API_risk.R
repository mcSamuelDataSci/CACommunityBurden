library(dplyr)
library(jsonlite)
library(docstring)
options(stringsAsFactors=FALSE)
options(max.print = 500)


# MCS IHME API Key-----------------------------------------------------------------------


ihme_key <- "5a4fc200e1af720001c84cf91e34303eca334ffa8a35722aac008232"
key_text <- paste0("authorization=",ihme_key)
api_root <- "https://api.healthdata.org/healthdata/v1/"
cause_subset <- "data/gbd/cause/?"
risk_subset <- "data/gbd/risk/?"

# TODO-----------------------------------------------------------------------

# Large Scale:
  # Runtime for column naming
  # Runtime for matrix vs df
# Efficiency increases?
  # Order at somepoint?
# File layout for storing data to access from other scripts
# Data type: matrix vs data frame vs ...?

# Define functions-----------------------------------------------------------------------

make_url <- function(subset=risk_subset, location_id=527) {
  #' Make base case URL
  return(paste0(api_root, subset,
                "age_group_id=", 22,
                "&cause_id=", 294,
                "&location_id=", location_id,
                "&metric_id=", paste0(as.character(c(1:3)), collapse = ","),
                "&sex_id=", paste0(as.character(c(1:3)), collapse = ","),
                "&year_id=", paste0(as.character(c(1990:2016)), collapse = ","),
                "&", key_text))
}

make_data_subset <- function(URL,measure_id=1,risk_id1=82,risk_id2=341){
  #' Inputs: URL, Optional: measure_id, cause_id1, cause_id2
  #' Output: data frame of json data call

  data <- paste0(URL,
                    "&measure_id=", paste0(as.character(measure_id), collapse = ","),
                    "&risk_id=", paste0(as.character(c(risk_id1:risk_id2)), collapse = ",")) %>% # Up to 294:460 works, but 294:461 (or more) gives lexical error Michael:294:731
    jsonlite::fromJSON()
  colnames(data$data) <- data$meta$fields
  return(as.data.frame(data$data))
}

merge_with_parent <- function(value_data){
  #'Input: value_data as created by make_data_subset function
  #'Output: data frame of value_data, now including parent data
  #'Parent Data Fields: cause_id, cause_name, acause, sort_order, level, parent_id

  parent  <- paste0(api_root,"metadata/risk/?risk_set_id=1","&",key_text) %>%  # output as CSV file for Michael?
    jsonlite::fromJSON()
  colnames(parent$data) <- parent$meta$fields
  return(merge(value_data, as.data.frame(parent$data), by="risk_id"))
}

# Load and format data-----------------------------------------------------------------------

start_time <- Sys.time()

output_data <- bind_rows(make_data_subset(URL=make_url(), 1, 82, 199),
                         make_data_subset(URL=make_url(), 1, 200, 341),
                         make_data_subset(URL=make_url(), 2, 82, 199),
                         make_data_subset(URL=make_url(), 2, 200, 341),
                         make_data_subset(URL=make_url(), 3, 82, 199),
                         make_data_subset(URL=make_url(), 3, 200, 341),
                         make_data_subset(URL=make_url(), 4, 82, 199),
                         make_data_subset(URL=make_url(), 4, 200, 341)) %>%
  merge_with_parent() %>%
  mutate_at(vars(c(1:11,14:ncol(.))), funs(as.numeric)) %>%
  mutate_at(vars(c(1:11,14:ncol(.))), funs(round(.,digits=4)))

saveRDS(output_data, "risk_data.rds")

# Get runtime to check for potential efficiency improvements
end_time = Sys.time()
print(end_time-start_time)

# Removal of non-necessary objects-----------------------------------------------------------------------

rm(list=setdiff(ls(), "output_data"))

# Tests-----------------------------------------------------------------------

# Check if data is same as IHME

smoking_data <- jsonlite::fromJSON(paste0(api_root, risk_subset,
                                  "age_group_id=", "22",
                                  "&location_id=", "527",
                                  "&metric_id=", "1",
                                  "&sex_id=", "3",
                                  "&measure_id=", "3",
                                  "&cause_id=", "294",
                                  "&year_id=", "1990,2016",
                                  "&risk_id=", "99,108",
                                  "&", key_text))
colnames(smoking_data$data) <- smoking_data$meta$fields
smoking <- as.data.frame(smoking_data$data)

smoking_parent  <- paste0(api_root,"metadata/risk/?risk_set_id=1","&",key_text) %>%
  jsonlite::fromJSON()
colnames(smoking_parent$data) <- smoking_parent$meta$fields

example <- merge(as.data.frame(smoking), as.data.frame(smoking_parent$data), by="risk_id")




# Other tests
fields_test <- function(data_frame) {
  #' Fields (1:10, 13:15) should be "numeric"
  #' Fields (11:12) should be "character"
  sapply(data_frame, class)
}

fields_test(risk_data)

factor_test <- function(data_frame) {
  #' Two ways to check if we have any factored data. We shouldn't
  
  for (y in 1:nrow(data_frame)) {
    for (x in 1:ncol(data_frame)) {
      if (sapply(data_frame[y,x],is.factor)) {
        print("Houston, we have a factor")
      }
    }
  }
  return(sapply(data_frame, is.factor))
}




# Bullshit notes-----------------------------------------------------------------------

### Merge with parent:
# One can recreate our cause hierarchy by examining this endpoint's response's "cause_id" and "parent_id" values. "All causes" (cause_id 294) is the root cause.
# "Cause set" metadata endpoint - Don't need this for anything....yet
# causeURL0 <- paste0(api_root,"metadata/cause_set","/?",key_text)
# 
# 
# format_data <- function(dataFrame){
#   # 1 Measure 2 Year 3 Location 4 Sex 5 Age 6 Cause 7 Metric 8 Value 9 Upper 10 Lower
#   dataFrame <- mutate(dataFrame, 
#                       V1 = as.numeric(V1),
#                       V2 = as.numeric(V2),
#                       V3 = as.numeric(V3),
#                       V4 = as.numeric(V4),
#                       V5 = as.numeric(V5),
#                       V6 = as.numeric(V6),
#                       V7 = as.numeric(V7),
#                       V8 = as.numeric(V8),
#                       V9 = as.numeric(V9),
#                       V10 = as.numeric(V10))
#   # colnames(dataFrame) <- c("measure_id", "year", "location_id", "sex_id", "age_id",
#   #                          "cause_id", "metric_id", "val", "upper", "lower")  ####################### MAX MIN WERE SWITCHED
#   return(dataFrame) 
# }
# 

# Other exploration-----------------------------------------------------------------------

# url <- paste0(api_root, subset, "age_group_id=", age, "&location_id=", location, "&measure_id=", measure,
#               "&metric_id=", metric, "&sex_id=", sex, "&year_id=", year, "&cause_id=", cause, "&", key_text)
# A <- rbind(value_data, as.data.frame(jsonlite::fromJSON(url)$data))
# A <- A$data
# A <- as.data.frame(A)
# 
# 
# causeURL  <- paste0(api_root,"metadata/measure/?age_group_id=22&measure_id=2?","&",key_text)
# 
# print(causeURL)
