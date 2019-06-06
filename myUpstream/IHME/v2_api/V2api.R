library(dplyr)
library(jsonlite)
library(docstring)
options(stringsAsFactors=FALSE)
options(max.print = 50)

# IHME API key, v2 root, and subsets-----------------------------------------------------------------------

ihme_key <- "5a4fc200e1af720001c84cf91e34303eca334ffa8a35722aac008232"
key_text <- paste0("authorization=",ihme_key)
v1_api_root <- "https://api.healthdata.org/healthdata/v1/"
v2_api_root <- "https://api.healthdata.org/healthdata/v2/"

cause_subset2017 <- "data/gbd/2017/cause/single/?"
cause_subset2016 <- "data/gbd/2016/cause/single/?"

risk_subset2017 <- "data/gbd/2017/risk/single/?"
risk_subset2016 <- "data/gbd/2016/risk/single/?"

cause_meta_subset <- "metadata/cause/?cause_set_id=3"
risk_meta_subset <- "metadata/risk/?risk_set_id=1"

# “2017” (for GBD rounds) and “single/multi” (for single-year data vs. cross-year data – version 2’s bigger than version 1)

# Define functions-----------------------------------------------------------------------

# Get Data functions-------------------

make_url <- function(subset, measure_id, year_id="", location_id=527, sex_id, age_group_id=22, metric_id, risk_id="", cause_id="") {
  #' Make URL for cause endpoint. Two degrees of freedom for every endpoint means that you can 
  #' leave up to two dimensions unspecified and you will get all data from them.
  return(paste0(v2_api_root, subset,
                "&measure_id=", measure_id,
                "&year_id=", year_id,
                "&location_id=", location_id,
                "&sex_id=", sex_id,
                "&age_group_id=", age_group_id,
                "&metric_id=", metric_id,
                "&risk_id=", risk_id,  # Only specify risk_id if using a risk_subset
                "&cause_id=", cause_id,
                "&", key_text))
}

make_data_subset <- function(URL){
  #' Inputs: URL, Optional: measure_id, cause_id1, cause_id2
  #' Output: data frame of json data call

  data <- jsonlite::fromJSON(paste0(URL))
  colnames(data$data) <- data$meta$fields
  return(as.data.frame(data$data))
}

# Format Data functions-------------------

parent_recursive <- function(row, df) {
  if (row[,'level'] == 0) {
    return('0')
  } else if (row[,'level'] == 1) {
    return(row[,'id_name'])
  } else {
    parent_recursive(df[which(df[,'id_num'] == row[,'parent_id']), ], df)
  }
}

merge_with_parent <- function(value_data, meta_subset){
  #'Input: value_data as created by make_data_subset function
  #'Output: data frame of value_data, now including parent data
  #'Parent Data Fields: cause_id, cause_name, acause, sort_order, level, parent_id
  #'cause_set_id is basically level?

  parent  <- jsonlite::fromJSON(paste0(v2_api_root, meta_subset,"&",key_text))
  colnames(parent$data) <- parent$meta$fields
  parent <- as.data.frame(parent$data) %>%
    rename('id_num' = 1, 'id_name' = 2, 'display' = 3)
  
  if (meta_subset == risk_meta_subset) {
    parent[2,'id_name'] <- 'Environmental/ occupational risks'
  }
  
  for (row in 1:nrow(parent)) {  # Use apply() here?
    parent[row, 'first_parent'] <- parent_recursive(parent[row,], parent)
  }
  
  parent <- parent %>%
    mutate(level = ifelse(id_num %in% setdiff(id_num, parent_id) & level == 2, paste(2,3,4, sep =","),
                          ifelse(id_num %in% setdiff(id_num, parent_id) & level == 3, paste(3,4, sep=","), level)))
  
  return(merge(value_data, parent, by='id_num'))
}

make_numeric <- function(df) {
  cols.num <- c(1:10, 13, 15)
  df[cols.num] <- sapply(df[cols.num], as.numeric)
  df[df$metric_id == 2, 8:10] <- df[df$metric_id == 2, 8:10]*100
  df[,8:10] <- round(df[,8:10],4)  # Is 4 the right number of decimal places?
  return(df)
}


# Load and format data-----------------------------------------------------------------------
# make_url() defaults: subset=cause_subset2017, measure_id="", year_id="", location_id=527,
#                      sex_id="", age_group_id=22, metric_id="", risk_id="", cause_id=""

get_data <- function(cause) {
  start_time <- Sys.time()
  big_data <- data.frame()
  for (m in 1:4) {
    for (s in 1:3) {
      for (n in 1:3) {
        if (cause) {
          url <- make_url(subset = cause_subset2017, measure_id = m, sex_id = s, metric_id = n)
        } else {
          url <- make_url(subset = risk_subset2017, measure_id = m, sex_id = s, metric_id = n, cause_id = 294)
        }
        little_data <- make_data_subset(url)
        big_data <- rbind(big_data, little_data)
      }
    }
  }
  rm(little_data)
  total_time <- Sys.time() - start_time
  print(total_time)
  return(big_data)
}

causeData <- get_data(cause = TRUE)
riskData <- get_data(cause = FALSE)

# Risk
v2risk_data <- riskData %>%
  rename('id_num' = 'risk_id') %>% # Could change to id_name = risk_short_name
  merge_with_parent(value_data = ., meta_subset = risk_meta_subset) %>%
  mutate(display = 'risk') %>%
  select(-cause_id) %>%
  make_numeric()


# Cause
v2cause_data <- causeData %>%
  rename('id_num' = 'cause_id') %>%
  merge_with_parent(value_data = ., meta_subset = cause_meta_subset) %>%
  mutate(display = 'cause') %>%
  make_numeric()

output <- bind_rows(v2cause_data, v2risk_data)

# saveRDS(output, file = "v2data.RDS")

myDrive <- getwd()  # Root location of CBD project
myPlace <- paste0(myDrive,"/myCBD")
saveRDS(output_data, file = path(myPlace,"/myData/v2IHME.rds"))
# 
# # See new cause ids-----------------------------------------------------------------------
# 
# parent  <- jsonlite::fromJSON(paste0(v1_api_root, v1_risk_meta_subset,"&",key_text))
# colnames(parent$data) <- parent$meta$fields
# meta <- as.data.frame(parent$data)
# 
# url <- make_url(subset = cause_subset2017, measure_id = 1, sex_id = 3, metric_id = 1, year_id = 2017)
# df <- make_data_subset(url)
# 
# old_cause_ids <- setdiff(meta3$cause_id, v2cause$cause_id)
# new_cause_ids <- setdiff(v2cause$cause_id, meta3$cause_id)
# 
# # visualize smoking data-----------------------------------------------------------------------
# 
# url2 <- make_risk_url(subset = risk_subset2017, risk_id = 99, age_id = 22, metric_id = 1, measure_id = 1, sex_id = 3)
# smoking_2017_risk <- make_data_subset(url2)
# 
# url3 <- make_risk_url(subset = risk_subset2016, risk_id = 99, age_id = 22, metric_id = 1, measure_id = 1, sex_id = 3)
# smoking_2016_risk <- make_data_subset(url3)
# 
# 
# plot(smoking_2016_risk$year_id, smoking_2016_risk$val, type = "b", col = "blue",
#      xlim = c(1990, 2018), ylim=c(30000, 60000),
#      main = "Smoking Risk", xlab = "Number Deaths", ylab = "year")
# lines(smoking_2017_risk$year_id, smoking_2017_risk$val, type = "p", col = "red")
# legend("topright", legend=c("2016 Data", "2017 Data"),
#        col = c("blue", "red"), lty = 1)



