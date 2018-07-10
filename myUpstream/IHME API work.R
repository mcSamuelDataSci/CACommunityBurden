
# MCS IHME API KEY
ihmeKey <- "5a4fc200e1af720001c84cf91e34303eca334ffa8a35722aac008232"
keyText <- paste0("authorization=",ihmeKey)
APIroot <- "https://api.healthdata.org/healthdata/v1/"

# "Cause set" metadata endpoint 
causeURL0 <- paste0(APIroot,"metadata/cause_set","/?",keyText)

# "Cause" metadata endpoint (default "GBD Reporting" cause set ID explicitly set):
#  One can recreate our cause hierarchy by examining this endpoint's response's "cause_id" and "parent_id" values. "All causes" (cause_id 294) is the root cause.
causeURL  <- paste0(APIroot,"metadata/cause/?cause_set_id=3","&",keyText)




#GBD 2016 etiology endpoint (with authorization key) (all ages, California, deaths, number, both sexes, 2016, lower respiratory infections, influenza)

subSet     <- "data/gbd/etiology/?age_group_id=22&location_id=527&measure_id=1&metric_id=1&sex_id=3&year_id=2016&cause_id=322&etiology_id=187"
gbd2016URL <- paste0(APIroot,subSet,"&",keyText)
t0         <- jsonlite::fromJSON(gbd2016URL)

#GBD 2016 etiology endpoint (with authorization key) (all ages, California, deaths, number, both sexes, 2016, lower respiratory infections, influenza) (human-readable)
gbd2016ReadableURL <- "https://api.healthdata.org/healthdata/v1/data/gbd/etiology/?age_group_id=22&location_id=527&measure_id=1&metric_id=1&sex_id=3&year_id=2016&cause_id=322&etiology_id=187&readable=true&object=true&authorization=5a4fc200e1af720001c84cf91e34303eca334ffa8a35722aac008232"
gbd2016ReadableURL <- "https://api.healthdata.org/healthdata/v1/data/gbd/etiology/?age_group_id=22&location_id=527&measure_id=1&metric_id=1&sex_id=3&year_id=2016&readable=true&object=true&authorization=5a4fc200e1af720001c84cf91e34303eca334ffa8a35722aac008232"


library(dplyr)

# library(httr)
# t0L  <- GET(URL1)

options(stringsAsFactors=FALSE)

library(jsonlite)
tX <- jsonlite::fromJSON(causeURL)
tY <- tX$data
tZ <- as.data.frame(tY)
names(tZ) <- tX$meta$fields



#why doesn't the meta data "tell" these to be numeric?
tZ <- mutate(tZ,
                cause_id   = as.numeric(cause_id), 
                sort_order = as.numeric(sort_order),
                level      = as.numeric(level),
                parent_id  = as.numeric(parent_id)
)



foodMarketsRaw<-jsonlite::fromJSON("https://data.ny.gov/api/views/9a8c-vfzj/rows.json?accessType=DOWNLOAD")
f2 <- as.data.frame(foodMarketsRaw$data)
#nope....


t0 <- jsonlite::fromJSON(gbd2016ReadableURL)
t00 <- t0$data
t000 <- as.data.frame(t00)
names(t000) <- t0$meta$fields



library(RJSONIO)
t0  <- RJSONIO::fromJSON(URL1)
t2  <- t0[['data']]




t2[[5]]
t2[5]
t2[[5]][[2]]
t2[5][2]  #--> NULL

# pretty good:
t3  <- t(as.data.frame(t2))
t3 <- unlist(t3)
row.names(t3) <- NULL
names(t3) <- t0$meta$fields


t4.1 <- sapply(t2, function(x) x[[1]])
t4.2 <- sapply(t2, function(x) x[[2]])
t4.3 <- sapply(t2, function(x) x[[3]])

t5 <- cbind(t4.1,t4.2,t4.3)




# http://zevross.com/blog/2015/02/12/using-r-to-download-and-parse-json-an-example-using-data-from-an-open-data-portal/
# -------------------------------------------------  
library(gdata) # for the trim function
grabInfo<-function(var){
  print(paste("Variable", var, sep=" "))  
  sapply(t2, function(x) returnData(x, var)) 
}

returnData<-function(x, var){
  if(!is.null( x[[var]])){
    return( trim(x[[var]]))
  }else{
    return(NA)
  }
}

#---------------------------------------------------


t6 <-data.frame(sapply(1:6, grabInfo), stringsAsFactors=FALSE)

names(t6) <- t0$meta$fields






t0L$headers
t0L$headers$`content-type`
t2  <- t0L[['data']]  #--> NULL


temp <- stream_in(file(junk),flatten=TRUE)



tX <- flatten(t0L)

xdata <- content(get.data)
names(xdata)
get.data$status_code
names(get.data)


x1data <- content(get.data, as="text")

xxxdata <- x1data %>% fromJSON



xxdata <- xdata$data
names(xxdata)




library(tidyverse)
morex <- unlist(xdata)
