# MCS IHME API KEY
ihmeKey <- "5a4fc200e1af720001c84cf91e34303eca334ffa8a35722aac008232"
keyText <- paste0("authorization=",ihmeKey)
APIroot <- "https://api.healthdata.org/healthdata/v1/"

# "Cause set" metadata endpoint 
#causeURL0 <- paste0(APIroot,"metadata/cause_set","/?",keyText)

# "Cause" metadata endpoint (default "GBD Reporting" cause set ID explicitly set):
#  One can recreate our cause hierarchy by examining this endpoint's response's "cause_id" and "parent_id" values. "All causes" (cause_id 294) is the root cause.
#causeURL  <- paste0(APIroot,"metadata/cause/?cause_set_id=3","&",keyText)
#causeURL  <- paste0(APIroot,"metadata/measure/?age_group_id=22&measure_id=2?","&",keyText)

### Start
library(dplyr)
options(stringsAsFactors=FALSE)
library(jsonlite)
library(treemap)
library(d3treeR)
options(max.print = 50)


# (All ages, California, All Measures, All metrics, All sex id's, 2001 through 2016, All causes)
age <- 22 # All ages
location <- 527 # California
measure <- paste(as.character(c(1:4)), collapse = ",") # 1 Deaths, 2 DALYs, 3 YLDs, 4 YLLs
metric <- paste(as.character(c(1:3)), collapse = ",") # 1 Number, 2 Percent, 3 Rate
sex <- paste(as.character(c(1:3)), collapse = ",") # 1 Male, 2 Female, 3 Both
year <- paste(as.character(c(2001:2016)), collapse = ",") # 2001 through 2016
causea <- paste(as.character(c(294:458)), collapse = ",") # Can do up to 294:489, but 294:490 (and above 490) doesn't work. Up to 458 with other variables set
subSet     <- "data/gbd/cause/?age_group_id=22&location_id=527&"

gbdURLa <- paste0(APIroot, subSet, "age_group_id=", age, "&location_id=", location, "&measure_id=", measure, "&metric_id=", metric, "&sex_id=", sex, "&year_id=", year, "&cause_id=", causea,"&", keyText)
A <- jsonlite::fromJSON(gbdURLa)
A <- A$data
A <- as.data.frame(A)
# names(A) <- A$meta$fields

causeb <- paste(as.character(c(459:623)), collapse = ",")
gbdURLb <- paste0(APIroot, subSet, "age_group_id=", age, "&location_id=", location, "&measure_id=", measure, "&metric_id=", metric, "&sex_id=", sex, "&year_id=", year, "&cause_id=", causeb,"&", keyText)
B <- jsonlite::fromJSON(gbdURLb)
B <- B$data
B <- as.data.frame(B)

causec <- paste(as.character(c(624:788)), collapse = ",")
gbdURLc <- paste0(APIroot, subSet, "age_group_id=", age, "&location_id=", location, "&measure_id=", measure, "&metric_id=", metric, "&sex_id=", sex, "&year_id=", year, "&cause_id=", causec,"&", keyText)
C <- jsonlite::fromJSON(gbdURLc)
C <- C$data
C <- as.data.frame(C)

caused <- paste(as.character(c(789:953)), collapse = ",")
gbdURLd <- paste0(APIroot, subSet, "age_group_id=", age, "&location_id=", location, "&measure_id=", measure, "&metric_id=", metric, "&sex_id=", sex, "&year_id=", year, "&cause_id=", caused,"&", keyText)
D <- jsonlite::fromJSON(gbdURLd)
D <- D$data
D <- as.data.frame(D)

# 1 Measure 2 Year 3 Location 4 Sex 5 Age 6 Cause 7 Metric 8 Value 9 Minimum 10 Maximum
tA <- rbind(A, B, C, D)
#write.csv(tA, file = "tA.csv")
#data <- read.csv("/Users/jonahgolden/CCB Stuff/src/tA.csv", header=TRUE)
tA <- mutate(tA, 
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
colnames(tA) <- c("Measure", "Year", "Location", "Sex", "Age", "Cause", "Metric", "Value", "Minimum", "Maximum")

##### ggplot2
library(ggplot2)
df = data.frame(year=tA$Year %in% 2002, value=tA$Value)



qplot(tA$V9, geom = "histogram")

## treemap
data_selected_region <- tA[tA$V1 %in% "2" &
                           tA$V2 %in% "2016" &
                           tA$V4 %in% "3" &
                           tA$V7 %in% "1"]
twentysixteen <- tA[V2 %in% 2016]

d3tree2(treemap(A,
        index = "V8",
        vSize = "V8",
        type = "value",
        palette = "Spectral"))



## Shiny
library(shiny)

ui <- fluidPage(
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    # Inputs: Select variables to plot
    sidebarPanel(
      
      # Select variable for sex
      selectInput("V4",
                  label = h4("Sex:"),
                  choices = c("1", "2", "3"),
                  selected = "3")
    ),
    
    # Output: Show tree
    mainPanel(
      d3tree2Output(outputId = "treemap", width = "100%", height = "600px")
    )
  )
)
# Define server function
server <- function(input, output) {
  
  #Create the treemap object the plotOutput function is expecting
  output$treemap <- renderD3tree2({
    d3tree2(treemap(tA,
                    index = "V8",
                    vSize = "V8",
                    vColor = "V8",
                    type = "value",
                    palette = "Spectral",
                    border.col = c("grey70", "grey90"),
                    fontsize.title = 18,
                    algorithm = "pivotSize"
            ))
  })
}

# Create the Shiny app object
shinyApp(ui = ui, server = server)

###########################
###########################
###########################


# causeURL  <- paste0(APIroot,"data/gbd/cause/?measure_id=1&age_group_id=22&location_id=102&year_id=1990&cause_id=298&sex_id=3&metric_id=1","&",keyText)

tX <- jsonlite::fromJSON(causeURL)
tX
tY <- tX$data
tZ <- as.data.frame(tY)
names(tZ) <- tX$meta$fields

tZ

#why doesn't the meta data "tell" these to be numeric?
tZ <- mutate(tZ,
             cause_id   = as.numeric(cause_id), 
             sort_order = as.numeric(sort_order),
             level      = as.numeric(level),
             parent_id  = as.numeric(parent_id)
)






### end 

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
