
# mySuppress <- function(myDat,gBy,NcolName,critN=11)  {
#  
#   myDat$myN <- myDat[,NcolName]
#   
#   myDat <- myDat  %>%   
#            mutate(suppress1   = ifelse(myN < critN,1,0) # supress if N < critN
#            ) %>%   
#            group_by(.dots = gBy) %>%
#              mutate(rowsSuppressedA  = sum(suppress1==1,na.rm=TRUE) ,
#                      nextSmallestN   = min(myN[myN >= critN],na.rm=TRUE),  # find next smallest N
#                      suppress2       = ifelse((rowsSuppressedA== 1 & myN==nextSmallestN),1,0), # suppress complementary row if only one row suppressed
#                      SUPPRESS        = ifelse(suppress1==1 | suppress2==1,1,0) # final suppression indicator
#            ) 
#            
#   myDat$SUPPRESS       
#  
# }

mySuppress <- function(myDat, grouping, NcolName, critN=11)  {

  myDat <- myDat  %>%
    rename(myN = {{NcolName}}) %>%
    mutate(suppress1   = ifelse(myN < critN,1,0) ) %>% # suppress if N < critN
    group_by(across( {{grouping }} )) %>%
    mutate(rowsSuppressedA  = sum(suppress1==1, na.rm=TRUE) ,
           nextSmallestN   = min(myN[myN >= critN], na.rm=TRUE),  # find next smallest N
           suppress2       = ifelse((rowsSuppressedA == 1 & myN == nextSmallestN),1,0), # suppress complementary row
           #  if only one row suppressed
           SUPPRESS        = ifelse(suppress1==1 | suppress2==1,1,0) # final suppression indicator
    )

  return(myDat$SUPPRESS)

}

# Address sex specific cancers ===================
# Added an ifelse statement in "nextSmallestN" to avoid dplyr warning (Inf)
# mySuppress <- function(myDat, grouping, NcolName, critN=11)  {
#   
#   myDat <- myDat  %>%
#     rename(myN = {{NcolName}}) %>% 
#     mutate(suppress1   = ifelse(myN < critN,1,0) ) %>% # suppress if N < critN
#     group_by(across( {{grouping }} )) %>%
#     mutate(rowsSuppressedA  = sum(suppress1==1, na.rm=TRUE) ,
#            nextSmallestN   = if ( all(causeCode %in% c(sexCauseList$Female, sexCauseList$Male)) & !("sex" %in% grouping) ) -1 else min(myN[myN >= critN], na.rm=TRUE),  # find next smallest N
#            suppress2       = ifelse((rowsSuppressedA == 1 & myN == nextSmallestN),1,0), # suppress complementary row
#            #  if only one row suppressed
#            SUPPRESS        = ifelse(suppress1==1 | suppress2==1,1,0) # final suppression indicator
#     ) 
#   
#   return(myDat$SUPPRESS)       
#   
# }


# Comments Etc ================================================================

if (1 == 2) {

 library(dplyr)

 inDat           <- read.csv(paste0(ccbUpstream,"/upstreamInfo/suppressionTestData.csv"))
 gBy             <-  c("Year","Place")
 inDatNew        <- mutate(inDat, supIndictor = mySuppress(inDat,gBy,"events"))
 inDatSupprssed  <- filter(inDatNew,supIndictor != 1)


# for use if experimenting outside of function
  myDat <- inDat %>% mutate(myN = events)
  critN <- 11
  
  # Second test
  inDat <- data.frame(sex = c("Female", "Total", "Female", "Male", "Total"), 
                      causeCode = c("B09", "B09", "D10", "D10", "D10"), 
                      Ndeaths = c(10, 10, 50, 5, 55), 
                      stringsAsFactors = F)
  gBy <- c("causeCode")
  inDatNew <- mutate(inDat, supIndicator = mySuppress(inDat, gBy, "Ndeaths"))
  inDatSupprssed <- filter(inDatNew, supIndicator != 1)
  
  # Third test
  inDat <- data.frame(sex = c("Female", "Total", "Female", "Male", "Total"), 
                      causeCode = c("B09", "B09", "D10", "D10", "D10"), 
                      Ndeaths = c(12, 12, 50, 5, 55), 
                      stringsAsFactors = F)
  gBy <- c("causeCode")
  inDatNew <- mutate(inDat, supIndicator = mySuppress(inDat, gBy, "Ndeaths"))
  inDatSupprssed <- filter(inDatNew, supIndicator != 1)
  
  # Third test
  races <- data.frame(race = c("Asian", "Black", "Total"), stringsAsFactors = F)
  sexes <- data.frame(sex = c("Female", "Male", "Total"), stringsAsFactors = F)
  causes <- data.frame(causeCode = c("B09", "D10"), stringsAsFactors = F)
  inDat <- sqldf(" select * from  races cross join sexes cross join causes") %>% 
    filter(!(causeCode == "B09" & sex == "Male")) %>% 
    arrange(causeCode, sex, race) %>% 
    mutate(Ndeaths = c(8, 22, 30, 8, 22, 30, 
                       4, 11, 15, 4, 11, 15, 8, 22, 30))
  
  gBy <- c("causeCode", "sex")
  inDatNew <- mutate(inDat, supIndicator = mySuppress(inDat, gBy, "Ndeaths"))
  inDatSupprssed <- filter(inDatNew, supIndicator != 1)
   }


# All related to unsucessful work to get varible as input
# library(lazyeval)
# N.column <- enquo(N.column)
# N.column <- as.name(N.column)
# junk <- "events"
