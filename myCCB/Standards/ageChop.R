require(readxl)

#mainPath <- 

ageChop <- function(INAGE, mSheet="age3", myCuts=FALSE, my_lAge, my_uAge, my_ageName, ourServer = T) {
  
  if (ourServer) {
    ageLink <- read_excel("/mnt/projects/FusionData/Standards/ageLink.xlsx",sheet=mSheet)
  } else {
    ageLink <- read_excel("G:/FusionData/Standards/ageLink.xlsx",sheet=mSheet)
  }
 
# ageLink <- read_excel("G:/CCB/Standards/ageLink.xlsx",sheet=mSheet)

if(!myCuts) {
aL       <-      ageLink$lAge   
aU       <- c(-1,ageLink$uAge)  
aLabs    <- ageLink$ageName
}

if(myCuts) {
  
  aL       <-      my_lAge   
  aU       <- append(-1,my_uAge)  
  aLabs    <- my_ageName
  
  
}

aMark    <- findInterval(INAGE,aU,left.open = TRUE)  
OUTAGE   <- aLabs[aMark]                                   
OUTAGE

}



# myCuts argument - FALSE if using ageGroup cutoffs from ageLink file; 
#                   TRUE if specifying your own cut offs (specify in my_lAge, my_uAge, & my_ageName)

# test <- ageChop(INAGE = c(1:30), 
#                  mSheet = "age3", 
#                  myCuts = TRUE, 
#                  my_lAge = c(0, 5, 18, 65), 
#                  my_uAge = c(4, 17, 64, 120),
#                  my_ageName = c("<5", "5-17", "18-64", "65+"))




