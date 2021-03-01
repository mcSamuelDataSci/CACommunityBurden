library(RColorBrewer)
library(readxl)

topLev              <- c("Communicable","Cancer","Cardiovascular","Other Chronic","Injury","Ill-Defined","Birth","Other")
topLevColors        <- brewer.pal(n = length(topLev), name = "Dark2") # Determine pallete; Dark2 allows max 8
names(topLevColors) <- topLev


#FROM plot life tables function
raceColors1        <- c("seashell4", "chocolate1", "firebrick", "royalBlue1", "darkblue", "navajowhite3", "red",   "chartreuse4")
raceNames1         <- c("AIAN_NH",   "ASIAN_NH",   "BLACK_NH",  "HISPANIC",   "MR_NH",    "NHPI_NH",      "TOTAL", "WHITE_NH")
names(raceColors1) <- raceNames1
totalColor         <- "red"



linkRace <- as.data.frame(read_excel("/mnt/projects/FusionData/Standards/raceLink.xlsx"))
raceNames         <- linkRace$raceName 
raceColors        <- brewer.pal(length(raceNames), "Paired")
names(raceColors) <- raceNames














genderNames         <- c("Female","Male")
genderColors          <- c("gray","lightblue")
names(genderColors) <- genderNames


# myTextSize2 <- 15
# myTitleSize <- 20
# 
# color1 <- "blue"
# color2 <- "dodgerblue"
# color3 <- "yellow"
# color4 <- "black"

# cdph1 <- theme_bw() +
#   theme(  plot.title   = element_text(size = myTitleSize, color=color1),
#           strip.text.y = element_text(size = myTextSize2, face="bold", angle = 0),
#           strip.text.x = element_text(size = myTextSize2, face="bold", angle = 0),
#           axis.title   = element_text(size = myTextSize2, face="bold"),
#           axis.text.y  = element_text(size = myTextSize2),
#           axis.text.x  = element_text(size = myTextSize2)   )  
# 
# theme_set(cdph1)