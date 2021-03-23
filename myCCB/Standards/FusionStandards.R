# server <- F
# if (!server) source("g:/FusionData/Standards/FusionStandards.R")
# if (server) source("/mnt/projects/FusionData/Standards/FusionStandards.R")



#.... source this file....  in auto....
library(readxl)
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(fs)
library(scales)
library(RColorBrewer)
library(tidyr)
# library(summarytools)

# 
# 
# FusionData
# 
#  0.CCB
#  
#   myCBD to myCCB
#  
#  0.CCB.resources   



#server <- TRUE

# if (server) highPath <-  "/mnt/projects/"
# if(!server) highPath <-  "G:/"

# securePlace    <-  path(highPath,"FusionData/0.Secure.Data/")  

# fusionPlace    <- path(highPath,"FusionData/")
# standardsPlace <- path("/Standards")
# sdohPlace      <- path(highPath,"FusionData/SDOH/")

# ccbPlace       <-  path(highPath,"FusionData/0.CCB/") 
# ccbData        <-  path(highPath,"FusionData/0.CCB/myCCB/myData/")  
# ccbInfo        <-  path("/myInfo")  
# ccbFunctions   <-  path(highPath,"FusionData/0.CCB/myCCB/myFunctions/")  
# ccbUpstream    <-  path(highPath,"FusionData/0.CCB/myUpstream/")  



if (CCB) {
standardsPlace <-  path(myPlace,"/Standards")
ccbInfo        <-  path(myPlace,"/myInfo")
ccbData        <-  path(myPlace,"/myData")
ccbFunctions   <-  path(myPlace,"/myFunctions/")
}
  




# NOTE: "LABEL" was changed to "causeCode" 12/2020
# changed "BG" to "topLev"

# ccs_code_to_name_link.csv  is going away, 
# incudes ccs_diagP	and ccsName variables
# should be replaced  with files below and ccsCode and ccsName  OR whatever we call thouse

raceLink    <-  read_excel(path(standardsPlace,"/raceLink.xlsx"))
ageLink     <-  read_excel(path(standardsPlace,"/ageLink.xlsx"),sheet = "standard")


## COLORS==========================================================

# ADA Compliant colors - http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette

# The palette with grey:
greyPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# The palette with black:
blackPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Top lev colors
topLev              <- c("Communicable","Cancer","Cardiovascular","Other Chronic","Injury","Ill-Defined","Perinatal","Other")
# topLevColors        <- brewer.pal(n = length(topLev), name = "Dark2") # Determine pallete; Dark2 allows max 8
topLevColors        <- greyPalette[1:length(topLev)]
names(topLevColors) <- topLev

# Toplev associated text colors (white or black) - for text in bars
topLevTextColors <- c("#FFFFFF", "#000000","#000000", "#000000", "#000000", "#000000", "#000000", "#000000")
names(topLevTextColors) <- topLev




#FROM plot life tables function

raceColors1        <- c("seashell4", "chocolate1", "firebrick", "royalBlue1", "darkblue", "navajowhite3", "red",   "chartreuse4")
raceNames1         <- c("AIAN_NH",   "ASIAN_NH",   "BLACK_NH",  "HISPANIC",   "MR_NH",    "NHPI_NH",      "TOTAL", "WHITE_NH")
names(raceColors1) <- raceNames1
totalColor         <- "red"

raceNames         <- raceLink$raceName 
raceColors        <- brewer.pal(length(raceNames), "Paired")
names(raceColors) <- raceNames

raceList <- raceLink[-8:-10,"raceNameShort"] %>% unlist() %>% unname()




# Race Codes
raceCodes <- c("AIAN", "Asian", "Black", "Hisp", "Multi", "NHPI", "Other", "White", "Total")
raceCodesColors <- c(greyPalette[1:(length(raceCodes) - 1)], "red")
# raceCodesColors <- c(greyPalette[1:length(raceCodes)], "red")
# raceCodesColors <- c("red", "yellow", "black", "brown", "blue", "lightblue", "darkblue", "gray","purple")

names(raceCodesColors) <- raceCodes
totalColor         <- "red"

# Race Names
raceNames <- c("American Indian or Alaska Native", 
               "Asian", 
               "Black",
               "Latino",
               "Multi-Race",
               "Native Hawaiian and other Pacific Islander", 
               "Other",
               "White", 
               "Total")
raceNamesColors <- c(greyPalette[1:length(raceNames)], "red")
names(raceNamesColors) <- raceNames

# Race Name Short
raceNamesShort <- c("AI/AN", 
               "Asian", 
               "Black",
               "Latino",
               "Multi-Race",
               "NH/PI", 
               "Other",
               "White", 
               "Total")
raceNamesShortColors <- c(greyPalette[1:length(raceNamesShort)], "red")
names(raceNamesShortColors) <- raceNamesShort



genderNames         <- c("Female","Male", "Total")
# genderColors          <- c("gray","lightblue")
genderColors <- c("#FF706E", "#00BEC6", "#999999")
names(genderColors) <- genderNames



#===========================================================================


commInfo <- read.csv(paste0(ccbInfo, "/comName.csv"), header = T)










# consider adding "Level"
# deathCauseLink
# deathCauseNamesLink
# death_Cause_Names

# fullCauseList   <-    read_excel( path(ccbPath,"myInfo/icd10_to_CAUSE.xlsx"), sheet="main") %>%
#                       filter(!is.na(causeList)) %>%
#                       mutate(causeNameShort = ifelse(is.na(causeNameShort), causeName, causeNameShort),
#                              topLevName = ifelse(topLev == "0","All Causes",
#                                           ifelse(topLev == "A","Communicable",
#                                           ifelse(topLev == "B","Cancer",
#                                           ifelse(topLev == "C","Cardiovascular",
#                                           ifelse(topLev == "D","Other Chronic",
#                                           ifelse(topLev == "E","Injury",
#                                                            "Ill-Defined"))))))   ) %>%
#                       select(causeCode, causeName, causeNameShort, causeList, topLev, topLevName) %>%
#                       arrange(causeCode)


deathCauseLink   <-
  read_excel(path(ccbInfo,"icd10_to_CAUSE.xlsx"), sheet="main") %>%
   filter(!is.na(causeList)) %>%
   mutate(causeNameShort = ifelse(is.na(causeNameShort), causeName, causeNameShort),
          topLevName     = case_when(topLevCode  == "0" ~ "All Causes",
                                     topLevCode  == "A" ~ "Communicable",
                                     topLevCode  == "B" ~ "Cancer",
                                     topLevCode  == "C" ~ "Cardiovascular",
                                     topLevCode  == "D" ~ "Other Chronic",
                                     topLevCode  == "E" ~ "Injury", 
                                    TRUE ~ "Ill-Defined")) %>%
  select(causeCode, causeName, causeNameShort, causeList, topLevCode, topLevName) %>%
  arrange(causeCode) %>%
  as.data.frame()



# not needed if sourced.....
# write.csv(deathCauseLink, path(standardsPlace,"/deathCauseLink.csv"))



hospCauseLink  <-    read_excel(path(ccbInfo,"CCS Code and Names Linkage.xlsx")) %>%
                      mutate(causeCode = str_pad(ccsCode, 5,"left",pad="o")) %>%
                      mutate(causeNameShort = ifelse(is.na(ccsNameShort),ccsName,ccsNameShort)) %>%
                      select(causeCode, causeName = ccsName, causeNameShort, topLevName, birth) %>%
                      as.data.frame()


# ggplot standards ---------------------

myTitleSize <- 20
myLegendSize <- 20


myTextSize <- 18
myAxisTextSize <- myTextSize2 <- 16
myAxisSize  <- myAxisTitleSize <- myTextSize3 <- 20

myWrapNumber <- 70
myTitleColor <- "darkblue"

myCex1           <- 2  # 1.5  #line labels
myCex2           <- 1.2  #currently used only in education trend
myLineLabelSpace <- 0.3

#myLineLabelSize <- 26 - deleted, not used

myLineLabelCex <- 2



myLineSize  <- 2
myPointSize <- 5 # line markers
myPointShape <- 18

myTheme <- theme_bw() +
  theme(plot.title   = element_text(size = myTitleSize, color=myTitleColor, face = 'bold'),
        strip.text.y = element_text(size = myTextSize2, face="bold", angle = 0),
        strip.text.x = element_text(size = myTextSize2, face="bold", angle = 0),
        axis.title   = element_text(size = myAxisTitleSize, face="bold"), # was myTextSize2, changed to myAxisSize
        axis.text.y  = element_text(size = myAxisTextSize),
        axis.text.x  = element_text(size = myAxisTextSize), 
        legend.text = element_text(size = myLegendSize), 
        legend.title = element_text(size = myLegendSize)
        #axis.text.x  = element_text(size = 10,          face="bold", angle = 40, hjust = 1),
  )

theme_set(myTheme)


# Highcharter standards + theme

hc_myTitleSize <- '20px'
hc_myAxisTitleSize <- '18px'

# myHCTheme <- hc_theme(
#   title = list(
#     style = list(
#       color = 'darkblue',
#       fontSize = '20px'
#     )
#   )
# )




# color standards -------------------------------


# ADA Compliant colors - http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette











