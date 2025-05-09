# server <- F
# if (!server) source("g:/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")
# if (server) source("/mnt/projects/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")

library(pacman)

#.... source this file....  in auto....

# p_load(
#   readxl,
#   readr,
#   dplyr,
#   stringr,
#   ggplot2,
#   fs,
#   scales , 
#   tidyr)
#RColorBrewer)
# library(summarytools)




library(readxl)
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(fs)
library(scales)
#library(RColorBrewer)
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



# securePlace    <-  path(highPath,"FusionData/0.Secure.Data/")  

# standardsPlace <- path("/Standards")
# sdohPlace      <- path(highPath,"FusionData/SDOH/")

# ccbPlace       <-  path(highPath,"FusionData/0.CCB/") 
# ccbData        <-  path(highPath,"FusionData/0.CCB/myCCB/myData/")  
# ccbInfo        <-  path("/myInfo")  
# ccbFunctions   <-  path(highPath,"FusionData/0.CCB/myCCB/myFunctions/")  
# ccbUpstream    <-  path(highPath,"FusionData/0.CCB/myUpstream/")  

if(!exists("CCB"))  CCB     <- FALSE
if (!CCB & !server) myPlace <- "g:/FusionData/0.CCB/myCCB"
if (!CCB & server)  myPlace <- "/mnt/projects/FusionData/0.CCB/myCCB"

# if (CCB) {
standardsPlace <-  paste0(myPlace,"/Standards/")
ccbInfo        <-  paste0(myPlace,"/myInfo/")
ccbData        <-  paste0(myPlace,"/myData/")
ccbFunctions   <-  paste0(myPlace,"/myFunctions/")
# }
  



if (server) highPath <-  "/mnt/projects/"
if(!server) highPath <-  "G:/"
securePlace    <-  paste0(highPath,"FusionData/0.Secure.Data/")  
fusionPlace    <- paste0(highPath,"FusionData/")
ccbUpstream    <-  paste0(highPath,"FusionData/0.CCB/myUpstream/")  



#Jaspreet, I (Michael) moved this here from "F3" as a placeholder if we want to use it more broadly.  Used only in F3 for now...
datPath <- "real/archive_2019Datasets/"
datPath <- "real/"



#myYearG3  <- "2017-2019" 
STATE       <- "CALIFORNIA"

# True if app should include 2020 data; False to exclude
# SET TO FALSE BEFORE PUBLIC APP GETS UPDATED ON THE 1ST OF EVERY MONTH
# hmmmmmmmmmmmmmmmmmm



currentYear          <- 2022
currentYear_hosp_ed  <- 2022

incRecentYearData <- T 
currentYear <- ifelse(incRecentYearData, currentYear, currentYear - 1)

incRecent_MultiYearData <- T   

t.year3 <- currentYear - 2
t.year5 <- currentYear - 4

yearGrp3 <- paste0(t.year3, "-", currentYear)
yearGrp5 <- paste0(t.year5, "-", currentYear)


###### USE THIS EVERYWHERE......
# yearGrp5      <- ifelse(incRecent_MultiYearData, "2016-2020", "2015-2019")
# yearGrp3      <- ifelse(incRecent_MultiYearData, "2018-2020", "2017-2019")
minYear      <- 2000
maxYear      <- currentYear


# For Hosp_ED_Deaths  
# use this FOR ALL combine  hosp/ed/Death work
# AND maybe add incRecent year type switch
yearGrp3_hosp_ed     <- paste0(currentYear_hosp_ed-2, "-", currentYear_hosp_ed)
yearGrp3_hosp_ed_num <- (currentYear_hosp_ed-2):(currentYear_hosp_ed)

yF           <- 100000  # rate constant 


# NOTE: "LABEL" was changed to "causeCode" 12/2020
# changed "BG" to "topLev"

# ccs_code_to_name_link.csv  is going away, 
# incudes ccs_diagP	and ccsName variables
# should be replaced  with files below and ccsCode and ccsName  OR whatever we call thouse

raceLink    <-  read_excel(paste0(standardsPlace,"raceLink.xlsx"))
ageLink     <-  read_excel(paste0(standardsPlace,"ageLink.xlsx"),sheet = "standard")


# Sex-specific cancers ============================================

# Breast cancer (F), Uterine Cancer
cancer_female <- c("Breast cancer", "Uterine cancer", "Ovary cancer")
cancer_male <- c("Prostate cancer")


## COLORS==========================================================


# ADA Compliant colors - http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette

# The palette with grey:
# greyPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Color-blind friendly (9 colors) palette 
paletteCB <- c("#0072B2", # darker blue, 
               "#4A4A4A", # darker gray,
               "#D55E00", # darker orange
               "#117733", # green
               "#56B4E9", # lightblue
               "#4BE62F", # light green
               "#E69F00", # lighter orange
               "#CC79A7",  # pink
               "#b22222" # firebrick
               )

paletteCB_race <- paletteCB[c(
  2, # darker gray
  3, # darker orange
  5, # lightblue
  4, # green
  7, # lighter orange
  1, # darker blue
  9, # firebrick
  6, # light green
  8 # pink
)]

# Top lev colors
topLev              <- c("Communicable","Cancer","Cardiovascular","Other Chronic","Injury","Ill-Defined","Perinatal","Other")
# topLevColors        <- brewer.pal(n = length(topLev), name = "Dark2") # Determine pallete; Dark2 allows max 8
topLevColors        <- paletteCB[1:length(topLev)]
names(topLevColors) <- topLev

# Toplev associated text colors (white or black) - for text in bars
topLevTextColors <- c("#FFFFFF", "#000000","#000000", "#000000", "#000000", "#000000", "#000000", "#000000")
names(topLevTextColors) <- topLev




#FROM plot life tables function

# raceColors1        <- c("seashell4", "chocolate1", "firebrick", "royalBlue1", "darkblue", "navajowhite3", "red",   "chartreuse4")
# raceNames1         <- c("AIAN_NH",   "ASIAN_NH",   "BLACK_NH",  "HISPANIC",   "MR_NH",    "NHPI_NH",      "TOTAL", "WHITE_NH")
# names(raceColors1) <- raceNames1
# totalColor         <- "red"
# 
# raceNames         <- raceLink$raceName 
# raceColors        <- brewer.pal(length(raceNames), "Paired")
# names(raceColors) <- raceNames

raceList <- raceLink[-8:-10,"raceNameShort"] %>% unlist() %>% unname()




# Race Codes
#raceCodes <- c("AIAN", "Asian", "Black", "Hisp", "Multi", "NHPI", "Other", "White", "Total")
#raceCodesColors <- c(greyPalette[1:(length(raceCodes) - 1)], "red")
# raceCodesColors <- c(greyPalette[1:length(raceCodes)], "red")
# raceCodesColors <- c("red", "yellow", "black", "brown", "blue", "lightblue", "darkblue", "gray","purple")

#names(raceCodesColors) <- raceCodes
#totalColor         <- "red"

# Race Names
raceName <- c("American Indian or Alaska Native", 
               "Asian", 
               "Black",
               "Latino",
               "Multi-Race",
               "Native Hawaiian and other Pacific Islander", 
               "Other",
               "White", 
               "Total")
raceNameColors <- paletteCB_race[1:length(raceName)]
names(raceNameColors) <- raceName

# Race Name Short
raceNameShort <- c("AI/AN", 
               "Asian", 
               "Black",
               "Latino",
               "Multi-Race",
               "NH/PI", 
               "Other",
               "White", 
               "Total")
raceNameShortColors <- paletteCB_race[1:length(raceNameShort)]
names(raceNameShortColors) <- raceNameShort



genderNames         <- c("Female","Male", "Total")
# genderColors          <- c("gray","lightblue")
genderColors <- paletteCB[1:length(genderNames)]
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
  read_excel(path(ccbInfo,"icd10_to_CAUSE.xlsx"), sheet="main")%>%
   filter(!is.na(causeList)) %>%
   mutate(causeNameShort = ifelse(is.na(causeNameShort), causeName, causeNameShort),
          topLevName     = case_when(topLevCode  == "0" ~ "All Causes",
                                     topLevCode  == "A" ~ "Communicable",
                                     topLevCode  == "B" ~ "Cancer",
                                     topLevCode  == "C" ~ "Cardiovascular",
                                     topLevCode  == "D" ~ "Other Chronic",
                                     topLevCode  == "E" ~ "Injury", 
                                     topLevCode  == "P" ~ "Perinatal",
                                    TRUE ~ "Ill-Defined")) %>%
  select(causeCode, causeName, causeNameShort, causeList, topLevCode, topLevName) %>%
  arrange(causeCode) %>%
  as.data.frame()


# deathCauseLink <- deathCauseLink %>% select(-causeList)
# write_csv(deathCauseLink, "deathCauseLink.csv")



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



cdph1 <- 
  theme_bw() + 
  theme(panel.grid.minor = element_blank(), 
        plot.title       = element_text(size = 16, color="darkblue"),
        axis.title       = element_text(size = 14), 
        axis.text  = element_text(size = 14)
  ) 







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











