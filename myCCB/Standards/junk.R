#.... source this file....  in auto....



# 
# 
# FusionData
# 
#  0.CCB
#  
#   myCBD to myCCB
#  
#  0.CCB.resources   



server <- TRUE

fusionPlace    <- "/mnt/projects/FusionData/"
standardsPlace <- "/mnt/projects/FusionData/Standards/"
sdohPlace      <- "/mnt/projects/FusionData/SDOH/"



ccbPlace       <-  "/mnt/projects/FusionData/0.CCB/"  # -- change this location...
ccbData        <-  "/mnt/projects/FusionData/0.CCB/myCCB/myData/"  # -- change this location...
ccbInfo        <-  "/mnt/projects/FusionData/0.CCB/myCCB/myInfo/"  # -- change this location...
ccbFunctions   <-  "/mnt/projects/FusionData/0.CCB/myCCB/myFunctions/"  # -- change this location...
ccbUpstream    <-  "/mnt/projects/FusionData/0.CCB/myUpstream/"  # -- change this location...


# NOTE: "LABEL" was changed to "causeCode" 12/2020
# changed "BG" to "topLev"

# ccs_code_to_name_link.csv  is going away, 
# incudes ccs_diagP	and ccsName variables
# should be replaced  with files below and ccsCode and ccsName  OR whatever we call thouse





# library(readxl)
# library(dplyr)
# library(stringr)
# ccbPath       <- "/mnt/projects/FusionData/CCB Project/0.CCB/myCBD/"
# standardsPlace <- "/mnt/projects/FusionData/Standards"
# 
# # consider adding "Level"
# deathCauseLink
# deathCauseNamesLink
# death_Cause_Names
# 
fullCauseList   <-    read_excel( paste0(ccbInfo,"icd10_to_CAUSE.xlsx"), sheet="main") %>%
                      filter(!is.na(causeList)) %>%
                      mutate(causeNameShort = ifelse(is.na(causeNameShort), causeName, causeNameShort),
                             topLevName = ifelse(topLev == "0","All Causes",
                                          ifelse(topLev == "A","Communicable",
                                          ifelse(topLev == "B","Cancer",
                                          ifelse(topLev == "C","Cardiovascular",
                                          ifelse(topLev == "D","Other Chronic",
                                          ifelse(topLev == "E","Injury",
                                                           "Ill-Defined"))))))   ) %>%
                      select(causeCode, causeName, causeNameShort, causeList, topLev, topLevName) %>%
                      arrange(causeCode)


deathCauseLink   <-
  read_excel(paste0(ccbPath,"myInfo/icd10_to_CAUSE.xlsx"), sheet="main") %>%
   filter(!is.na(causeList)) %>%
   mutate(causeNameShort = ifelse(is.na(causeNameShort), causeName, causeNameShort),
          topLevName     = case_when(topLev  == "0" ~ "All Causes",
                                    topLev  == "A" ~ "Communicable",
                                    topLev  == "B" ~ "Cancer",
                                    topLev  == "C" ~ "Cardiovascular",
                                    topLev  == "D" ~ "Other Chronic",
                                    topLev  == "E" ~ "Injury")  ) %>%
  select(causeCode, causeName, causeNameShort, causeList, topLev, topLevName) %>%
  arrange(causeCode)
# 
# 
# 
# # not needed if sourced.....
# write.csv(deathCauseLink, paste0(standardsPlace,"/deathCauseLink.csv"))
# 
# 
# 
# hospCauseLink  <-    read_excel(paste0(ccbPath,"myInfo/CCS Code and Names Linkage.xlsx")) %>%
#                       mutate(causeCode = str_pad(ccsCode, 5,"left",pad="o")) %>%
#                       mutate(causeNameShort = ifelse(is.na(ccsNameShort),ccsName,ccsNameShort)) %>%
#                       select(causeCode, causeName = ccsName, causeNameShort, topLev, birth)

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
myLineLabelSize <- 18


myLineSize  <- 2
myPointSize <- 5 # line markers
myPointShape <- 18

myTheme <- theme_bw() +
  theme(plot.title   = element_text(size = myTitleSize, color="blue"),
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


# color standards -------------------------------


# ADA Compliant colors - http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette











