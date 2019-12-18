library(readxl)
library(openxlsx)

myDrive  <- "f:"
myPlace  <- paste0(myDrive,"/0.CBD/myCBD")
upPlace  <- paste0(myDrive,"/0.CBD/myUpstream")

# ccs_dx_icd10cm_2019_1.xlsx is dowloaded from, with quotes removed from:
#    https://www.hcup-us.ahrq.gov/toolssoftware/ccsr/ccs_refined.jsp
#          CCS for ICD-10-CM Diagnoses, v2019.1 (October 2018) (beta version) (ZIP file, 597 KB)

rawBetaInfo <- as.data.frame(read_excel(paste0(upPlace,"/upData/ccs_dx_icd10cm_2019_1.xlsx")))
betaInfoCCS <- unique(rawBetaInfo[,c(2,4:8)])

hs <- createStyle(fontSize = 14, fontColour = "#FFFFFF", halign = "center",
                  fgFill = "#4F81BD", border="TopBottom", borderColour = "#4F81BD")

write.xlsx(betaInfoCCS, paste0(myPlace,"/myInfo/ICD10 to CCS Code and Names Linkage.xlsx"),headerStyle=hs, borders="all",colWidths = "auto", firstRow = TRUE)
