server <- TRUE
if (server) highPath <-  "/mnt/projects/"
if(!server) highPath <-  "G:/"


library(docxtractr)
docX <- read_docx(paste0(highPath,"FusionData/State of Public Health/VizText.docx"))
ourText <- docx_extract_tbl(docX, 1)
