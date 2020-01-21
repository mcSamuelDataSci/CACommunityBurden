myPlace  <- "f:/0.CBD/myCBD"
whichData <- "real" 

library(sqldf)
library(dplyr)
library(readxl)
library(shiny)
library(fs)
library(stringr)
library(ggplot2)



myMeasure = "n_hosp"
mySex = "Total"
myN = 10
myYearG3 = "2016-2018"



oshpd_PDD_AGE  <- readRDS(file = path(myPlace, "myData/", whichData, "oshpd_PDD_AGE.rds")) %>% 
                            mutate( ccsCode = str_pad(ccs_diagP, 5,"left",pad="o"))
                     
                         
oshpd_ED_AGE   <- readRDS(file = path(myPlace, "myData/", whichData, "oshpd_ED_AGE.rds"))


ccsMap  <- as.data.frame(read_excel( path(myPlace,"myInfo/CCS Code and Names Linkage.xlsx"))) %>%
                    mutate(ccsCode = str_pad(ccsCode, 5,"left",pad="o")) %>%
                    select("ccsCode","ccsName","birth")


datCounty_AGE_3year   <- readRDS(file = path(myPlace, "myData/", whichData, "datCounty_AGE_3year.rds"))

gbdMap0 <- as.data.frame(read_excel( path(myPlace,"myInfo/gbd.ICD.Map.xlsx"), sheet="main"))

fullCauseList     <- gbdMap0[!is.na(gbdMap0$causeList),] %>% arrange(LABEL)  %>%
  filter(!is.na(PH),is.na(DETAIL)) %>%
  select(LABEL,causeList,nameOnly)


ageList <- unique(datCounty_AGE_3year$ageG) 
ageList[ageList == "0 - 4"]  <- " 0 - 4"
ageList[ageList == "5 - 14"] <- " 5 - 14"
ageList <- sort(ageList)


countyList <- unique(datCounty_AGE_3year$county)



rankOSHPDAgeCause <- function(myData = "Deaths",
                              myAgeG = "75 - 84",
                              myCounty = "Los Angeles",
                              myMeasure = "n_hosp",
                              mySex = "Total",
                              myN = 10,
                              myYearG3 = "2016-2018"
                              ){
  
  
  
  
  if (myData == "Deaths") {
    myMeasure = "Ndeaths"
    
    tLab  <- "Deaths"
     
      t.dataSet  <- datCounty_AGE_3year %>% filter(yearG3 == myYearG3, Level == "lev2") %>%
        select(sex,ageG,CAUSE,county,measure=myMeasure) %>%
        mutate(ageG = ifelse(ageG == "5 - 14"," 5 - 14",ageG),
               ageG = ifelse(ageG == "0 - 4"," 0 - 4",ageG)) %>%
        filter(sex == mySex) 
        
     
      
      causeTemp <- data.frame(CAUSE = fullCauseList$LABEL ,stringsAsFactors = FALSE)
      ageTemp   <- data.frame(ageG = unique(t.dataSet$ageG),stringsAsFactors = FALSE)
      county    <- data.frame(county   = unique(t.dataSet$county),stringsAsFactors = FALSE)
      fullMat   <- sqldf(" select * from  county cross join causeTemp cross join ageTemp")
      
      
      t.dataSet <- full_join(t.dataSet,fullMat,by=c("county","CAUSE","ageG")) %>%
        arrange(county,CAUSE, ageG) %>%
        mutate(measure = ifelse(is.na(measure),0,measure))    %>%
        left_join(., fullCauseList, by = c("CAUSE" = "LABEL")) %>%
        rename(ccsName = nameOnly)
      
  }
  
  
  
  
  if (myData == "PDD") { t.dataSet  <- oshpd_PDD_AGE 
  tLab  <- "Hospitalizations"}
  if (myData == "ED")  { t.dataSet  <- oshpd_ED_AGE
  tLab  <- "ER Visits"} 
  
  
  if (myData %in% c("PDD","ED")) {
  
  t.dataSet <- t.dataSet %>%      
    select(ageG, county, ccsCode, measure = n_hosp,sex,year) %>%  #measure rename is a quick fix for now
    mutate(ageG = ifelse(ageG == "5 - 14"," 5 - 14",ageG),
           ageG = ifelse(ageG == "0 - 4"," 0 - 4",ageG)) %>%
    filter(sex == mySex)   %>% select(-sex,-year)    ## ALL sex = "Total"  and year is only = 2016 for now
  
  library(sqldf)
  causeTemp <- data.frame(ccsCode = unique(t.dataSet$ccsCode))
  ageTemp   <- data.frame(ageG = unique(t.dataSet$ageG))
  county    <- data.frame(county   = unique(t.dataSet$county))
  fullMat   <- sqldf(" select * from  county cross join causeTemp cross join ageTemp")
  
  
  t.dataSet <- full_join(t.dataSet,fullMat,by=c("county","ccsCode","ageG")) %>%
    arrange(county,ccsCode, ageG) %>%
    mutate(measure = ifelse(is.na(measure),0,measure))  %>%
    left_join(ccsMap,by="ccsCode")    %>%
    filter(!birth) %>% select(-birth) %>%
    rename(CAUSE = ccsCode)
  
  }
  
  
  
  # create a vector of CAUSE for top N
  theseCodes <- t.dataSet %>%
    filter(county == myCounty) %>%
    group_by(ageG) %>% arrange(desc(measure)) %>% dplyr::slice(1:myN) %>% #this selects the top N rows for myOSHPDtype
    filter(ageG == myAgeG) %>% ungroup() %>% pull(CAUSE)
  
  
  
  # creates dataframe with data only for CAUSEs from theseCodes, i.e. the top N CAUSES for the specified theseCodes
  plot_data.2 <-     t.dataSet %>%
    filter(!is.na(CAUSE), county == myCounty, CAUSE %in% theseCodes) %>%
    #filter(sex == mySex, year == myYear) %>%
    group_by(ageG)    %>%
    mutate(ccsName = forcats::fct_reorder(ccsName, filter(., ageG == myAgeG)  %>%
                                            pull(measure)))
  
  sexLab <- ""
  if (mySex != "Total") sexLab <- paste0(", among ",mySex,"s")
  myTitle <- paste0("Number of ", tLab,
                    " by Cause by Age Group, Age Group ", 
                    myAgeG, " ORDER, in ", myYearG3, " in ", myCounty, sexLab)
  
  ageCausePlot  <-     ggplot(plot_data.2, aes(x = ccsName, y = measure)) +
    coord_flip() + geom_bar(stat = "identity", fill = "blue") +
    facet_grid(. ~ ageG,                    labeller=labeller(type = label_wrap_gen(5))) +
    #  facet_grid(. ~ ageG, scales = "free_x", labeller=labeller(type = label_wrap_gen(5))) +
    theme_bw() + 
    scale_y_continuous(labels = scales::comma) + # numbers shown with commas
    scale_x_discrete(labels = scales::wrap_format(50)) + #x-axis is condition label--wrapping text so it stacks on top of each other
    labs(title = myTitle,x ="condition",y = paste("Number of",tLab)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  ageCausePlot
  
}



#(for testing code outside of the app)
if(1==2) {
  myCounty = "CALIFORNIA"
  mySex = "Total"
  myN = 10
  myYearG3 = "2016-2018"
  myAgeG = "75 - 84"
  myAgeG = "0 - 4"
  myAgeG = "25 - 34"
  myAgeG = "15 - 24"
  myMeasure = "n_hosp"
  myData = "PDD"
  myData = "ED"
  #cDeathRate"
}


# rankOSHPDAgeCause(myCounty = "Los Angeles",
#                               myMeasure = "n_hosp",
#                               mySex = "Total",
#                               myN = 10,
#                               myYearG3 = "2016-2018",
#                               myAgeG = "75 - 84",
#                               myData = "PDD")










library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("AGE"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput( "myData", label = "Data Source:", choices = c("PDD","ED","Deaths")),
          selectInput( "myAgeG", label = "Age Group:", choices = ageList, selected = "35 - 44"),
          selectInput( "myCounty", label = "County:", choices = countyList, selected = "CALIFORNIA")
                              )
        ,

        mainPanel(
           plotOutput("agePlot")
        )
    )
)

    
# Define server logic required to draw a histogram
server <- function(input, output) {

    output$agePlot <- renderPlot({rankOSHPDAgeCause(input$myData,input$myAgeG, input$myCounty)})
}

# Run the application 
shinyApp(ui = ui, server = server)
