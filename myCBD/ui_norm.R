# =============================================================================
# "ui.R" file     
#
# required file for Shiny Application
#
# sets up user inputs (drop downs, buttons, etc.) and text in side pannels 
# sets up tabs and places all maps, charts, images, titles in main pannels
# set all "styles"
#
# Michael Samuel
# 2018
#
#=============================================================================

# "homeTab" = 11
# "interactiveMapTab" = 22
# "staticMapTab" = 23
# "rankByCauseTab" = 33
# "dataTableTab" = 45
# "rankByGeographyTab" = 44
# "trendTab" = 55
# "socialDeterminantsTab" = 66
# "raceTrendTab" = 56
# "educationTrendTab" = 57
# "hospitalMapTab" = 91

# STYLES, CONSTANTS AND FUNCTIONS FOR UI --------------------------------------

STATE <- "CALIFORNIA"   # needed this here with CDPH Shiny Server but not otherwise?

# function used as "short-cut" when making criteria for conditionals below
int_fC <- function(vec) {
  tRep <- length(vec)-1
  paste("input.ID == ",vec,    c(rep("|",tRep),""), collapse="")
}

fC <- function(vec) {   # Same as int_fC() but for a character vector
  tRep <- length(vec)-1
  paste0("input.ID == '", vec, c(rep("'|",tRep),"'"), collapse="")
}

# Styles for help buttons and boxes
myButtonSty     <- "height:22px; padding-top:0px; margin-top:-5px; float:right;
color: #fff; background-color: #337ab7; 
border-color: #2e6da4"
myHelpButtonSty <- "background-color: #694D75;font-size:14px;"
myBoxSty        <- "cursor:pointer; border: 3px solid blue;
padding-right:0px;padding-left:0px;"

# START OF UI --------------------------------------------------------------------

shinyUI(
  fluidPage(
    theme = "bootstrap.css",
    
    # current approach to setting style for main fonts and hyperlink font
    # TODO needs cleaning, improvement, and documentation from someone knowledgeable
    tags$head(
      tags$style(HTML("
                      @import url('//fonts.googleapis.com/css?family=Open+Sans');
                      * {font-family: 'Open Sans';line-height: 1.5;}
                      a {text-decoration: none; color: #0000EE;}
                      ")   ),
      includeScript("googleAnalytics.js")
      
      ),
    
    tags$h3(mTitle), # app main title supplied from Global
    
    # supprisingly enough, this removes the tick marks between the years on the year slider
    # TODO how does this work?
    tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
    
    
    # code from here https://community.rstudio.com/t/shiny-slider-new-design/24765/4
    #  so that slider bar does not fill with color
    tags$style(
      ".irs-bar {",
      "  border-color: transparent;",
      "  background-color: transparent;",
      "}",
      ".irs-bar-edge {",
      "  border-color: transparent;",
      "  background-color: transparent;",
      "}"
    ),
    
    
    sidebarPanel(width=3,
                 
                 # Tab help buttons on each tab ----------------------------
                 conditionalPanel(condition = fC(c("interactiveMapTab","staticMapTab")), actionButton("mapTab",           "Tab Help",style=myHelpButtonSty),br(),br()),
                 conditionalPanel(condition = fC(c("rankByCauseTab")),                   actionButton("conditionTab",     "Tab Help",style=myHelpButtonSty),br(),br()),
                 conditionalPanel(condition = fC(c("dataTableTab")),                     actionButton("conditionTableTab","Tab Help",style=myHelpButtonSty),br(),br()),
                 conditionalPanel(condition = fC(c("rankByCauseAndSexTab")),             actionButton("conditionSexTab",  "Tab Help",style=myHelpButtonSty),br(),br()),
                 conditionalPanel(condition = fC(c("rankByGeographyTab")),               actionButton("rankGeoTab",       "Tab Help",style=myHelpButtonSty),br(),br()),
                 conditionalPanel(condition = fC(c("trendTab")),                         actionButton("trendTab",         "Tab Help",style=myHelpButtonSty),br(),br()),
                 conditionalPanel(condition = fC(c("socialDeterminantsTab")),            actionButton("sdohTab",          "Tab Help",style=myHelpButtonSty),br(),br()),
                 conditionalPanel(condition = fC(c(1)),                                  actionButton("sdohTab",          "Tab Help",style=myHelpButtonSty),br(),br()),
                 
                 # Input selections on each tab  ----------------------------
                 #initializeWidgets(),
                 source(paste0(myPlace,"/myFunctions/input_widgets.R"),local = TRUE)$value,
                 
                 # Figure Download buttons ---------------------------------------------------
                 conditionalPanel(condition = fC(c("trendTab")), downloadButton('trendPNG', 'Download Figure'),br(),br()),     
                 conditionalPanel(condition = fC(c("trendTab")), downloadButton('trendData', 'Download Data'),br(),br()),     
                 
                 #conditionalPanel(condition = fC(c("staticMapTab")), downloadButton('mapFigure', 'Download Map')),       
                 conditionalPanel(condition = fC(c("rankByCauseTab")), downloadButton('rankCauseFigure', 'Download Figure'),br(),br()),
                 
                 # Home page side bar text ---------------------------------------------------
                 conditionalPanel(condition = fC(c("homeTab")),
                                  HTML('<left><img src="CDPH.gif" height="125" width="150"></left>'),  # 85  100
                                  br(),br(),
                                  
                                  helpText(h4("Welcome to the Preview Version of the CCB!"),style="color:green",align="left"), br(),
                                  
                                  h5(tags$a(href="CA_Health_Views.pdf","SEE CCB DATA IN ACTION, in the new 'Measuring Health Status in California'")), br(),
                                  
                                  actionButton("newsUse","News and Updates",style=myHelpButtonSty), br(),
                                  h5(tags$a(href="https://www.surveymonkey.com/r/2N2JSTV","Report 'bugs' HERE!")),
                                  h5(tags$a(href="https://www.surveymonkey.com/r/ZH9LSR8","Share your feedback HERE!")),
                                  helpText(textIntroA,style="color:black"), br(),
                                  helpText(textIntroC,style="color:black"), br(),
                                  if (whichData == "real") { helpText(textNote.real,style="color:black")},
                                  if (whichData == "fake") { helpText(textNote.fake,style="color:red")},
                                  br(),br(),
                                  icon("envelope-o"),tags$a(href = "mailto:michael.samuel@cdph.ca.gov","Questions?  Want to Help?"), br(),
                                  tags$a(href="https://shiny.rstudio.com/","Developed in R-Shiny"), br(),
                                  tags$a(href="https://github.com/mcSamuelDataSci/CACommunityBurden","GitHub Site")
                 ),
                 
                 # Home page 0 side bar text
                 conditionalPanel(condition = fC(c(10)),
                                  helpText(h2("which CONDITIONS result in the largest (by what MEASURE) BURDEN of MORTALITY (death) and MORBIDITY (cases, disabiltity, hospitalization) in which POPULATIONS (sex, race/ethnicity, age) in which COMMUNITIES in California at what TIME (years)?",
                                              style="color:red"),h1("WHY?",style="color:blue")),h2("WHAT TO DO ABOUT IT?")
                 ),
                 
                 # Help Texts ---------------------------------------------
                 # myGeo Help text
                 # conditionalPanel(condition = paste("(input.myGeo == 'Census Tract') & (",fC(c("interactiveMapTab","staticMapTab","hospitalMapTab")),")"),
                 #                  helpText(h6(tractWarning,style="color:red"))
                 # ),
                 # myMultiRace Help text
                 # conditionalPanel(condition = paste("(input.myMultiRace) & (",fC(c("raceTrendTab")),")"),
                 #                  helpText(h6(multiRaceWarning,style="color:red"))
                 # ),
                 
                 # Text on other pages  -----------------------------------------
                 conditionalPanel(condition = fC(c("rankByCauseTab","dataTableTab","rankByGeographyTab","trendTab","raceTrendTab","educationTrendTab","raceDisparityTab","socialDeterminantsTab")),
                                  paste('Note: All values <',criticalNumber,'including zeros are excluded '),style="color:green;font-weight: bold;"
                 ),
                 conditionalPanel(condition = fC(c("interactiveMapTab","staticMapTab","rankByCauseTab","dataTableTab","rankByGeographyTab","trendTab","raceTrendTab","educationTrendTab","raceDisparityTab","socialDeterminantsTab")),
                                  helpText(helpText('Note: YLL is "Years of Life Lost"',style="color:green;font-weight: bold;"))
                 ),
                 conditionalPanel(condition = "input.ID != homeTab",
                                  br(),HTML('<left><img src="CDPH.gif" height="125" width="150"></left>')
                 ),
                 
                 # Text on all side bars ------------------------------------------------------
                 helpText(br(),h4(VERSION),style="color:green")
                 
                 
    ), # -- END of sidebarPanel -------------------------------------------------------
    
    # MAIN PANNELS-------------------------------------------------------------------------
    mainPanel(
      useShinyjs(),
      tabsetPanel(type = "tab", id = "ID",
                  
                  tabPanel(title = "HOME", value = "homeTab",
                           br(),align='center',
                           h4(HTML(above1),align="left"),
                           fluidRow(
                             column(width=3,img(id="map1I",      src="mapInt.png",    width="100%", onmouseout="this.src='mapInt.png'",    onmouseover="this.src='mapInt2.png'",    style = myBoxSty)),
                             column(width=3,img(id="map2I",      src="mapStat.png",   width="100%", onmouseout="this.src='mapStat.png'",   onmouseover="this.src='mapStat2.png'",   style = myBoxSty)),
                             column(width=3,img(id="trendI",     src="trends.png",    width="100%", onmouseout="this.src='trends.png'",    onmouseover="this.src='trends2.png'",    style = myBoxSty)),
                             column(width=3,img(id="scatterI",   src="SDOH.png",      width="100%", onmouseout="this.src='SDOH.png'",      onmouseover="this.src='SDOH2.png'",      style = myBoxSty))), 
                           br(),
                           fluidRow(
                             column(width=4,img(id="rankgeoI",   src="rankGeo.png",   width="100%", onmouseout="this.src='rankGeo.png'",   onmouseover="this.src='rankGeo2.png'",   style = myBoxSty)),
                             column(width=4,img(id="ranktableI", src="table.png", width="100%", onmouseout="this.src='table.png'", onmouseover="this.src='table2.png'", style = myBoxSty)),
                             column(width=4,img(id="rankcauseI", src="rankCause.png",  width="100%", onmouseout="this.src='rankCause.png'",  onmouseover="this.src='rankCause2.png'",  style = myBoxSty)))
                  ),          
                  
                  tabPanel(title = "ABOUT", value = "aboutTab",
                           br(), 
                           includeMarkdown("About.md")
                  ),
                  
                  tabPanel(title = "LIFE EXPECTANCY", value = "lifeExpectancyTab",
                           br(),
                           plotOutput("lifeTable", width="100%",height = 700)
                  ),
                  
                  tabPanel(title = "INTERACTIVE MAP", value = "interactiveMapTab",
                           br(), htmlOutput("map_title")  ,
                           leafletOutput("cbdMapTL", width=700, height=700)
                  ),
                  # tabPanel("STATIC MAP",
                  # plotOutput("cbdMapTS",  height=700,width="100%"), value = "staticMapTab"),
                  
                  tabPanel(title = "RANK BY CAUSE", value = "rankByCauseTab",
                           br(),
                           plotOutput("rankCause", width="100%",height=700)
                  ),
                  # tabPanel("RANK BY CAUSE AND SEX",
                  # plotOutput("rankCauseSex", width="100%",height=700), value = "rankByCauseAndSexTab"),
                  
                  tabPanel(title = "RANK BY GEOGRAPHY", value = "rankByGeographyTab",
                           plotOutput("rankGeo", width="100%", height=1700)
                  ),
                  
                  tabPanel(title = "Trend", value = "trendTab",
                           br(), 
                           plotOutput("trend", width="100%",height=700)  # plotlyOutput("trend", width="100%",height=700),  value = "trendTab"),
                  ),
                  
                  tabPanel(title = "Age Trend", value = "ageTrendTab",
                           br(), 
                           plotOutput("trendAge", width="100%",height=700)
                  ),
                  
                  tabPanel(title = "Race Trend", value = "raceTrendTab",
                           br(),
                           plotOutput("trendRace", width="100%",height = 700)
                  ),
                  # myPlotly <- TRUE,
                  
                  tabPanel(title = "Race Dispartiy", value = "raceDisparityTab",
                           br(),
                           plotOutput("disparityRace", width="100%",height = 700) # plotlyOutput("disparityRace", width="100%",height = 700)
                  ),
                  
                  tabPanel(title = "Education Trend", value = "educationTrendTab",
                           br(), 
                           plotOutput("trendEducation", width="100%",height=700)
                  ),
                  
                  tabPanel(title = "DATA TABLE", value = "dataTableTab",
                           dataTableOutput("rankCauseT")   #DT::
                  ),   
                  
                  tabPanel(title = "SOCIAL DETERMINANTS", value = "socialDeterminantsTab",
                           br(), 
                           plotlyOutput("scatter", height=700)
                  ),
                  
                  tabPanel(title = "HOSPITAL DISCHARGE", value = "hospitalDischargeTab",
                           br(),
                           plotOutput("OSHPD1", height=700)
                  ),
                  
                  #  tabPanel("HOSPITAL DISCHARGE (2)",
                  #         br(),
                  #         plotlyOutput("OSHPD2", height=700),
                  #         value = "hospitalDischargeTab"),
                  # 
                  #  tabPanel("MDC/DRG",
                  #          br(),
                  #          plotOutput("mdcdrg", height = 700), value = "MDC/DRGTab"),
                  
                  tabPanel(title = "HOSPITAL DISCHARGE--PRIMARY AND ANY DIAGNOSES", value = "hospitalDischargePandDTab",
                           br(),
                           plotOutput("any_primary", height = 700)
                  ),
                  # tabPanel("HOSPITALIZATION MAP",
                  #          br(),
                  #          plotOutput("oshpdmap", height = 700), value = "hospitalMapTab"),
                  
                  tabPanel(title = "Arrows", value = "arrowsTab",
                           visNetworkOutput("network")
                  ),
                  
                  tabPanel(title = "Risk by Cause", value = "riskByCauseTab",
                           plotlyOutput("riskByCause", height = 600)
                  ),
                  
                  tabPanel(title = "Technical Documentation", value = "techDocTab",
                           br(), 
                           includeMarkdown("technical.md")
                  ),
                  
                  tabPanel(title = "Links to Other Data", value = "otherLinksTab",
                           br(), 
                           includeMarkdown("ourLinks.md")
                  )
                  ) # END tabSetPanel
      ) # END mainPanel
    ) # END fluidPage
  ) # END ShinyUI




# END =============================================================================================

# NOTES etc. :

# convert Markdown doc to Word if needed forediting
# https://cloudconvert.com/md-to-docx

# tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"), # removes ticks between years
# https://stackoverflow.com/questions/44474099/removing-hiding-minor-ticks-of-a-sliderinput-in-shiny

# "BETTER" drop down list look
# https://stackoverflow.com/questions/40513153/shiny-extra-white-space-in-selectinput-choice-display-label

#library(shinythemes)
# shinyUI(fluidPage(theme = "bootstrap.css",
#                  #shinythemes::themeSelector(),

# wellPanel
# navBarPanel 

# work on customizing help button
# actionButton("causeHelp", "?",style=" height:22px; padding-top:0px; margin-top:-5px; 
#      float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4") 
# selectizeInput("myCAUSE", "Cause:", choices=fullList, selected="A",
#    options = list(maxOptions = 10000),width='50%')),# size=30 selectize = F, size=3,
#width:100px;
# https://shiny.rstudio.com/reference/shiny/latest/selectInput.html
# https://shiny.rstudio.com/articles/selectize.html
# https://www.w3schools.com/html/html_form_elements.asp
#  https://www.w3schools.com/css/css3_buttons.asp
