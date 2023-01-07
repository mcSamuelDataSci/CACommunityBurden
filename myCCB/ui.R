
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


options(shiny.sanitize.errors = FALSE)

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
#####tuhdtjyj
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

# Colors from CDPH logo
BLUE <- "#0079C2"
GREEN <- "#5C8727"
ORANGE <- "#EB6E1F"
BEIGE <- "#F9F5F1"




# Styles for help buttons and boxes ============================

myInputHelpButtonSty <- paste0("width:20px;  color:#fff; background-color:#337ab7; border-color:white; padding:0px; font-size: 18px;",
                               "margin:0px;",
                               "margin-left:10px;",
                               "float:right;"
)   #2e6da4
helpIcon <- "?"

# Used for broad group input in leading causes tab - Replaced float right with display inline block. 
myInputHelpButtonSty_broadGroup <- paste0("width:20px;  color:#fff; background-color:#337ab7; border-color:white; padding:0px; font-size: 18px;",
                               "margin:0px;",
                               "margin-left:10px;",
                               "display:inline-block;"
) 

#helpIcon <- icon("question-circle-o") # or: HTML('<i class="fa fa-question-circle-o"></i>')
#helpIcon <- icon("question-circle")   # or: HTML('<i class="fa fa-question-circle"></i>')  
#helpIcon <- icon("question") 
#helpIcon <- icon("info")


# # Styles for help buttons and boxes
myTabHelpButtonSty <- "background-color:#694D75; font-size:14px;"
myMeasuresButtonSty <- "background-color:#694D75; font-size:14px; margin: 0;"
myDownloadButtonSty <- "padding-left: 6px; padding-right: 6px; margin-top: 5px;"
myBoxSty        <- "cursor:pointer; border: 3px solid blue; padding-right:0px;padding-left:0px;"
myBoxSty_side        <- "height: auto; cursor:pointer; border: 2px solid #222D32; border-radius: 5px; padding-right:0px;padding-left:0px;"
mySidebarTextSty <- "float:left; margin: 20px; color:#000000;"








# START OF UI --------------------------------------------------------------------

shinyUI(
  fluidPage(
    theme = "bootstrap.css",
    
    # Jaspreet's addition ---------------------------------------
    
    # This gets rid of the unwanted bacl space by making it the same color as the main panel background.
    # Additionally, this hides the scrollbar so that there is only one
    # tags$head(tags$style(HTML('.wrapper {
    #                             background-color: #ecf0f5 !important;
    #                           height: auto !important; 
    #                           position:relative; 
    #                           overflow-x:hidden; 
    #                           overflow-y:hidden
    #                           }'))),
    
    # Gets rid of the unwanted black space on the bottom by making it the same color as the main panel
    tags$head(tags$style(HTML('.wrapper {
                                background-color: #ecf0f5 !important;
                              height: auto !important;
                              }'))),
    
    
    # This gets rid of the white space on the side so that the entire page gets filled up
    tags$head(tags$style(HTML('.container-fluid {
                              padding-right: 0px !important;
                              padding-left: 0px !important;
                              }'))),
    
    tags$head(tags$style(HTML('.sidebar {
                              height: 95vh;
                              width: 300px;
                              position: fixed;
                              overflow-y: scroll;
                              z-index: 1000;
                              }'))),
    
    # ------------------------------------------------------------------------------
    
    
    
    # current approach to setting style for main fonts and hyperlink font
    # TODO needs cleaning, improvement, and documentation from someone knowledgeable
    tags$head(
      tags$style(HTML("
                      @import url('//fonts.googleapis.com/css?family=Open+Sans');
                      * {font-family: 'Open Sans';line-height: 1.5;}
                      #textHomeTab a { color: #0079C2; }
                      .fa-envelope-o {color:black;}
                      a {text-decoration: none; color: #0000EE;}
                      #inputs {color:black;}
                      ",
                      
                      "
                      /* main sidebar */
                      .skin-blue .main-sidebar { background-color: #F9F5F1; }
                      /* active selected tab in the sidebarmenu */
                      .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{ background-color: #222D32; color:white; }
                      /* other links in the sidebarmenu */
                      .skin-blue .main-sidebar .sidebar .sidebar-menu a{ background-color: #D6D2CE; color: #000000; }
                      /* other links in the sidebarmenu when hovered */
                      .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{ background-color: #222D32; color:white; }
                      ",
                      
                      "
                      .navbar-brand {display:none;}
                      .navbar-default {background-color: #222D32 !important;}
                      .navbar-default .navbar-nav > li > a {font-size: 13px;}
                      ",
                      "
                      .tabbable {font-family: Arial;}
                      ",

                      "
                      .navbar-default .navbar-nav > .active > a,
                      .navbar-default .navbar-nav > .active > a:focus,
                      .navbar-default .navbar-nav > .active > a:hover
                      {color: white; background-color: #1E282D;}
                      ",

                      "
                      .tabbable > .nav > .active > a,
                      .tabbable > .nav > .active > a:focus,
                      .tabbable > .nav > .active > a:hover
                      {color: black;}
                      "
                      )
                 )),
    
    
    # Browser tab icon
    tags$head(HTML("<link rel='icon' href='Fusion Center Knot.png'>")),
      
      
      
      ######## Google Analytics Script Start ###############
      # HTML(
      #   "<script async src='https://www.googletagmanager.com/gtag/js?id=UA-154687382-1'></script>"
      # ),
      # includeScript("googleAnalytics.js"),
    
    
    tags$head(includeScript("www/googleAnalytics.js")),
    tags$noscript(tags$iframe(src="https://www.googletagmanager.com/ns.html?id=GTM-5Q2H73S",
                              height="0", width="0", style="display:none;visibility:hidden")),
    # tags$head(includeScript("www/scrollbar.js")),
    
    
      ######## Google Analytics Script End ###############
      
      
      
      
    
    #tags$h3(mTitle), # app main title supplied from Global
    
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
    
    dashboardPage(
      # https://stackoverflow.com/questions/31440564/adding-a-company-logo-to-shinydashboard-header
      dashboardHeader(title = appTitle, titleWidth = 500,
                      tags$li(a(href = 'https://www.cdph.ca.gov/Programs/FCSD/Pages/FusionCenter.aspx',
                                target = "_blank", 
                                rel = "noopener noreferrer",
                                img(src = 'Fusion Center Knot.png',
                                    title = "Company Home", height = "30px"),
                                style = "padding-top:10px; padding-bottom:10px;"),
                              class = "dropdown")),
      dashboardSidebar(width=300,
                       
                       
                       
                       # Menu Items & tabHelp ---------------------------------------------------
                       
                     br(),
                       hidden(
                         div(id = "plotsMenu",
                             sidebarMenu(id = "plotsMenuItems",
                                         menuItem("Show Visualization Controls", tabName = "tabInputs"),
                                         menuItem("Show Tab Information", tabName = "tabInfo")
                             )
                         ),
                         div(id = "tabHelpInfo", style = mySidebarTextSty,
                             htmlOutput("currTabInfo", inline=TRUE)
                         ),
                         actionButton("tabHelp", "Tab Help", style=myTabHelpButtonSty)
                       ),

                       # Input selections on each tab  --------------------------------------------
                       source(paste0(myPlace,"/myFunctions/inputFunctions/input_widgets.R"),local = TRUE)$value,
                       
                       # Figure Download buttons ---------------------------------------------------
                       hidden(
                         div(id = "ourDownloads", style = "margin: 10px;",
                             downloadButton('ourPNG', 'Download Figure', style = paste0("float: left;", myDownloadButtonSty)),
                             downloadButton('ourData', 'Download Data', style = paste0("float: right;", myDownloadButtonSty))
                         ),
                         # div(id = "rankCauseDownloads", style = "margin: 10px;",
                         #     downloadButton('rankCauseFigure', 'Download Figure', style = myDownloadButtonSty)
                         # ),
                         div(id = "ourOnlyPNGDownload", style = "margin: 10px;",
                             downloadButton('ourOnlyPNG', 'Download Figure', style = paste0("float: left;", myDownloadButtonSty))
                         )
                       ),

                       # Side bar text -------------------------------------------------------------
                       
                       div(id = "textHomeTab", style = mySidebarTextSty,
                           HTML('<left><img src="CDPH.gif" height="125" width="150"></left>'),  # 85  100
                           br(),br(), br(),

                           tags$a(href = "xMDA/2020_Excess_Mortality.html", 
                                  target = "_blank", 
                                  rel = "noopener noreferrer", 
                                  img(
                                    src = "xMDA/backgroundLight_xMDA.png", 
                                    width = "100%",
                                    onmouseout = "this.src='xMDA/backgroundLight_xMDA.png'",
                                    onmouseover = "this.src='xMDA/backgroundDark_xMDA.png'",
                                    style = myBoxSty_side
                                  )
                                  ), 
                           
                           br(), br(), br(),
                           
                           # h5(tags$a(href="SOPH_2020.pdf",target="_blank", "CCB Data in Action, 2020 State of Public Health Handout")), 
                           # h5(tags$a(href="SOPH_2019.pdf",target="_blank", "CCB Data in Action, 2019 State of Public Health Handout")), 
                           # h5(tags$a(href="https://skylab.cdph.ca.gov/lghcBurdenView/",target="_blank", "See Multiple Measures of Health in Your County")), br(),
                           appTextL$textIntroA, br(), br(), appTextL$textIntroC, br(), br(),
                           #helpText(appTextL$textIntroA,style="color:black"), br(),
                           #helpText(textIntroC,style="color:black"), br(),
                           if (whichData == "real") { helpText(appTextL$textNote.real,style="color:black")},
                           if (whichData == "fake") { helpText(appTextL$textNote.fake,style="color:red")},
                           br(),br(),
                           
                           
                           HTML("<div id='sophDropdown' class='dropdown'>
                             <button class='dropbtn'>STATE OF PUBLIC HEALTH<br/>UPDATES</button>
                             <div id = 'sophDropdownContent' class='dropdown-content'>
                               <a href='SOPH/2022/Full Report.html' target = '_blank' rel = 'noopener noreferrer'>2022 Full Update</a>
                               <a href='SOPH/2021/Full Report.html' target = '_blank' rel = 'noopener noreferrer'>2021 Full Update</a>
                               <a href='SOPH/2020/Handout.pdf' target = '_blank' rel = 'noopener noreferrer'>2020 Handout</a>
                               <a href='SOPH/2019/Handout.pdf' target = '_blank' rel = 'noopener noreferrer'>2019 Handout</a>
                             </div>
                             </div>"
                             ),
                           
                           br(), br(), br(), 
                           
                           actionButton("multipleMeasures", 
                                        HTML("<center>See Multiple Measures of<br/>Health in Your County</center>"), 
                                        style=myMeasuresButtonSty,
                                        onclick = "window.open('https://skylab.cdph.ca.gov/lghcBurdenView/', '_blank')"
                                        ), 
                           
                           br(), br(),
                           
                           actionButton("newsUse","News and Updates",style=myTabHelpButtonSty), br(),
                           h5(tags$a(href="https://www.surveymonkey.com/r/2N2JSTV",target="_blank","Report 'bugs' HERE!")),
                           h5(tags$a(href="https://www.surveymonkey.com/r/ZH9LSR8",target="_blank","Share your feedback HERE!")),
                           
                           
                           icon("envelope-o"),tags$a(href = "mailto:CCB@cdph.ca.gov","Questions?  Want to Help?"), br(),
                                                      tags$a(href="https://shiny.rstudio.com/",target="_blank","Developed in R-Shiny"), br(),
                           tags$a(href="https://github.com/mcSamuelDataSci/CACommunityBurden",target="_blank","GitHub Site")
                           
                       ),
                       
                       div(id = "textNotHomeTab", style = mySidebarTextSty,
                           HTML('<center><img src="CDPH.gif" height="125" width="150"></center>'),
                           br(),
                           br(),HTML('<center><img src="Fusion Center Knot.png" height="125" width="150"></center>'),
                       ),
                       
                       # Home page 0 side bar text. condition = fC(c(10)),  Not sure when this comes up?
                       hidden(div(id = "homeTab0",
                                  helpText(h2("which CONDITIONS result in the largest (by what MEASURE) BURDEN of MORTALITY (death) and MORBIDITY (cases, disabiltity, hospitalization) in which POPULATIONS (sex, race/ethnicity, age) in which COMMUNITIES in California at what TIME (years)?",
                                              style="color:red"),h1("WHY?",style="color:blue")),h2("WHAT TO DO ABOUT IT?")
                       )),
                       
                       # Text on all side bars ------------------------------------------------------
                       div(id = "textAllTabs", style = mySidebarTextSty,
                           helpText(br(),h4(VERSION),style=paste0("color:", GREEN)),
                           helpText(h4(paste(format(Sys.Date(),"%B 01, %Y"))),style=paste0("color:", GREEN))
                       )
                       
                       
      ), # -- END of dashboard Sidebar-------------------------------------------------------
      
      # MAIN PANNELS-------------------------------------------------------------------------
      dashboardBody(
        useShinyjs(),
        navbarPage(title="", id = "navsID", # collapsible = TRUE,
                   
                   tabPanel(title = strong("HOME"), value = "home",
                            br(),align='center',
                            h4(HTML(appTextL$above1),align="left"),
 fluidRow(
  column(width=3,img(id="mapI",       src="map.png",              width="100%", onmouseout="this.src='map.png'",               onmouseover="this.src='map2.png'",       style = myBoxSty)),
  column(width=3,img(id="trendI",     src="trends.png",           width="100%", onmouseout="this.src='trends.png'",            onmouseover="this.src='trends2.png'",    style = myBoxSty)),
  column(width=3,img(id="rankgeoI",   src="rank_by_geography.png",width="100%", onmouseout="this.src='rank_by_geography.png'", onmouseover="this.src='rank_by_geography2.png'",      style = myBoxSty)),
  column(width=3,img(id="rankcauseI", src="rank_by_cause.png",    width="100%", onmouseout="this.src='rank_by_cause.png'",     onmouseover="this.src='rank_by_cause2.png'",      style = myBoxSty))
  
  ),
  
 br(),
 fluidRow(
  column(width=3,img(id="dispI",       src="disparities.png",              width="100%", onmouseout="this.src='disparities.png'",               onmouseover="this.src='disparities2.png'",    style = myBoxSty)),
  column(width=3,img(id="sdohI",       src="SDOH.png",           width="100%", onmouseout="this.src='SDOH.png'",            onmouseover="this.src='SDOH2.png'",    style = myBoxSty)),
  column(width=3,img(id="hospI",   src="hospitalizations.png",width="100%",     onmouseout="this.src='hospitalizations.png'",     onmouseover="this.src='hospitalizations2.png'",      style = myBoxSty)),
  column(width=3,img(id="riskI",   src="risk.png",width="100%", onmouseout="this.src='risk.png'", onmouseover="this.src='risk2.png'",      style = myBoxSty))
  
                   )
 ),
  
  
                   
                   tabPanel(title = strong("MAPS"), value = "maps",
                            tabsetPanel(type = "tab", id = "mapsID",
                                        tabPanel(title = "INTERACTIVE MAP", value = "interactiveMapTab",
                                                 br(), htmlOutput("map_title")  ,
                                                 leafletOutput("cbdMapTL", width=700, height=700)
                                        )
                            )
                   ),
                   
                   tabPanel(title = strong("RANKS"), value = "ranks",    # value of navsID is 'ranks' here
                            tabsetPanel(type = "tab", id = "ranksID",
                                        tabPanel(title = "RANK BY CAUSE - Deaths", value = "rankByCauseTab",   # value of ranksID is 'rankByCauseTab' here
                                                 br(),
                                                 plotOutput("rankCause", width="100%",height=700)
                                        ),
                                        tabPanel(title = "RANK BY GEOGRAPHY - Deaths", value = "rankByGeographyTab",
                                                 plotOutput("rankGeo", width="100%", height=1700)
                                        ),
                                        tabPanel(title = "MULTIPLE CAUSES OF DEATH", value = "mcodTab", 
                                                 plotOutput("mcodPrimarySecondary", width = "100%", height = 700)
                                                 ),
                                        
                                        tabPanel(title = "AGE RACE FOCUS", value = "ageRaceFocusTab",
                                                 plotOutput("ageRaceFocus", width="100%", height=1700)
                                        ),
                                              
                                        tabPanel(title = "Death Hosp ED", value = "deathHospEDTab",
                                                 plotOutput("deathHospED", width="100%", height=1700)
                                        ),
                                        
                                        tabPanel(title = "Attributable Risks - IHME", value = "riskByCauseTab",
                                                 plotlyOutput("riskByCause", height = 600)
                                        ),
                                        tabPanel(title = "Two-Year IHME Rankings", value = "arrowsTab",
                                                 htmlOutput("arrowsTitles"),
                                                 visNetworkOutput("network")
                                                 # fluidRow(
                                                 #   column(10, visNetworkOutput("network")),
                                                 #   column(2, 
                                                 #          htmlOutput("arrowsLegend")
                                                 #) 
                                                 #)
                                        )
                            )
                   ),
                   
                   tabPanel(title = strong("TRENDS"), value = "trends",
                            tabsetPanel(type = "tab", id = "trendsID",
                                        
                                        tabPanel(title = "Sex Trend", value = "sexTrendTab",
                                                 br(),
                                                 plotOutput("trendSex", width="100%",height=700)
                                        ),
                                        
                                        tabPanel(title = "Age Trend", value = "ageTrendTab",
                                                 br(),
                                                 plotOutput("trendAge", width="100%",height=700)
                                        ),
                                        tabPanel(title = "Race Trend", value = "raceTrendTab",
                                                 br(),
                                                 plotOutput("trendRace", width="100%",height = 700)
                                        ),
                                        
                                        tabPanel(title = "Education Trend", value = "educationTrendTab",
                                                 br(),
                                                 plotOutput("trendEducation", width="100%",height=700)
                                        ),
                                        
                                        tabPanel(title = "LIFE EXPECTANCY", value = "lifeExpectancyTab",
                                                 br(),
                                                 plotOutput("lifeTable", width="100%",height = 700)
                                        ), 
                                        tabPanel(title = "Leading Causes", value = "topTrendsTab",
                                                 br(),
                                                 plotOutput("trendTop", width="100%",height = 800)
                                        )
                            )
                   ),
                   
                   tabPanel(title = strong("DISPARITIES"), value = "disparities",
                            tabsetPanel(type = "tab", id = "disparitiesID",
                                        tabPanel(title = "Disparties", value = "disparitiesTab",
                                                 br(),
                                                 plotOutput("disparityRace", width="100%",height = 700) # plotlyOutput("disparityRace", width="100%",height = 700)
                                        )
                            )
                   ),
                   
                   
                   
 
         
                   
 
 
 #new SDOH
 
 
 tabPanel(title = strong("SDOH"), value = "sdoh",
          tabsetPanel(type = "tab", id = "sdohID",
                      
                      tabPanel(title = "SOCIAL DETERMINANTS", value = "sdohTab",
                               br(),
                               htmlOutput("sdoh_title"), # Search for "sdoh_title" in server.R to see how the title is generated
                               fluidRow(
                                 
                                 column(width = 8, height = 200,
                                        shinydashboard::box(
                                          title = NULL,
                                          status = "primary",
                                          width = NULL,
                                          solidHeader = F,
                                          collapsible = F,
                                          plotlyOutput("scatter")
                                        )),
                                 column(width = 4, 
                                        shinydashboard::box(
                                          title = NULL,
                                          status = "primary",
                                          width = NULL,
                                          solidHeader = F,
                                          collapsible = F,
                                          plotOutput("violin1")
                                        ))
                               ), 
                               br(),
                               htmlOutput("sdoh_title2"),
                               fluidRow(
                                 column(width = 6,
                                        shinydashboard::box(
                                          title = NULL,
                                          status = "primary",
                                          width = NULL,
                                          solidHeader = F,
                                          collapsible = F,
                                          plotOutput("sdohMap1")
                                        )),
                                 column(width = 6,
                                        shinydashboard::box(
                                          title = NULL,
                                          status = "primary",
                                          width = NULL,
                                          solidHeader = F,
                                          collapsible = F,
                                          plotOutput("hist1")
                                        ))
                               ),
                               br(),
                               htmlOutput("sdoh_title3"),
                               
                               fluidRow(
                                 column(width = 6,
                                        shinydashboard::box(
                                          title = NULL,
                                          status = "primary",
                                          width = NULL,
                                          solidHeader = F,
                                          collapsible = F,
                                          plotOutput("sdohMap2")
                                        )),
                                 column(width = 6,
                                        shinydashboard::box(
                                          title = NULL,
                                          status = "primary",
                                          width = NULL,
                                          solidHeader = F,
                                          collapsible = F,
                                          plotOutput("hist2")
                                        ))
                               )
                               
                               
                               )
                      )),
 
 
 
 
 
 # ##################################                                 
 
 
 
 
                   
                   tabPanel(title = strong("HOSPITALIZATIONS"), value = "hospitalizations",
                            tabsetPanel(type = "tab", id = "hospitalizationsID",
                                        
                                       
                                        tabPanel(title = "HOSPITAL DISCHARGE", value = "hospitalDischargeTab",
                                                 br(),
                                                 plotOutput("OSHPD1", height=700)
                                        ),
                                        tabPanel(title = "HOSPITAL DISCHARGE--PRIMARY AND ANY DIAGNOSES", value = "hospitalPrimaryAnyTab",
                                                 br(),
                                                 plotOutput("any_primary", height = 700)
                                        )
                            )
                   ),
                   
 
 
 

                                 
                                 
                                 
                                 
                                 
 
 
 
 
 
  
                    # JASPO - Demographics
                   tabPanel(strong("DEMOGRAPHICS"), value = "demographics",
                            
                            tabsetPanel(type = "tab", id = "demographicsID",
                                        tabPanel(title = "Demographics", value = "demographicsTab",
                                                 br(),
                                                 
                                                 fluidRow(
                                                   column(width = 6,
                                                          shinydashboard::box(
                                                            title = NULL,
                                                            #closable = F,
                                                            status = "primary",
                                                            width = NULL,
                                                            solidHeader = F,
                                                            collapsible = F,
                                                            #maximizable = T,
                                                            #enable_dropdown = F,
                                                            actionButton("plotButton_demoPop_RacePie", "Plot", icon("chart-pie"),
                                                                         style="padding: 6px; background-color: #D1D1D1; color: black;"),
                                                            actionButton("tableButton_demoPop_RacePie", "Table", icon("table"),
                                                                         style = "padding: 6px; background-color: #D1D1D1; color: black;"
                                                                         ),
                                                            hr(),
                                                            plotlyOutput("demoPop_RacePie"),
                                                            hidden(DT::dataTableOutput("demoPop_RacePie_table"))
                                                          )),
                                                   column(width = 6,
                                                          shinydashboard::box(
                                                            title = NULL,
                                                            #closable = F,
                                                            status = "primary",
                                                            width = NULL,
                                                            solidHeader = F,
                                                            collapsible = F,
                                                            #enable_dropdown = F,
                                                            actionButton("plotButton_demoPop_Pyramid", "Plot", icon("chart-bar"),
                                                                         style = "padding: 6px; background-color: #D1D1D1; color: black;"),
                                                            actionButton("tableButton_demoPop_Pyramid", "Table", icon("table"),
                                                                         style = "padding: 6px; background-color: #D1D1D1; color: black;"),
                                                            hr(),
                                                            plotlyOutput("demoPop_Pyramid"),
                                                            hidden(DT::dataTableOutput("demoPop_Pyramid_table"))
                                                          ))
                                                 ),

                                                 fluidRow(
                                                   column(width = 6,
                                                    shinydashboard::box(
                                                       title = NULL,
                                                      # closable = F,
                                                       status = "primary",
                                                       width = NULL,
                                                       solidHeader = F,
                                                       collapsible = F,
                                                      # enable_dropdown = F,
                                                       actionButton("plotButton_demoPop_RaceAge", "Plot", icon("chart-bar"),
                                                                    style = "padding: 6px; background-color: #D1D1D1; color: black;"),
                                                       actionButton("tableButton_demoPop_RaceAge", "Table", icon("table"),
                                                                    style = "padding: 6px; background-color: #D1D1D1; color: black;"),
                                                       hr(),
                                                       plotlyOutput("demoPop_RaceAge"),
                                                       hidden(DT::dataTableOutput("demoPop_RaceAge_table"))
                                                       )
                                                     ),
                                                   column(width = 6,
                                                          shinydashboard::box(
                                                            title = NULL,
                                                           # closable = F,
                                                            status = "primary",
                                                            width = NULL,
                                                            solidHeader = F,
                                                            collapsible = F,
                                                            #enable_dropdown = F,
                                                            div(style = "display:inline-block",
                                                                actionButton("plotButton_demoPop_Trend", "Plot", icon("chart-line"),
                                                                             style = "padding: 6px; background-color: #D1D1D1; color: black;")
                                                            ),
                                                            div(style = "display:inline-block",
                                                                actionButton("tableButton_demoPop_Trend", "Table", icon("table"),
                                                                             style = "padding: 6px; background-color: #D1D1D1; color: black;")
                                                            ),
                                                            div(style="display:inline-block; float:right;",
                                                                selectInput("plotSelect_demoPop_Trend", label = "Select Trend:", choices = c("Total", "Sex", "Race/Ethnicity", "Age Group"))
                                                                ),
                                                            br(),
                                                            br(),
                                                            hr(),
                                                            plotlyOutput("demoPop_Trend"),
                                                            hidden(DT::dataTableOutput("demoPop_Trend_table"))
                                                          )
                                                   )
                                                   )

                                                 # Uncomment above


                                                 # boxPlus(
                                                 #   title = "Population Demographics in California",
                                                 #   closable = F,
                                                 #   width = NULL,
                                                 #   solidHeader = F,
                                                 #   collapsible = F,
                                                 #   enable_dropdown = F,
                                                 #   fluidRow(
                                                 #     column(width = 7,
                                                 #            plotlyOutput("demoPop_RacePie")),
                                                 #     column(width = 5,
                                                 #            plotlyOutput("demoPop_Pyramid"))
                                                 #   ),
                                                 #   fluidRow(
                                                 #     column(width = 1),
                                                 #     column(width = 10, plotlyOutput("demoPop_RaceAge")),
                                                 #     column(width = 1)
                                                 #   )
                                                 # )

                                                 # fluidRow(
                                                 #   column(width = 7,
                                                 #          style = 'padding:0px; height:49vh',
                                                 #          plotlyOutput("demoPop_RacePie",
                                                 #                       height = '49vh')),
                                                 # 
                                                 #   column(width = 5,
                                                 #          style = 'padding:0px; height:49vh',
                                                 #          plotlyOutput("demoPop_Pyramid",
                                                 #                       height = '49vh'))
                                                 #   #column(width = 1)
                                                 # 
                                                 # ),
                                                 #
                                                 # fluidRow(
                                                 #   column(width = 1, style = 'padding:0px; height:37vh; background-color:#ffffff'),
                                                 #   column(width = 10,
                                                 #          style = 'padding:0px; height:37vh',
                                                 #          plotlyOutput("demoPop_RaceAge",
                                                 #                       height = '37vh')),
                                                 # 
                                                 #   column(width = 1, style = 'padding:0px; height:37vh; background-color:#ffffff')
                                                 #   )
                                                 
                                        )
                            )
      
                   ),
                   
                   
                   
                   tabPanel(title = strong("DATA TABLE"), value = "dataTableTab",
                            DT::dataTableOutput("rankCauseT")   
                          
                   ),
                   
                   
                   
                   
                
                   
                   tabPanel(title = strong("ABOUT"), value = "about", #changed from abouts to about
                            tabsetPanel(type="tab", id="aboutID",
                                        
                                        tabPanel(title = "OVERVIEW", value = "overviewTab",
                                                 br(), 
                                                 includeMarkdown("About.md")
                                        ),
                                        tabPanel(title = "Technical Documentation", value = "techDocTab",
                                                 br(), 
                                                 includeMarkdown("technical.md")
                                        ),
                                        tabPanel(title = "Links to Other Data", value = "otherLinksTab",
                                                 br(), 
                                                 includeMarkdown("ourLinks.md")
                                        ), 
                                        tabPanel(title = "CCB URL Parameters", value = "urlParametersTab",
                                                 br(), 
                                                 includeMarkdown("url_parameters.md")
                                        )
                            )
                   )
                   
                  

        ) # END navbarPage
      ) # END dashboardBody
    ) # END dashboardPage
  ) # END fluidPage
  )# END ShinyUI
  
  
  
  
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
  