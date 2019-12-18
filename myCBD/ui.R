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

# Colors from CDPH logo
BLUE <- "#0079C2"
GREEN <- "#5C8727"
ORANGE <- "#EB6E1F"
BEIGE <- "#F9F5F1"

# # Styles for help buttons and boxes
myTabHelpButtonSty <- "background-color:#694D75; font-size:14px;"
myDownloadButtonSty <- "padding-left: 8px; padding-right: 8px; margin: 5px;"
myBoxSty        <- "cursor:pointer; border: 3px solid blue; padding-right:0px;padding-left:0px;"
mySidebarTextSty <- "float:left; margin: 20px; color:#000000;"


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
                 ),
      
      
      
      ######## Google Analytics Script Start ###############
      HTML(
        "<script async src='https://www.googletagmanager.com/gtag/js?id=UA-154687382-1'></script>"
      ),
      includeScript("googleAnalytics.js"),
      ######## Google Analytics Script End ###############
      
      
      
      
      
      
      
      
      
      
      ),
    
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
      dashboardHeader(title = mTitle, titleWidth = 550,
                      tags$li(a(href = 'http://www.company.com',
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
                         div(id = "trendDownloads", style = "margin: 10px;",
                             downloadButton('trendPNG', 'Download Figure', style = paste0("float: left;", myDownloadButtonSty)),
                             downloadButton('trendData', 'Download Data', style = paste0("float: right;", myDownloadButtonSty))
                         ),
                         div(id = "rankCauseDownloads", style = "margin: 10px;",
                             downloadButton('rankCauseFigure', 'Download Figure', style = myDownloadButtonSty)
                         ),
                         div(id = "disparityDownloads", style = "margin: 10px;",
                             downloadButton('disparityPNG', 'Download Figure', style = paste0("float: left;", myDownloadButtonSty))
                         )
                       ),

                       # Side bar text -------------------------------------------------------------
                       
                       div(id = "textHomeTab", style = mySidebarTextSty,
                           HTML('<left><img src="CDPH.gif" height="125" width="150"></left>'),  # 85  100
                           br(),br(),
                           
                           helpText(h4("Welcome to the Preview Version of the CCB!"),style=paste0("color:", GREEN),align="left"), br(),
                           
                           h5(tags$a(href="CA_Health_Views.pdf","SEE CCB DATA IN ACTION, in the new 'Measuring Health Status in California'")), br(),
                           
                           actionButton("newsUse","News and Updates",style=myTabHelpButtonSty), br(),
                           h5(tags$a(href="https://www.surveymonkey.com/r/2N2JSTV","Report 'bugs' HERE!")),
                           h5(tags$a(href="https://www.surveymonkey.com/r/ZH9LSR8","Share your feedback HERE!")),
                           textIntroA, br(), br(), textIntroC, br(), br(),
                           #helpText(textIntroA,style="color:black"), br(),
                           #helpText(textIntroC,style="color:black"), br(),
                           if (whichData == "real") { helpText(textNote.real,style="color:black")},
                           if (whichData == "fake") { helpText(textNote.fake,style="color:red")},
                           br(),br(),
                           icon("envelope-o"),tags$a(href = "mailto:michael.samuel@cdph.ca.gov","Questions?  Want to Help?"), br(),
                                                      tags$a(href="https://shiny.rstudio.com/","Developed in R-Shiny"), br(),
                           tags$a(href="https://github.com/mcSamuelDataSci/CACommunityBurden","GitHub Site")
                       ),
                       
                       div(id = "textNotHomeTab", style = mySidebarTextSty,
                           br(),HTML('<center><img src="CDPH.gif" height="125" width="150"></center>'),
                           br(),
                           br(),HTML('<center><img src="Fusion Center Knot.png" height="125" width="150"></center>')
                       ),
                       
                       # Home page 0 side bar text. condition = fC(c(10)),  Not sure when this comes up?
                       hidden(div(id = "homeTab0",
                                  helpText(h2("which CONDITIONS result in the largest (by what MEASURE) BURDEN of MORTALITY (death) and MORBIDITY (cases, disabiltity, hospitalization) in which POPULATIONS (sex, race/ethnicity, age) in which COMMUNITIES in California at what TIME (years)?",
                                              style="color:red"),h1("WHY?",style="color:blue")),h2("WHAT TO DO ABOUT IT?")
                       )),
                       
                       # Text on all side bars ------------------------------------------------------
                       div(id = "textAllTabs", style = mySidebarTextSty,
                           helpText(br(),h4(VERSION),style=paste0("color:", GREEN))
                       )
                       
                       
      ), # -- END of dashboard Sidebar-------------------------------------------------------
      
      # MAIN PANNELS-------------------------------------------------------------------------
      dashboardBody(
        useShinyjs(),
        navbarPage(title="", id = "navsID", # collapsible = TRUE,
                   
                   tabPanel(title = strong("HOME"), value = "home",
                            br(),align='center',
                            h4(HTML(above1),align="left"),
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
                                        
                                        tabPanel(title = "Sex Trend", value = "trendTab",
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
                                        
                                        tabPanel(title = "Education Trend", value = "educationTrendTab",
                                                 br(),
                                                 plotOutput("trendEducation", width="100%",height=700)
                                        ),
                                        tabPanel(title = "LIFE EXPECTANCY", value = "lifeExpectancyTab",
                                                 br(),
                                                 plotOutput("lifeTable", width="100%",height = 700)
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
                   
                   
                   
                   tabPanel(title = strong("SDOH"), value = "sdoh",
                            tabsetPanel(type = "tab", id = "sdohID",
                                        
                                        tabPanel(title = "SOCIAL DETERMINANTS", value = "sdohTab",
                                                 br(),
                                                 plotlyOutput("scatter", height=700)
                                        )
                            )
                   ),
                   
                   
                   
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
                   
                   
                   
                   tabPanel(title = strong("DATA TABLE"), value = "dataTableTab",
                            dataTableOutput("rankCauseT")   #DT::
                   ),
                   
                   
                   
                   
                
                   
                   tabPanel(title = strong("ABOUT"), value = "abouts",
                            tabsetPanel(type="tab", id="aboutsID",
                                        
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
  