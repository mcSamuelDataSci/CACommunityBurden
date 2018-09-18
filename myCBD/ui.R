STATE <- "CALIFORNIA"   # needed this here with CDPH Shiny Server but not otherwise?

# text used later in the UI
n1                           <- textIntro1
if (whichData == "real") {n2 <- textIntro2.real}
if (whichData == "fake") {n2 <- textIntro2.fake}

# funtion used as "short-cut" when making criteria for conditionals below
fC <- function(vec) {
  tRep <- length(vec)-1
  paste("input.ID == ",vec,    c(rep("|",tRep),""), collapse="")
}

myButtonSty <- "height:22px; padding-top:0px; margin-top:-5px; float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"

#-----------------------------------------------------------------------------------------------------------------------------

shinyUI(fluidPage(theme = "bootstrap.css",
                  tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),   # removes ticks between years
                  tags$h3(mTitle),                                                       # title supplied from Global
  
sidebarPanel( 
 
 conditionalPanel(condition = fC(c(22,23,44,55,66)),    actionButton("causeHelp", "?",style=myButtonSty) ,
                                                        selectInput("myCAUSE", "Cause:", choices=causeNum36, selected="A")),  # size=30 selectize = F, size=3,
 conditionalPanel(condition = fC(c(22,23,44)),          checkboxInput("cZoom","Zoom to County",value=FALSE)),
 
 conditionalPanel(condition =  paste(
                               "((",fC(c(33,45,55)),") |",
                                "(input.cZoom && (",fC(c(22,23,44)),")))" 
                               ), 
                                                        selectInput("myLHJ","County:",choices=lList,selected=STATE)  ),
 
 conditionalPanel(condition = fC(c(22,23,66)),          selectInput("myGeo","Geographic Level:",choices=c("County","Community","Census Tract"))),

 conditionalPanel(condition =paste(
                             "(!(input.myGeo == 'Community' | input.myGeo == 'Census Tract') && (", fC(c(22,23)),") ) 
                               | (", fC(c(33,45,44)),")"  
                             ),
                                                        sliderInput("myYear","Year:",value=2015,min=2001,max=2015,animate = TRUE,round=TRUE,sep="",step=1)  ),

 conditionalPanel(condition = fC(c(22,23,33,45,44,66)), radioButtons( "mySex",      "Sex:", choices=c("Total","Female","Male"))),
 conditionalPanel(condition = fC(c(22,23)),             checkboxInput("myStateCut", "State-based cutpoints", value=TRUE)),
 conditionalPanel(condition = fC(c(33)),                numericInput( "myN",        "How Many:", value=10,min=1,max=50)),
 conditionalPanel(condition = fC(c(22,23,33,44,55,66)), actionButton("measureHelp", "?",style=myButtonSty) ,
                                                        selectInput(  "myMeasure",  "Measure:", choices=lMeasures,selected="YLLper")),
 conditionalPanel(condition = fC(c(22,23)),             radioButtons( "myCutSystem","Cut-point method:", choices=c("quantile","fisher"))),   # pretty
 conditionalPanel(condition = fC(c(23)),                checkboxInput("myLabName",  "Place Names", value=FALSE)),
 conditionalPanel(condition = fC(c(44)),                checkboxInput("myCI",       "95% CIs?", value=TRUE)),
 conditionalPanel(condition = fC(c(66)),                selectInput(  "myX",        "SDOH Variable:", choices=sdohVec)),

 hr(), 
 
 # TEXT on bottom of SIDEBAR
 
 conditionalPanel(condition = fC(c(11)), 
                  
 helpText(n1,style="color:black"), br(),
 helpText(n2,style="color:black"), br(),
 
 tags$br(),
 icon("envelope-o"),tags$a(href = "mailto:michael.samuel@cdph.ca.gov","Find a bug or have a question?"),
 tags$br(),
 tags$h6("Developed in R-Shiny"),
 
 helpText("LINKS",tags$a(href="https://www.cdph.ca.gov/Programs/CHSI/Pages/Data-and-Statistics-.aspx", 
                               h6("California Death Data")),
                  tags$a(href="https://data.chhs.ca.gov/",
                               h6("California Health and Human Service Agency Open Data Portal")),
                  tags$a(href="https://www.census.gov/programs-surveys/acs/",
                               h6("American Community Survey")),  style="color:black"),
   br(),
   helpText("DEFINITIONS",
     h6("YLL:  Years of Life Lost"),
     h6("SMR:  Standard Mortality Ratio (Local Rate/State Rate)"),
     h6("Community: Medical Service Study Areas (MSSA)"),
     tags$a(href="https://oshpd.ca.gov/HWDD/MSSA.html",h6("MSSA Info")),
     style="color:black"),
   br(),
   HTML('<center><img src="cdph2.gif" height="85" width="100"></center>'),
   #img(src='cdph2.gif',width = "100px", height = "85px", align = "center")  # , align = "center",
   helpText(paste("Version:",version),style="color:black")
 )
),
 
mainPanel(
  hr(), 
  tabsetPanel(type = "tabs",id="ID",
              
   tabPanel("Home Page",  br(),align='center',img(src="logo1.png",height="50%",width="50%"),   value = 11),          
  
   tabPanel("Map (tmap Leaf)",           htmlOutput(      "map_title"                      ),
                                         leafletOutput(   "cbdMapTL",  width=700,height=700),  value = 22),
   tabPanel("Map (tmap Stat)",           plotOutput(      "cbdMapTS",  width=700,height=700),  value = 23),
   tabPanel("Rank Causes",               plotOutput(      "rankCause", width=700,height=700),  value = 33),
   tabPanel("Rank Counties/Communities", plotOutput(      "rankGeo",   width=700,height=1700), value = 44),
   tabPanel("Rank Causes Table",         dataTableOutput( "rankCauseT"                     ),  value = 45),   #DT::
   tabPanel("Trend",                     plotOutput(      "trend",     width=700,height=700),  value = 55),
   tabPanel("SES Burden Scatter",        plotlyOutput(    "scatter",             height=700),  value = 66)
  )       ) 
 
))

# END -----------------------------------------------------------------------------------------------------------------

# NOTES etc. :
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
# actionButton("causeHelp", "?",style=" height:22px; padding-top:0px; margin-top:-5px; float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4") ,
# selectizeInput("myCAUSE", "Cause:", choices=causeNum36, selected="A",options = list(maxOptions = 10000),width='50%')),# size=30 selectize = F, size=3,
#width:100px;
  # https://shiny.rstudio.com/reference/shiny/latest/selectInput.html
  # https://shiny.rstudio.com/articles/selectize.html
  # https://www.w3schools.com/html/html_form_elements.asp
  #  https://www.w3schools.com/css/css3_buttons.asp


# Junk:

#tabPanel("Map (static)",      plotOutput(      "cbdMap1",   width=700,height=700),   value =  3),
#tabPanel("Map (interactive)", 
#htmlOutput(      "map_title"                      ),
#                             leafletOutput(   "cbdMap0",             height=700),   value =  1),






