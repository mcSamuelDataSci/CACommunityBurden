STATE <- "CALIFORNIA"   # needed this here with CDPH Shiny Server but not otherwise?

#https://stackoverflow.com/questions/40513153/shiny-extra-white-space-in-selectinput-choice-display-label

n1 <- textIntro1

if (whichData == "real") {n2 <- textIntro2.real}
if (whichData == "fake") {n2 <- textIntro2.fake}

fC <- function(vec) {
  tRep <- length(vec)-1
  paste("input.ID == ",vec,    c(rep("|",tRep),""), collapse="")
}

#library(shinythemes)
shinyUI(fluidPage(theme = "bootstrap.css",
                  #shinythemes::themeSelector(),
                  tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
                  # removes ticks between years
                  # https://stackoverflow.com/questions/44474099/removing-hiding-minor-ticks-of-a-sliderinput-in-shiny
                  tags$h3(mTitle),
  
# wellPanel
# navBarPanel 

sidebarPanel( 
 
  # https://shiny.rstudio.com/reference/shiny/latest/selectInput.html
  # https://shiny.rstudio.com/articles/selectize.html
  # https://www.w3schools.com/html/html_form_elements.asp
 #  https://www.w3schools.com/css/css3_buttons.asp
  
 conditionalPanel(condition = fC(c(33,34,6,8,9)),   
                               actionButton("causeHelp1", "?",style=" height:22px; padding-top:0px; margin-top:-5px; float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4") ,
               # selectizeInput("myCAUSE", "Cause:", choices=causeNum36, selected="A",options = list(maxOptions = 10000),width='50%')),# size=30 selectize = F, size=3,
 #width:100px;
 selectInput("myCAUSE", "Cause:", choices=causeNum36, selected="A")),# size=30 selectize = F, size=3,

 conditionalPanel(condition = fC(c(6,33,34)),
                 # actionButton("causeHelp", "?",style=" height:22px; padding-top:0px; margin-top:-5px; float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4") ,
                   checkboxInput("cZoom","Zoom to County",value=FALSE)
                  ),
 
 conditionalPanel(condition =  paste(
                               "((",fC(c(2,4,5,55,8)),") |",
                                "(input.cZoom && (",fC(c(33,34,6)),")))" 
                               ), 
                  selectInput("myLHJ","County:",choices=lList,selected=STATE)
                  ),
 
 conditionalPanel(condition = fC(c(33,34,9)),
                  selectInput("myGeo","Geographic Level:",choices=c("County","Community","Census Tract"))
                  ),

 conditionalPanel(condition =paste(
                             "(!(input.myGeo == 'Community' | input.myGeo == 'Census Tract') && (", fC(c(33,34)),") ) 
                               | (", fC(c(5,55,6)),")"
                             ),
                  sliderInput("myYear","Year:",value=2015,min=2001,max=2015,animate = TRUE,round=TRUE,sep="",step=1)
                  ),

 conditionalPanel(condition = fC(c(33,34,5,55,6,7,9)),  radioButtons( "mySex",      "Sex:", choices=c("Total","Female","Male"))),
 conditionalPanel(condition = fC(c(33,34)),              checkboxInput("myStateCut", "State-based cutpoints", value=TRUE)),
 conditionalPanel(condition = fC(c(2,4,5,7)),                numericInput( "myN",        "How Many:", value=10,min=1,max=50)),
 conditionalPanel(condition = fC(c(33,34,5,6,7,8,9)), 
                  actionButton("measureHelp", "?",style=" height:22px; padding-top:0px; margin-top:-5px; float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4") ,
                  selectInput(  "myMeasure",  "Measure:", choices=lMeasures,selected="YLLper")),
 
 
 conditionalPanel(condition = fC(c(33,34)),                radioButtons( "myCutSystem","Cut-point method:", choices=c("quantile","fisher"))),   # pretty
 conditionalPanel(condition = fC(c(34)),                      checkboxInput("myLabName",  "Place Names", value=FALSE)),
 conditionalPanel(condition = fC(c(6)),                      checkboxInput("myCI",       "95% CIs?", value=TRUE)),
 conditionalPanel(condition = fC(c(9)),                      selectInput(  "myX",        "SDOH Variable:", choices=sdohVec)),

 hr(), 
 conditionalPanel(condition = fC(c(10)), 
 helpText(n1,style="color:black"), br(),
 helpText(n2,style="color:black"),br(),
 
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
   tabPanel("Home Page",  br(),align='center',img(src="logo1.png",height="50%",width="50%"),   value =  10),            #textOutput("HomeText"                             )
  
   tabPanel("Map (tmap Leaf)",   htmlOutput(      "map_title"                      ),
                                 leafletOutput(   "cbdMapTL",   width=700,height=700),   value = 33),
   tabPanel("Map (tmap Stat)",   plotOutput(      "cbdMapTS",   width=700,height=700),   value = 34),
   
   tabPanel("Rank Causes",       plotOutput(      "rankCause", width=700,height=700),   value =  5),
   tabPanel("Rank Causes Table", dataTableOutput( "rankCauseT"                     ),   value = 55),   #DT::
   tabPanel("Rank Counties/Communities",  plotOutput(      "rankGeo",    width=700,height=1700),  value =  6),
   tabPanel("Trend",             plotOutput(      "trend",      width=700,height=700),  value =  8),
   tabPanel("SES Burden Scatter",plotlyOutput(    "scatter",              height=700),  value =  9)
  )       ) 
 
))


#tabPanel("Map (static)",      plotOutput(      "cbdMap1",   width=700,height=700),   value =  3),
#tabPanel("Map (interactive)", 
#htmlOutput(      "map_title"                      ),
#                             leafletOutput(   "cbdMap0",             height=700),   value =  1),


#junk i think

