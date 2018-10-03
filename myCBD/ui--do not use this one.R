STATE <- "CALIFORNIA"   # needed this here with CDPH Shiny Server but not otherwise?


# funtion used as "short-cut" when making criteria for conditionals below
fC <- function(vec) {
  tRep <- length(vec)-1
  paste("input.ID == ",vec,    c(rep("|",tRep),""), collapse="")
}

myButtonSty     <- "height:22px; padding-top:0px; margin-top:-5px; float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"
myHelpButtonSty <- "height:12px;padding-top:0px; margin-top:-5px"

#-----------------------------------------------------------------------------------------------------------------------------

shinyUI(fluidPage(theme = "bootstrap.css",
                #  tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),   # removes ticks between years
                  tags$h3(mTitle),                                                       # title supplied from Global
  
sidebarPanel( 
 
  conditionalPanel(condition = fC(c(22)), actionButton("mapTabHelp", "Map Tab Help"),style=myHelpButtonSty),hr(),
                   
   
  
 conditionalPanel(condition = fC(c(22,23,44,55,66)),    actionButton("causeHelp", "?",style=myButtonSty) ,
                                                        selectInput("myCAUSE", "Cause:", choices=causeNum36, selected="A")),  # size=30 selectize = F, size=3,
 conditionalPanel(condition = fC(c(22,23,44)),          checkboxInput("cZoom","Zoom to County",value=FALSE)),
 
 conditionalPanel(condition =  paste(
                               "((",fC(c(33,34,45,55)),") |",
                                "(input.cZoom && (",fC(c(22,23,44)),")))" 
                               ), 
                                                        selectInput("myLHJ","County:",choices=lList,selected=STATE)  ),
 
 conditionalPanel(condition = fC(c(22,23,66)),          selectInput("myGeo","Geographic Level:",choices=c("County","Community","Census Tract"))),

 conditionalPanel(condition =paste(
                             "(!(input.myGeo == 'Community' | input.myGeo == 'Census Tract') && (", fC(c(22,23)),") ) 
                               | (", fC(c(33,34,45,44)),")"  
                             ),
                                                        sliderInput("myYear","Year:",value=2017,min=2001,max=2017,animate = TRUE,round=TRUE,sep="",step=1)  ),

 conditionalPanel(condition = fC(c(22,23,33,45,44,66)), radioButtons( "mySex",      "Sex:", choices=c("Total","Female","Male"))),
 
 conditionalPanel(condition = fC(c(33)),                checkboxGroupInput("myLev", "Levels to show:",c("Top Level" = "lev1","Public Health" = "lev2","Detail" = "lev3"),"lev1")),
 conditionalPanel(condition = fC(c(22,23)),             checkboxInput("myStateCut", "State-based cutpoints", value=TRUE)),
 conditionalPanel(condition = fC(c(33,34)),                numericInput( "myN",        "How Many:", value=10,min=1,max=50)),
 conditionalPanel(condition = fC(c(22,23,34,44,55,66)),    actionButton( "measureHelp", "?",style=myButtonSty) ,
                                                       #selectInput(  "myMeasure",  "Measure:", choices=lMeasures,selected="YLLper")),
                                                        radioButtons(  "myMeasure",  "Measure:", choices=lMeasures,selected="YLLper")),
 conditionalPanel(condition = fC(c(33)),                actionButton( "measureHelp", "?",style=myButtonSty) ,
                                                        selectInput(  "myMeasureShort",  "Measure Sort Order:", choices=lMeasuresShort)),
 conditionalPanel(condition = fC(c(22,23)),             radioButtons( "myCutSystem","Cut-point method:", choices=c("quantile","fisher"))),   # pretty
 conditionalPanel(condition = fC(c(23)),                checkboxInput("myLabName",  "Place Names", value=FALSE)),
 conditionalPanel(condition = fC(c(44)),                checkboxInput("myCI",       "95% CIs?", value=TRUE)),
 conditionalPanel(condition = fC(c(66)),                selectInput(  "myX",        "SDOH Variable:", choices=sdohVec)),

 hr(), 
 
 # TEXT on bottom of SIDEBAR
 
 conditionalPanel(condition = "input.ID !=  11 ", 
                  br(),
                  helpText("DEFINITIONS",h6("YLL:  Years of Life Lost"),style="color:black"),br()
                  ),
 
 conditionalPanel(condition = fC(c(11)), 
                  
 helpText(textIntroA,style="color:black"), br(),
 #helpText(textIntroB,style="color:black"), br(),
 helpText(textIntroC,style="color:black"), br(),
 
 
 if (whichData == "real") { helpText(textNote.real,style="color:black")},
 if (whichData == "fake") { helpText(textNote.fake,style="color:red")},
 
 br(),
 
 
 tags$br(),
 icon("envelope-o"),tags$a(href = "mailto:michael.samuel@cdph.ca.gov","Find a bug or have a question?"),
 tags$br(), tags$a("Developed in R-Shiny"),
 helpText(tags$a(href="https://github.com/mcSamuelDataSci/CACommunityBurden/wiki/Technical-Documentation",
                 ("Technical Documentation (on GitHub)"))),
   br(),
   HTML('<center><img src="cdph2.gif" height="85" width="100"></center>'),
   #img(src='cdph2.gif',width = "100px", height = "85px", align = "center")  # , align = "center",
   helpText(paste("Version:",version),style="color:black")
 )
),
 

# https://stackoverflow.com/questions/35025145/background-color-of-tabs-in-shiny-tabpanel
# works: h5("Home Page",style="color:red")

mainPanel(
  hr(), 
  tabsetPanel(type = "tab",id="ID",
              
              
          #  each image ID and style
          # shiny jj
          
   tabPanel("Home Page",  br(),align='center',
            fluidRow(column(width=4,
              img(src="screen shots.jpg"),
              img(src="screen shots.jpg",height="100%",width="100%"),

              
            )),
              column(width=4,
                   img(src="screen shots.jpg",height="100%",width="100%"),
                   img(src="screen shots.jpg",height="100%",width="100%"),
                   
                   
            ))
            ,   value = 11),          
  
   
   
   
   
   
   
   tabPanel("Map - Interactive",         htmlOutput(      "map_title"                      ),
                                         leafletOutput(   "cbdMapTL",  width=700,height=700),  value = 22),
   tabPanel("Map - Static",              plotOutput(      "cbdMapTS",  height=700,width="100%"),  value = 23),
   tabPanel("Rank Conditions",               plotOutput(      "rankCause", width="100%",height=700),  value = 33),
   tabPanel("Rank Conditions Table",         dataTableOutput( "rankCauseT"                     ),  value = 45),   #DT::
   tabPanel("Rank Conditions by Sex",     plotOutput(      "rankCauseSex", width="100%",height=700),  value = 34),
   
   tabPanel("Rank Counties/Communities", plotOutput(      "rankGeo",   width="100%",height=1700), value = 44),
   tabPanel("Trend",                     plotOutput(      "trend",     width="100%",height=700),  value = 55),
   tabPanel("SDOH Associations",        plotlyOutput(    "scatter",             height=700),  value = 66),
   tabPanel("Technical",              includeMarkdown("technical.md"),value = 77)
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






