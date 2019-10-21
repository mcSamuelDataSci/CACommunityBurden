# ==============================================================
# Sidebar inputs live here. (functions for Showing & Hiding them are in input_functions.R)
# Jonah Golden, October 8 2019
# ==============================================================

# Styles for help buttons and boxes ============================

myInputHelpButtonSty <- paste0("width:20px;  color:#fff; background-color:#337ab7; border-color:white; padding:0px; font-size: 18px;",
                               "margin:0px;",
                               "margin-left:10px;",
                               "float:right;"
                               )   #2e6da4
helpIcon <- "?"
helpIcon <- icon("question-circle-o") # or: HTML('<i class="fa fa-question-circle-o"></i>')
#helpIcon <- icon("question-circle")   # or: HTML('<i class="fa fa-question-circle"></i>')  
#helpIcon <- icon("question") 
#helpIcon <- icon("info")

# Input Widgets ================================================
hidden(
  div(id = "inputs",
        
      # myCAUSE ======================
      dottedSelectInput("myCAUSE", label=list("Cause of Death:", actionButton(inputId="causeHelp", label=helpIcon, style=myInputHelpButtonSty)), choices=fullList),
      
      # myLHJ ======================
      selectInput("myLHJ","County/State:",choices=lList,selected=STATE),
      
      # myGeo ======================
      selectInput("myGeo","Geographic Level:", choices=c("County","Community","Census Tract")),
      # helpText
      div(id = "myGeoHelpText", helpText(h6(tractWarning,style="color:red; float:left; margin: 20px;"))),
      
      # myYear ======================
      sliderInput("myYear","Year:",value=maxYear,min=2001,max=maxYear,animate = TRUE,
                  round=TRUE,sep="",step=1),  #can use value=c(2017,2017)
      
      # mySex ======================
      radioButtons("mySex", "Sex:", choices=c("Total","Female","Male"), inline=TRUE),
      
      # myLev ======================
      radioButtons("myLev", label=list("Levels to show:", actionButton("levelHelp", label=helpIcon, style=myInputHelpButtonSty)),
                   choices=c("Top" = "lev1","Public Health" = "lev2","Detail" = "lev3"), inline=TRUE),
      
      # myStateCut ======================
      # add br(), here to fix spacing, but does not yet....
      checkboxInput("myStateCut", label=list("State-based cutpoints", actionButton("stateCutHelp", label=helpIcon, style=myInputHelpButtonSty)),
                   value=TRUE),
      
      # myN ======================
      numericInput( "myN",  "How Many:", value=10,min=1,max= 50),
      
      # myMeasure--uses deathMeasures_Dropdown because the function uses short names in it (?)
      selectInput("myMeasure",  label=list("Measure:", actionButton( "measureHelp", label=helpIcon,style=myInputHelpButtonSty)),
                  choices=deathMeasures_Dropdown),
      
      # myMeasureShort ======================
      selectInput("myMeasureShort",  label=list("Measure Sort Order:", actionButton( "measureHelp", label=helpIcon,style=myInputHelpButtonSty)),
                  choices=dMNames_short, selected="YLL Rate per 100,000 population"),
      
      # myYearGrouping ======================
      radioButtons("myYearGrouping", "Years to Group:", choices=c("One","Three","Five"), inline = TRUE),
      
      # myCutSystem ======================
      
      radioButtons("myCutSystem",label=list("Cut-point method:", actionButton("cutmethodHelp", label=helpIcon,style=myInputHelpButtonSty)),
                   choices=c("quantile","fisher")),  # pretty
      
      # myLabName ======================
      checkboxInput("myLabName",  "Place Names", value=FALSE),
      
      # myCI ======================
      checkboxInput("myCI",       "95% CIs", value=FALSE),
      
      # myRefLine ======================
      checkboxInput("myRefLine",  "Reference Line", value=FALSE),
      
      # myLogTrans ======================
      checkboxInput("myLogTrans",  "Log Transform of Y Axis", value=FALSE),
      
      # myMultiRace ======================
      checkboxInput("myMultiRace",  "Include Multirace Line", value=FALSE),
      # helpText
      div(id = "myMultiRaceHelpText", helpText(h6(multiRaceWarning,style="color:red; float:left; margin: 20px;"))),
      
      # myX ======================
      selectInput("myX", "Social Determinant of Health Variable:", choices=sdohVec),
      
      # myOSHPDtype ======================
      selectInput( "myOSHPDtype", "Measure Sort Order:", choices = hMNames_short),
      
      # myOSHPDtype-mdcdrg ======================
      selectInput( "myOSHPDtype_mdcdrg", "Measure Sort Order:", choices = hMDCDrop_down),
      
      # myVar(ICD/MDC/DRG) ======================
      selectInput("myVar", "Variable:", choice = MDC_DRG_ICD_Dropdown),
      
      # myPrimetype ======================
      selectInput("myprimetype", "Variable", choice = c("any", "primary")),
      
      # IHME inputs ======================
      sliderInput( "level", label = "Level:", min = 0, max = 4, value = 1),
      
      sliderInput( "year", label = "Year:", min = min(VALID_YEARS), max = max(VALID_YEARS), value = max(VALID_YEARS), sep = "", step = 1), # animate = animationOptiointerval = 3000)
      
      sliderTextInput( "yearRange", label = "Years:", choices = as.character(VALID_YEARS), selected = range(VALID_YEARS)),  # grid=TRUE
      
      radioButtons("display",label = "Display:",choices = c("Cause" = "cause", "Risk" = "risk"), inline=TRUE),
      
      radioButtons("sex", "Sex:", choices=c("Both" = 3,"Female" = 2,"Male" = 1), inline=TRUE),
      
      radioButtons("metric",label = "Metric:",choices = c("Number" = 1, "Percent" = 2, "Rate" = 3), inline=TRUE),
      
      selectInput( "measure", label = "Measure:", choices = c("Deaths" = 1,
                                                              "Disability Adjusted Life Years (DALYs)" = 2,
                                                              "Years Lived with Disability (YLDs)" = 3,
                                                              "Years of Life Lost (YLLs)" = 4), selected = 1),
      div(id="suppressionNote",
          paste('Note: All values <',criticalNumber,'including zeros are excluded '),style="color:green;font-weight: bold;"
      ),
      
      div(id="yllNote",
          helpText(helpText('Note: YLL is "Years of Life Lost"',style="color:green;font-weight: bold;"))
      )
  )
)

# Home ==================