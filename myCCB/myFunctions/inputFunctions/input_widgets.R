# ==============================================================
# Sidebar inputs live here. (functions for Showing & Hiding them are in input_functions.R)
# Jonah Golden, October 8 2019
# ==============================================================



# Input Widgets ================================================
hidden(
  div(id = "inputs",
        
      # myCAUSE ======================
      dottedSelectInput("myCAUSE", label=list("Cause of Death:", actionButton(inputId="causeHelp", label=helpIcon, style=myInputHelpButtonSty)), choices=fullList),
      
      # myLHJ ======================
      selectInput("myLHJ","County/State:",choices=lList,selected=STATE),
      
      # myLHJ w/ cities and HDs ======================
      # selectInput("myLHJplus","LHJ/State:",choices=lList_lhj,selected=STATE),
      
      # myGeo ======================
      selectInput("myGeo","Geographic Level:", choices=c("County","Community","Census Tract")),
      # helpText
      div(id = "myGeoHelpText", helpText(h6(appTextL$tractWarning,style="color:red; float:left; margin: 20px;"))),
      
      selectInput("myGeo_sdoh","Geographic Level:", choices=c("County","Community")),
      
      # myYear ======================
      sliderInput("myYear","Year:",value=maxYear,min=2001,max=maxYear,animate = TRUE,
                  round=TRUE,sep="",step=1),  #can use value=c(2017,2017)
      
      # myYear ======================
      sliderInput("myYear_mcod","Year:",value=maxYear,min=2019,max=maxYear,animate = TRUE,
                  round=TRUE,sep="",step=1),
      
      # myYearDemo ======================
      sliderInput("myYearDemo","Year:",value=maxYear,min=2000,max=maxYear,animate = TRUE,
                  round=TRUE,sep="",step=1),
      
      # myYear - Life Course ==============
      # Shuo Life Course
      sliderTextInput(inputId = "myYear_lifeCourse", label = "Year(s):", choices = as.character(minYear:maxYear), selected = as.character(maxYear)),
      
      ## chinwag --
      radioButtons("myYearGrouping_lifeCourse", label = "Years to Group:", choices = c(1, 5), inline = TRUE),
      
      sliderTextInput(inputId = "myYear_lifeCourseNew", label = "Year:", choices = as.character(unique(datLifeCourse1y$year)), selected = max(datLifeCourse1y$year)),

      radioButtons("myMeasure_lifeCourse", label = "Measure", choices = c("Crude Death Rate", "Ranking"), selected = "Ranking"),
      
      radioButtons("bySex", label = "Disaggregated by Sex", choices = c("Yes", "No"), selected = "No"),
      
      selectInput("myRace_lifeCourse", label = "Race and Ethnicity", choices = raceList, selected = "Total"),
      
      pickerInput("myRaceMulti_lifeCourse", label = "Race and Ethnicity (Multi-Select)", options = list(`actions-box` = TRUE),
                  choices = raceList, selected = c("Black", "White"), multiple = TRUE),
      
      pickerInput("myAge_lifeCourse", label = "Age Group (Multi-Select)", options = list(`actions-box` = TRUE),
                  choices = ageSort, selected = c("55 - 64", "65 - 74"), multiple = TRUE),
      
      sliderInput("myN_lifeCourseNew", label = "Number of Causes", min = 1, max = 10, value = 5),
      
      checkboxInput("myLogTrans_lifeCourse",  label = "Log Transform of Y Axis for Crude Death Rate", value=TRUE),
      
      checkboxInput("myLabelRepel", label = "Avoid Label Overlap for Crude Death Rate", value=TRUE),
      
      ## -- chinwag
      
      # mySex ======================
      radioButtons("mySex", "Sex:", choices=c("Total","Female","Male"), inline=TRUE),
      
      # myLev ======================
      radioButtons("myLev", label=list("Levels to show:", actionButton("levelHelp", label=helpIcon, style=myInputHelpButtonSty)),
                   choices=c("Top" = "lev1","Public Health" = "lev2","Detail" = "lev3"), inline=TRUE, selected = 'lev2'),
      
      radioButtons("myLevShort", label=list("Levels to show:", actionButton("levelShortHelp", label=helpIcon, style=myInputHelpButtonSty)),
                   choices=c("Top" = "lev1","Public Health" = "lev2"), inline=TRUE, selected = 'lev2'),
      
      # Broad groups - top Trends ========================
      checkboxGroupButtons( "myBroadGroups",
                            label = list("Select one or more broad condition group:", actionButton(inputId="broadGroupHelp", label=helpIcon, style=myInputHelpButtonSty_broadGroup)),
                            choices = c("All" = "0", "Communicable" = "A", "Cancer" = "B", "Cardiovascular" = "C", "Other Chronic" = "D", "Injury" = "E", "Perinatal" = "P"),
                            selected = c("A", "B", "C", "D", "E", "P"), individual=TRUE, size="sm"),
      
      # myStateCut ======================
      # add br(), here to fix spacing, but does not yet....
      checkboxInput("myStateCut", label=list("State-based cutpoints", actionButton("stateCutHelp", label=helpIcon, style=myInputHelpButtonSty)),
                   value=TRUE),
      
      # IHME ===================
      selectInput("ihmeGroup", label = "Display by:", choices = c("Sex", "Age Group", "Year")),
      
      sliderInput("level", label = "Level:", min = 1, max = 3, value = 2),
      
      radioButtons("display", label = "Display:", choices = c("Cause", "Risk"), inline=TRUE),
      
      selectInput("year", label = "Year:", choices = ihmeYears, selected = max(ihmeYears)),
      
      radioButtons("metric",label = "Metric:", choices = sort(unique(ihmeLink$metric$metric_name)), selected = "Rate", inline=TRUE),
      
      selectInput("measure", label = "Measure:", choices = c("YLDs (Years Lived with Disability)", 
                                                             "DALYs (Disability-Adjusted Life Years)", 
                                                             "Prevalence")),
      # myScale input
      # "myN_lifeCourse" input
      
      
      # myN ======================
      numericInput( "myN",  "How Many:", value=10,min=1,max= 50),
      
      # myN Broad - Top Trends ======================
      numericInput( "myN_topTrends",  "How many conditions:", value=5,min=1,max= 50),
      
      # myN - Life Course ==========================
      selectInput("myN_lifeCourse", "How many conditions:", c(5, 10, 15), selected = 5),
      
      # Multiple Lenses (Burden) - How Many Shown? ======================
      selectInput("myObserv", "How Many:", choices = list("How many shown?" = c(5,10,15,20)), selected=10),
      
      # myMeasure--uses deathMeasures_Dropdown because the function uses short names in it (?)
      selectInput("myMeasure",  label=list("Measure:", actionButton( "measureHelp", label=helpIcon,style=myInputHelpButtonSty)),
                  choices=deathMeasures_Dropdown, selected = "aRate"), 
      
      # myMeasureShort ======================
      # selectInput("myMeasureShort",  "Measure Sort Order:",  
      #             choices=dMNames_short, selected="Age-Adjusted Death Rate"),
      
      selectInput("myMeasureShort",  "Measure Sort Order:",  
                  choices=deathMeasuresShort_Dropdown, selected= "aRate"),
      
      radioButtons("myMeanAge_sort",  'Sort "Mean Age at Death" from:',  
                  choices=mean_age_sort),
      
      # selectInput("myMeasureShort",  list(label="Measure Sort Order:", actionButton("measureHelp", label=helpIcon,style=myInputHelpButtonSty)),  
      #             choices=dMNames_short, selected="Age-Adjusted Death Rate"),
      
     
      
      # myYearGrouping ======================
      radioButtons("myYearGrouping", "Years to Group:", choices=c(1,3,5), inline = TRUE),
      
      radioButtons("myYearGrouping_race_age", "Years to Group:", choices=c(1,3), inline = TRUE),
      
      #radioButtons("myYearGrouping_lifeCourse", "Years to Group:", choices=c(1,3), inline = TRUE),
   
      # myData ======================
      selectInput("myData","Data Type:", choices=c("Deaths", "Hospitalizations","Emergency Department")),  
      
      # myStrata ======================
      selectInput("myStrata","Grouping Variable:", choices=c("Age Group", "Race/Ethnicity")),  
      
      
      # mySort ==============================
      selectInput( "mySort", "Sort by?",
                            choices = raceSort,
                            selected = "White"),
      
      
      # myOlderFocus ===========================
      checkboxInput("myOlderFocus", label="Older Adult Focus",value=FALSE),
      
      
      # myLiveborn =============================
      checkboxInput("myLiveborn", list(label="Include Births", actionButton("includeBirthsHelp", label=helpIcon, style=myInputHelpButtonSty)),
                    value=FALSE),
      
      # myMeasureAgeRaceFocus ======================
      selectInput("myMeasureAgeRaceFocus",  list(label="Measure:", actionButton("measureAgeRaceFocusHelp", label=helpIcon, style=myInputHelpButtonSty)),
                  choices=c("Number"="N","Crude Rate"="cRate","Adjusted Rate"="aRate"), selected="cRate"),
      
      # myScale ======================
      selectInput("myScale",  label= list("Scale:", actionButton("axisScaleHelp", label=helpIcon,style=myInputHelpButtonSty)),
      choices=c("fixed","free")),
      
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
      
      # Year to rank by - top Trends ================================
      sliderInput( "myYearRank", label = "Leading causes in which year?:", value = maxYear, min = minYear, max = maxYear,
                   round = TRUE, sep = "", step = 1),
      
      # Year range - top Trends ================================
      sliderInput( "myYearRange", label = "Year range To display:", min = minYear, max = maxYear, value = c(minYear, maxYear), sep = "", step = 1),
      
      # myMultiRace ======================
      checkboxInput("myMultiRace",  "Include Multirace Line", value=FALSE),
      # helpText
      div(id = "myMultiRaceHelpText", helpText(h6(multiRaceWarning,style="color:red; float:left; margin: 20px;"))),
      
      # myCompare ======================
      radioButtons("myCompare", list(label = "Compare to group with:", actionButton("disparityCompareHelp", label = helpIcon, style = myInputHelpButtonSty)), 
                   choices=c("lowest rate","highest rate")),
      
      # myAddN ======================
      checkboxInput("myAddN",  "Show NUMBER of Deaths?", value=FALSE),
  
      # myAddRate ======================
      checkboxInput("myAddRate",  "Show Rate?", value=FALSE),      
      
      # myAddRR ======================
      checkboxInput("myAddRR",  "Show Rate Ratio?", value=FALSE),
    
     
      # myLifeRace ===================
      checkboxInput("myLifeRace",  "Show Race/Ethnicity Detail?", value=FALSE),
      
      
      # myRace ==========================
      checkboxGroupButtons( "myRace", "Which Race/Ethnic Groups?",
            choices = raceList,
            selected = raceList[!raceList %in% c("Multi-Race","Total","NH/PI","AI/AN")],individual=TRUE,size="sm"),
      
      # mySexMult ==========================
      checkboxGroupButtons( "mySexMult", "Which Sex Groups?",
                         choices = c("Total", "Male", "Female"),
                         selected = c("Male", "Female"), individual=TRUE,size="sm"),
      
      
      
      # myX ======================
      selectInput("myX", "Social Determinant of Health Variable:", choices=sdohVec, selected="pov"),
      
      # myOSHPDtype ======================
      selectInput( "myOSHPDtype", "Measure Sort Order:", choices = hMNames_short),
      
      # myOSHPDtype-mdcdrg ======================
      selectInput( "myOSHPDtype_mdcdrg", "Measure Sort Order:", choices = hMDCDrop_down),
      
      # myPosition ======================
      selectInput( "myPosition", "Sort Order:", choices = listPosition),
      
      # myVar(ICD/MDC/DRG) ======================
      # selectInput("myVar", label=list("Variable:",  actionButton( "dxGroupsHelp", label=helpIcon,style=myInputHelpButtonSty)),
      #            choice = MDC_DRG_ICD_Dropdown),
      
      
      
      # myPrimetype ======================
      selectInput("myprimetype", "Variable", choice = c("any", "primary")),
      
      # IHME inputs ======================
      # sliderInput( "level", label = "Level:", min = 0, max = 4, value = 2),

      # sliderInput( "year", label = "Year:", min = min(VALID_YEARS), max = max(VALID_YEARS), value = max(VALID_YEARS), sep = "", step = 1), # animate = animationOptiointerval = 3000)
      
      # sliderTextInput( "yearRange", label = "Years:", choices = as.character(VALID_YEARS), selected = range(VALID_YEARS)),  # grid=TRUE
      
      # radioButtons("display",label = "Display:",choices = c("Cause" = "cause", "Risk" = "risk"), inline=TRUE),
      
      # radioButtons("sex", "Sex:", choices=c("Both" = 3,"Female" = 2,"Male" = 1), inline=TRUE),
      
      # radioButtons("metric",label = "Metric:",choices = c("Number" = 1, "Percent" = 2, "Rate" = 3), inline=TRUE),
      
      # selectInput( "measure", label = "Measure:", choices = c("Deaths" = 1,
      #                                                         "Disability Adjusted Life Years (DALYs)" = 2,
      #                                                         "Years Lived with Disability (YLDs)" = 3,
      #                                                         "Years of Life Lost (YLLs)" = 4), selected = 1),
      
      # ========================================
      
      actionBttn(
        inputId = "updateLifeCourse",
        label = "Update Table",
        style = "material-flat", 
        color = "primary",
        icon = icon("table")), 
      
      
      
      div(id="suppressionNote",
          paste('Note: All measures associated with counts <',criticalNumber,', as well as necessary complementrary counts/measures are excluded for data de-identification purposes'),style="color:blue;font-size:12px;padding-left:5px;"
      ),
      
      br(),
      
      div(id="recentYearNote",
          paste('Note: Data for', currentYear, 'are not yet final. Number of deaths are likely to increase slightly.  Some cause of death codes will become more accurate. These changes are not expected to significantly impact the interpretation of any observed noteworthy patterns or trends.'),
          style="color:blue;font-size:12px;padding-left:5px;"
          )
      
      
  )
)

# Home ==================