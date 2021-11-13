# =============================================================================
# "server.R" file     
#
# required file for Shiny Application
#
# renders all visual object (maps, charts, help boxes)
# modifies inputs through interativity 
#  
# Michael Samuel
# 2018
#
# =============================================================================

# Load functions that determine what to show based on current tab selection
source(paste0(myPlace,"/myFunctions/inputFunctions/input_functions.R"))

shinyServer(function(input, output,session) {


  # Store current navID (big tabs) and tabID (subtabs) for use throughout Server
  current <- reactiveValues()
  
  # Update current nav and tab values any time tab selection changes
  observe({
    current$nav = input$navsID
    current$tab <- switch(current$nav,
                          "maps"             = input$mapsID,
                          "ranks"            = input$ranksID,
                          "trends"           = input$trendsID,
                          "disparities"      = input$disparitiesID,
                          "hospitalizations" = input$hospitalizationsID,
                          "sdoh"             = input$sdohID,
                          "demographics"     = input$demographicsID, # JASPO added demographics
                          current$nav)
    print(current$tab) # For debugging only
    print(current$nav) # For debugging only
    
    updateTabItems(session, "plotsMenuItems", "tabInputs")  # Always set menu to tabInputs, not tabInfo
  })
  
 
  observe({
    if (current$nav %in% c("home", "about")) {
      hideAllInputs()
      hide("plotsMenu")
      show("textHomeTab")
    } else {
      show("plotsMenu")
      show("textNotHomeTab")
      updateInputsOnTabId(current$tab, input$myGeo, input$myLHJ, input$myMeasure, input$myMultiRace)  #MCS?
      hide("textHomeTab")
    }
  })

  # showModal(
  #   modalDialog(
  #     title = HTML("<h3><center>We are looking for feedback from users like you!</center></h3>"),
  #     HTML("<ul>
  #      <li>What you are using the information on the site to do?</li>
  #          <li>What do you like? What don't you like?</li>
  #          <li>What is missing that you'd like to see?</li></ul> <h1> <center><a href='mailto:michael.samuel@cdph.ca.gov?subject=CCHVIz feedback'>Email us to share your ideas</a></h1>"),
  #     footer = NULL,
  #     easyClose = T
  #     # ,
  #     # footer = modalButton("Close")
  #   )
  # )
 

  
## TEMP FIX...... -------------------------------------------------------  
  
  
  observeEvent(current$tab,{
    if(current$tab %in% c("ageTrendTab") )
    {updateSelectInput(session, "myMeasure", choices = deathMeasures_Dropdown_noADJ )}
  })
  
  
  observeEvent(current$tab,{
    if(!(current$tab %in% c("ageTrendTab")) )
    {updateSelectInput(session, "myMeasure", choices = deathMeasures_Dropdown,selected = "aRate" )}
  })
  
  
  
## TEMP FIX...... -------------------------------------------------------  


# Navigation from home tab Images -------------------------------------------------------------------------------

# "onclick" is a function from shinyjs package
# used here to move to specified tab when specified image in clicked
# first perameter is id associated with images from home page main pannel
#  see ui.R  "tabPanel("Home Page"..."

  # Function to update panel selection (or do anything else..) on img click
updatePanels <- function(navsID, tabID="") {
  updateTabsetPanel(session, inputId="navsID", selected = navsID)                # navigate to upper level tab
  updateTabsetPanel(session, inputId=paste0(navsID, "ID"),   selected=tabID)     #  then to sub-tab if there is one
}

onclick("mapI",       updatePanels(navsID = "maps",             tabID = "interactiveMapTab"))
onclick("rankcauseI", updatePanels(navsID = "ranks",            tabID = "rankByCauseTab"))
onclick("rankgeoI",   updatePanels(navsID = "ranks",            tabID = "rankByGeographyTab"))
onclick("trendI",     updatePanels(navsID = "trends",           tabID = "trendTab"))
onclick("sdohI",      updatePanels(navsID = "sdoh",             tabID = "socialDeterminantsTab"))
onclick("dispI",      updatePanels(navsID = "disparities",      tabID = "disparitiesTab"))
onclick("sdohI",      updatePanels(navsID = "sdoh",             tabID = "sdohTab"))
onclick("hospI",      updatePanels(navsID = "hospitalizations", tabID = "hospitalizations"))
onclick("riskI",      updatePanels(navsID = "ranks",            tabID = "riskByCauseTab"))

  
# Tab help display -------------------------------------------------------------------------------

# function used below as "shortcut" for formating each Modal
myModal <- function(whatInfo) {
  showModal(modalDialog(HTML(whatInfo),  
            easyClose = TRUE,
            #footer = modalButton("Close")
            footer = NULL
))}

# Modal pop-up whenever tab is navigated to OR first time tab is navigated to:

# Option 1 ---
#observeEvent(req(current$tab == "hospitalDischargeTab"), {
#  myModal("YOU BETTER READ THIS!")
#}, once = TRUE)  # Set once to TRUE to make it only happen once

# Option 2 ---
# observe({
#   if (current$tab == "hospitalDischargeTab") {
#     myModal(appTextL$oshpdModal)
#   }
# })  # Set once to TRUE to make it only happen once

observe({
  if (current$tab == "educationTrendTab") {
    myModal(appTextL$educationModal)
  }
})



# generates help "objects" used for "drop down" help buttons
# single argument to each HTML function is a text object (vector of length 1)
#   generated by source("...AppText.txt) in Global.R
observeEvent(input$causeHelp,     {myModal(appTextL$causeHelp)})
observeEvent(input$cutmethodHelp, {myModal(appTextL$cutmethodHelp)})
observeEvent(input$stateCutHelp,  {myModal(appTextL$stateCutHelp)})
observeEvent(input$measureHelp,   {myModal(appTextL$measureHelp)})
observeEvent(input$levelHelp,     {myModal(appTextL$levelHelp)})
observeEvent(input$dxGroupsHelp,  {myModal(appTextL$dxGroupsHelp)})
observeEvent(input$axisScaleHelp,  {myModal(appTextL$axisScaleHelp)})
observeEvent(input$measureAgeRaceFocusHelp, {myModal(appTextL$measureAgeRaceFocusHelp)})
observeEvent(input$includeBirthsHelp, {myModal(appTextL$includeBirthsHelp)})
observeEvent(input$disparityCompareHelp, {myModal(appTextL$disparityCompareHelp)})



# generates help "objects" used for tab help buttons, as above
tabHelpList <- list("dataTableTab"           = appTextL$conditionTableTab,
                    "interactiveMapTab"      = appTextL$mapTab,
                    "socialDeterminantsTab"  = appTextL$sdohTab,
                    "trendTab"               = appTextL$trendTab,
                    "rankByCauseTab"         = appTextL$conditionTab,
                    "rankByGeographyTab"     = appTextL$rankGeoTab,
                    "rankByCauseAndSexTab"   = appTextL$conditionSexTab,
                    "lifeExpectancyTab"      = appTextL$lifeExpectancyTab,
                    "hospitalDischargeTab"   = HospitalizationsTab, # created in Global.R
                    "hospitalPrimaryAnyTab"  = HospitalPrimaryAnyTab, # created in Global.R
                    "riskByCauseTab"         = appTextL$ihmeTab,
                    "disparitiesTab"         = appTextL$disparitiesTab,
                    "ageRaceFocusTab"        = appTextL$ageRaceFocusTab,
                    "deathHospEDTab"         = appTextL$deathHospEDTab,
                    "demographicsTab"        = appTextL$demographicsTab
                    )

whoNeedsHelp <- reactive({
  if (current$tab %in% names(tabHelpList)) {
    return(tabHelpList[[current$tab]])
  } else { return("Help Info is not yet available for this tab.") }
})

# tabHelp as a Menu Item and output variable
observeEvent(input$plotsMenuItems, {
  if (input$plotsMenuItems == "tabInputs") {
    hide("tabHelpInfo")
    show("inputs")
  } else {
    show("tabHelpInfo")
    hide("inputs")
  }
})

output$currTabInfo <- renderText(whoNeedsHelp())

# tabHelp as a button
# observeEvent(input$tabHelp, {myModal(whoNeedsHelp())})

# observeEvent(input$mapTab,            {myModal(mapTab)})
# observeEvent(input$conditionTab,      {myModal(conditionTab)})
# observeEvent(input$conditionTableTab, {myModal(conditionTableTab)})
# observeEvent(input$conditionSexTab,   {myModal(conditionSexTab)})
# observeEvent(input$rankGeoTab,        {myModal(rankGeoTab)})
# observeEvent(input$trendTab,          {myModal(trendTab)})
# observeEvent(input$sdohTab,           {myModal(sdohTab) })

# generates text "object" used for news you can use buttons, from "...newsUseText.txt" as above
observeEvent(input$newsUse,           {myModal(news_and_updates)})

# -------------------------------------------------------------------------------
# TODO figure out what these observeEvents should achieve & simplify

# create "empty" reactive value
#  then fill it with current "myCause" selection for use elsewhere
current_Cause <- reactiveVal(NULL)
observeEvent(input$myCAUSE, { current_Cause(input$myCAUSE) })

# change "myCause" list based on geographic level selected, and specifiy current
#   selection if Community or County level
observeEvent(input$myGeo, {
    if(input$myGeo=="Census Tract"){updateSelectInput(session, "myCAUSE", choices = bigCode ) }
    if(input$myGeo=="Community")   {updateSelectInput(session, "myCAUSE", choices = phCode, selected=current_Cause()) }
    if(input$myGeo=="County")      {updateSelectInput(session, "myCAUSE", choices = fullList,selected=current_Cause()) }
})

# if myLHJ is not STATE (e.g. "CALIFORNIA"), then myGeo is "Community" so
#  that county map will not show just overall county data

# Can combine these 2 into 1:
observeEvent(input$myLHJ, {
  if(input$myLHJ != STATE & current$tab %in% c("interactiveMapTab","staticMapTab")){
    updateSelectInput(session, "myGeo", selected = "Community") 
     }
})

observeEvent(current$tab, {
  if(input$myLHJ != STATE & current$tab %in% c("interactiveMapTab","staticMapTab")){updateSelectInput(session, "myGeo", selected = "Community") }
})

# Hard wire... Trend only COUNTY for now
observeEvent(current$tab,{
  if(current$tab %in% c("trendTab") )
  {updateSelectInput(session, "myCAUSE", choices = fullList,selected=current_Cause()  )}
})


# TODO These tabs don't have myGeo input?
observeEvent(current$tab,{
  if(current$tab %in% c("rankByGeographyTab","raceTrendTab","educationTrendTab") &  input$myGeo=="Community")
  {updateSelectInput(session, "myCAUSE", choices = phCode,selected=current_Cause()  )}
})

observeEvent(current$tab,{
  if(current$tab %in% c("interactiveMapTab","staticMapTab")  & input$myGeo=="Census Tract" )
  {updateSelectInput(session, "myCAUSE", choices = bigCode )}
})


# -- Jaspo's Version ---
  
# observe({
#   
#   # Jaspo added additional check - This solves the following issue:
#   
#   # User is in Maps tab, and selects a level 3 cause and county. The app updates myGEO to community and myCAUSE list to phCode.
#   # However, the level 3 cause is currently selected, which doesn't exist in phCode. This results in an error. Same with Census Tract.
#   # The additional if conditions in Census Tract and Community below resolve this issue.
#   
#   if(input$myGeo=="Census Tract" & current$tab == "interactiveMapTab"){
#     
#     tempCause <- substr(current_Cause(), 1, 1)
#     current_Cause(tempCause) # current_Cause$val <- tempCause
#     updateSelectInput(session, "myCAUSE", choices = bigCode, selected=current_Cause())
#   }
#   
#   if(input$myGeo=="Community" & current$tab == "interactiveMapTab")   {
#     
#     tempCause <- substr(current_Cause(), 1, 3)
#     current_Cause(tempCause) # current_Cause$val <- tempCause
#     updateSelectInput(session, "myCAUSE", choices = phCode, selected=current_Cause())
#   }
#   
#   if(input$myGeo=="County" & current$tab == "interactiveMapTab") {updateSelectInput(session, "myCAUSE", choices = fullList,selected=current_Cause()) }
# })
# 
# # The problem with above is:
# 
# # 1) if Community or Census Tract is chosen, and a user exits map tab to a tab that has a myCAUSE dropdown,
# # then the list of options will not be the full list.
# 
# # 2) If, for example, a third level dropdown is chosen outside of maps Tab, and a user enters the map Tab where input$myGEO != County,
# # then an error is outputted
# 
# # As a [temp] solution, we observe the tab event and make updates entering and exiting the map tab. For entering the map tab:
# 
# # 1) If myLHJ == STATE, automatically update myGEO to County, which then triggers the observeEvent(myGEO) code above
# # 2) If myLHJ != STATE, automatically update myGEO to Community, which then triggers the observeEvent(myGEO) code above
# 
# # This means entering the map tab will never default myGEO to Census Tract, which I think is the right choice since CT maps 
# # (particularly California) takes a while to load
# 
# observeEvent(current$tab, {
#   
#   # Exiting map tab - Ensure myCAUSE dropdown contains full list
#   if (current$tab != "interactiveMapTab") updateSelectInput(session, "myCAUSE", choices = fullList, selected=current_Cause())
#   
#   # Entering map tab
#   
#   # Always update myGeo to 'Community' upon entering map Tab if myLHJ != STATE
#   if(input$myLHJ != STATE & current$tab %in% c("interactiveMapTab","staticMapTab")){updateSelectInput(session, "myGeo", selected = "Community") }
#   # Always update myGeo to 'County' upon entering map Tab if myLHJ == STATE
#   if(input$myLHJ == STATE & current$tab %in% c("interactiveMapTab","staticMapTab")){updateSelectInput(session, "myGeo", selected = "County") }
#   
# })
#
# observeEvent(input$myLHJ, {
#   if(input$myLHJ != STATE & current$tab %in% c("interactiveMapTab","staticMapTab")){
#     updateSelectInput(session, "myGeo", selected = "Community") 
#   }
# })



#--------------------------------------------------------------------------------
# Render Application Maps and Charts --------------------------------------------




# Trend ----------------------------------------------------------------------------------------------------

trendStep        <- reactive(trendGeneric(input$myLHJ, input$myCAUSE, input$myMeasure, input$trendsID, input$myYearGrouping, input$myLogTrans,input$myMultiRace))
output$trendSex  <- renderPlot(trendStep()$plotL)
output$trendAge  <- renderPlot(trendStep()$plotL)
output$trendRace <- renderPlot(trendStep()$plotL)

observeEvent(current$tab,{
   if(current$tab %in% c("sexTrendTab") ) {

   output$ourPNG <- downloadHandler(filename=function(){paste0(current$tab,"-",input$myCAUSE,"-",input$myLHJ,"-",Sys.Date(),".png")},
                                     content = function(file) {
                                       png(file,width=1000, height=600)
                                       print(trendStep()$plotL)
                                       dev.off()  }  )

   output$ourData <- downloadHandler(filename = function() {paste0(current$tab,"-",input$myCAUSE,"-",input$myLHJ,"-",Sys.Date(), ".csv")},
                                       content = function(file) {
                                         write.csv(trendStep()$dataL, file,row.names = FALSE) } )

 } } )

# Education Trend ----------------------------------------------------------------------------------

trendEducationStep <- reactive(trendEducation(input$myLHJ, input$myCAUSE, input$mySex,input$myMeasure,input$myLogTrans))
output$trendEducation <- renderPlot(trendEducationStep()$plotL)

observeEvent(current$tab,{
  if(current$tab %in% c("educationTrendTab") ) {
    
    output$ourPNG <- downloadHandler(filename=function(){paste0(current$tab,"-",input$myCAUSE,"-",input$myLHJ,"-",Sys.Date(),".png")},
                                     content = function(file) {
                                       png(file,width=1000, height=600)
                                       print(trendEducationStep()$plotL)
                                       dev.off()  }  )
    
    output$ourData <- downloadHandler(filename = function() {paste0(current$tab,"-",input$myCAUSE,"-",input$myLHJ,"-",Sys.Date(), ".csv")},
                                      content = function(file) {
                                        write.csv(trendEducationStep()$dataL, file,row.names = FALSE) } )
    
  } } )


# Life Expectancy ---------------------------------------------------------------------------------------------------------------



leStep            <- reactive(LEtrend(input$myLHJ, input$mySexMult, input$myRace,input$myCI, input$myYearGrouping))
output$lifeTable  <- renderPlot(leStep()$plotL)


observeEvent(current$tab,{
  if(current$tab %in% c("lifeExpectancyTab") ) {
    
    output$ourPNG <- downloadHandler(filename=function(){paste0(current$tab,"-",input$myCAUSE,"-",input$myLHJ,"-",Sys.Date(),".png")},
                                     content = function(file) {
                                       png(file,width=1000, height=600)
                                       print(leStep()$plotL)
                                       dev.off()  }  )
    
    output$ourData <- downloadHandler(filename = function() {paste0(current$tab,"-",input$myCAUSE,"-",input$myLHJ,"-",Sys.Date(), ".csv")},
                                      content = function(file) {
                                        write.csv(leStep()$dataL, file,row.names = FALSE) } )
    
  } } )



# MAPS --------------------------------------------------


# ALL HELL BREAKS LOOSE WITH dbounce
# use dbounce() below to "delay" rendering to avoid temporary error
# output$cbdMapTL     <- debounce(renderLeaflet(cbdMapXLeaf(input$myLHJ, input$myCAUSE, input$myMeasure,      input$myYear, input$mySex,input$myStateCut, input$myGeo, input$myLabName, input$myCutSystem)),2000)

# make's sure inputs are Truthy (read ?isTruthy)
# mapJunk          <- reative({
#   req(input$myLHJ, input$myCAUSE, input$myMeasure,      input$myYear, input$mySex,input$myStateCut, input$myGeo, input$myLabName, input$myCutSystem)
#   cbdMapXLeaf(input$myLHJ, input$myCAUSE, input$myMeasure,      input$myYear, input$mySex,input$myStateCut, input$myGeo, input$myLabName, input$myCutSystem)})


  mapStep          <- reactive(cbdMap(input$myLHJ, input$myCAUSE, input$myMeasure,input$myYear, input$mySex,input$myStateCut, input$myGeo, input$myLabName, input$myCutSystem))
  output$cbdMapTL  <- renderLeaflet(mapStep()$leafPlot)

  observeEvent(current$tab,{
    if(current$tab %in% c("interactiveMapTab") ) {

      output$ourPNG <- downloadHandler(filename=function(){paste0(current$tab,"-",input$myCAUSE,"-",input$myLHJ,"-",Sys.Date(),".png")}, 
                                         content = function(file) {
                                           png(file, width = 10, height = 7, units = "in", pointsize = 10,res=100)
                                           print(mapStep()$plotL)
                                           dev.off() } )

      output$ourData <- downloadHandler(filename = function() {  paste0(current$tab,"-",input$myCAUSE,"-",input$myLHJ,"-",Sys.Date(),".csv")  }, 
                                          content = function(file) {
                                            write.csv(mapStep()$dataL, file,row.names = FALSE) } )

  } } )

  
  
  # ---------------------------------------------------------------------------------------------------------
  
  # CAUSE
  rankCauseStep      <- reactive(rankCause(input$myLHJ,input$myMeasureShort, input$myYear, input$mySex, input$myLev, input$myN))
  output$rankCause   <- renderPlot(rankCauseStep()$plotL)
  
  observeEvent(current$tab,{
    if(current$tab %in% c("rankByCauseTab") ) {
      
      output$ourPNG <- downloadHandler(filename=function(){paste0(current$tab,"-",input$myLHJ,"-",Sys.Date(),".png")}, 
                                       content = function(file) {
                                         png(file, width = 18, height = 10, units = "in", pointsize = 10,res=100)
                                         print(rankCauseStep()$plotL)
                                         dev.off() } )

      output$ourData <- downloadHandler(filename = function() {  paste0(current$tab,"-",input$myLHJ,"-",Sys.Date(),".csv")  }, 
                                        content = function(file) {
                                          write.csv(rankCauseStep()$dataL, file,row.names = FALSE) } )
      
    } } )
  
  
  # GEO
  rankGeoStep      <- reactive(rankGeo(input$myLHJ, input$myCAUSE, input$myMeasure, input$myYear, input$mySex, input$myCI, input$myRefLine))
  output$rankGeo   <- renderPlot(rankGeoStep()$plotL)
  
  observeEvent(current$tab,{
    if(current$tab %in% c("rankByGeographyTab") ) {
      
      output$ourPNG <- downloadHandler(filename=function(){paste0(current$tab,"-",input$myCAUSE,"-",Sys.Date(),".png")},
                                       content = function(file) {
                                         png(file, width = 18, height = 18, units = "in", pointsize = 10,res=100)
                                         print(rankGeoStep()$plotL)
                                         dev.off() }, contentType = "png" )
      
      
       output$ourData <- downloadHandler(filename = function() {  paste0(current$tab,"-",input$myCAUSE,"-",Sys.Date(),".csv")  },
                                         content = function(file) {
                                           write.csv(rankGeoStep()$dataL, file,row.names = FALSE) } )

    } } )
  
# ---------------------------------------------------------------------------------------------------------

disparityStep <- reactive(disparity(input$myLHJ, input$myCAUSE, input$myCompare, input$myAddN,input$myAddRR,input$myAddRate))
output$disparityRace  <- renderPlot(disparityStep()$plot)

observeEvent(current$tab,{
  if(current$tab %in% c("disparitiesTab") ) {
    
    output$ourPNG <- downloadHandler(filename=function(){paste0(current$tab,"-",input$myCAUSE,"-",Sys.Date(),".jpg")},content = function(file) {
      jpeg(file, width = 1200, height = 680)  # , pointsize = 20
      print(disparityStep()$plot)
      dev.off()
    })
    
    
    output$ourData <- downloadHandler(filename = function() {  paste0(current$tab,"-",input$myCAUSE,"-",Sys.Date(),".csv")  },
                                      content = function(file) {
                                        write.csv(disparityStep()$dataL, file,row.names = FALSE) } )
    
  } } )



# myPlotly <- FALSE
# if (!myPlotly)  output$disparityRace <- renderPlot(disparityStep())
# if ( myPlotly)  output$disparityRace <- renderPlotly(disparityStep())

# --AGE/PLACE FOCUS------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------

#------- Initialize the Memory ----------
selected_vals = reactiveValues(myData = "Deaths", myStrata = "Age Group", mySort = "75 - 84", myOlderFocus = F, myMeasureAgeRaceFocus = "cRate")

# AgeRace tab


observe({
  
  selected_vals$myData <- input$myData
  selected_vals$myStrata <- input$myStrata
  selected_vals$mySort <- input$mySort
  selected_vals$myMeasureAgeRaceFocus <- input$myMeasureAgeRaceFocus
  selected_vals$myOlderFocus <- input$myOlderFocus
  
 # print(selected_vals$myOlderFocus)
 # print(selected_vals$mySort)
  
})

# Older Adult Focus
# 1) Only show when age group is selected
# 2) Hide when race/ethncity is selected
# 3) Update sort list when olderAdultFocus = T
# 4) Update sort list when olderAdultFocus = F
# 5) Update default olderAdultFocus value to F when exiting ageRaceFocusTab

# Ensure Older Adult Focus is always set to F when coming into Age Race Focus Tab
observe({
  if (current$tab != "ageRaceFocusTab") updateCheckboxInput(session, "myOlderFocus", label = "Older Adult Focus", value = F)
})

# If age group is chosen AND tab is currently in ageRaceFocus, show older focus checkbox. If Race/Ethnicity is chosen, hide OlderFocus
observe({
  if (selected_vals$myStrata == "Age Group" & current$tab == "ageRaceFocusTab") shinyjs::show("myOlderFocus")
  if (selected_vals$myStrata == "Race/Ethnicity") shinyjs::hide("myOlderFocus")
})

# Update age groups based on OlderAdultFocus selection
observe({
  if(selected_vals$myOlderFocus & current$tab == "ageRaceFocusTab" & selected_vals$myStrata == "Age Group") {
    selectedSort <- if (selected_vals$mySort %in% c("55 - 64", "65 - 74", "75 - 84", "85+")) selected_vals$mySort else "75 - 84"
    updateSelectInput(session, "mySort", choices = ageSort[ageSort >= "55 - 64"], selected = selectedSort)
  }
  
  if(!selected_vals$myOlderFocus & current$tab == "ageRaceFocusTab" & selected_vals$myStrata == "Age Group") {
    updateSelectInput(session, "mySort", choices = ageSort, selected = selected_vals$mySort)
  }
})

# In the event OlderFocus is True, and we go to DeathHospED tab, the age group sorting should be updated to include full list
observe({
  if (current$tab == "deathHospEDTab" & selected_vals$myStrata == "Age Group") {
    updateSelectInput(session, "mySort", choices = ageSort, selected = selected_vals$mySort)
  }
})

# Update age groups based on OlderAdultFocus selection

# observeEvent(input$myOlderFocus, {
#   if(input$myOlderFocus) {updateSelectInput(session, "mySort", choices = ageSort[ageSort >= "55 - 64"], selected = "75 - 84") }
#   if(!input$myOlderFocus){updateSelectInput(session, "mySort", choices = ageSort,                       selected = "75 - 84")}
# })


# If Race/Ethnicity is chosen, update grouping variable dropdown. If age group is chosen, update grouping variable dropdown
observeEvent(input$myStrata, {
  
  if (input$myStrata == "Race/Ethnicity") {
    updateSelectInput(session, "mySort", choices = raceSort, selected = "Black")
  } else {
    updateSelectInput(session, "mySort", choices = ageSort, selected = "75 - 84")
  }
   
})

# Condition to show Adjusted-Rate in measure dropdown
observe({
  if (current$tab == "ageRaceFocusTab" & selected_vals$myData == "Deaths" & selected_vals$myStrata == "Race/Ethnicity") updateSelectInput(session, "myMeasureAgeRaceFocus", choices = c("Number"="N","Crude Rate"="cRate","Adjusted Rate"="aRate"), selected = selected_vals$myMeasureAgeRaceFocus)
})

# Condition to hide Adjusted-Rate in measure dropdown
observe({
  
  if (current$tab == "deathHospEDTab" | selected_vals$myData != "Deaths" | selected_vals$myStrata == "Age Group"){
    
    selectedMeasure <- if (selected_vals$myMeasureAgeRaceFocus != "aRate") selected_vals$myMeasureAgeRaceFocus else "cRate"
    
    updateSelectInput(session, "myMeasureAgeRaceFocus", choices = c("Number"="N","Crude Rate"="cRate"), selected = selectedMeasure)
    
  }
  
 # print(selected_vals$myData)

})

ageRaceFocusStep    <- reactive(makePlotRank(myCounty = input$myLHJ, myData = input$myData, myStrata = input$myStrata, 
                                             mySort = input$mySort, myMeasure = input$myMeasureAgeRaceFocus, 
                                             myLiveborn = input$myLiveborn, myOlderFocus = input$myOlderFocus, myScale = input$myScale))
output$ageRaceFocus <- renderPlot({
  
  # shiny::validate(
  #   need(nrow(ageRaceFocusStep()$dataL) > 0, "Loading...")
  # )
  
  
  ageRaceFocusStep()$plotL 
  
}, height = 800)


observeEvent(current$tab,{
  if(current$tab %in% c("ageRaceFocusTab") ) {
    
    output$ourPNG <- downloadHandler(filename=function(){paste0(current$tab,"-",input$myLHJ,"-",Sys.Date(),".png")}, 
                                     content = function(file) {
                                       png(file, width = 18, height = 10, units = "in", pointsize = 10,res=100)
                                       print(ageRaceFocusStep()$plotL)
                                       dev.off() } )
    
    output$ourData <- downloadHandler(filename = function() {  paste0(current$tab,"-",input$myLHJ,"-",Sys.Date(),".csv")  }, 
                                      content = function(file) {
                                        write.csv(ageRaceFocusStep()$dataL, file,row.names = FALSE) } )
    
  } } )


deathHospEDStep  <- reactive(deathHospEDchart(myCounty = input$myLHJ, myStrata = input$myStrata, mySort = input$mySort, myMeasure = input$myMeasureAgeRaceFocus, myLiveborn = input$myLiveborn))
output$deathHospED <- renderPlot({
  
  # shiny::validate(
  #   need(nrow(deathHospEDStep()$loadData) > 0, "Loading...")
  # )
  
  deathHospEDStep()$plotL 
  
}, height = 800)


observeEvent(current$tab,{
  if(current$tab %in% c("deathHospEDTab") ) {
    
    output$ourPNG <- downloadHandler(filename=function(){paste0(current$tab,"-",input$myLHJ,"-",Sys.Date(),".png")}, 
                                     content = function(file) {
                                       png(file, width = 18, height = 10, units = "in", pointsize = 10,res=100)
                                       print(deathHospEDStep()$plotL)
                                       dev.off() } )
    
    output$ourData <- downloadHandler(filename = function() {  paste0(current$tab,"-",input$myLHJ,"-",Sys.Date(),".csv")  }, 
                                      content = function(file) {
                                        write.csv(deathHospEDStep()$dataL, file,row.names = FALSE) } )
    
  } } )





# --DEMOGRAPHICS - JASPO------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------

# Race/Ethnicity Pie Chart

demographics1Step <- reactive(make_demoPop_RacePie(myCounty = input$myLHJ, myYear = input$myYearDemo))
output$demoPop_RacePie <- renderPlotly(demographics1Step()$plotL)
output$demoPop_RacePie_table <- DT::renderDataTable(demographics1Step()$tableL, server = F)

observeEvent(input$plotButton_demoPop_RacePie,{
  show("demoPop_RacePie")
  hide("demoPop_RacePie_table")

})

observeEvent(input$tableButton_demoPop_RacePie,{
  show("demoPop_RacePie_table")
  hide("demoPop_RacePie")

})


# Population Pyramid Bar Chart

demographics2Step <- reactive(make_demoPop_Pyramid(myCounty = input$myLHJ, myYear = input$myYearDemo))
output$demoPop_Pyramid <- renderPlotly(demographics2Step()$plotL)
output$demoPop_Pyramid_table <- DT::renderDataTable(demographics2Step()$tableL, server = F)

observeEvent(input$plotButton_demoPop_Pyramid,{
  show("demoPop_Pyramid")
  hide("demoPop_Pyramid_table")

})

observeEvent(input$tableButton_demoPop_Pyramid,{
  show("demoPop_Pyramid_table")
  hide("demoPop_Pyramid")

})

# Race/Ethnicity & Age Chart

demographics3Step <- reactive(make_demoPop_RaceAge(myCounty = input$myLHJ, myYear = input$myYearDemo))
output$demoPop_RaceAge <- renderPlotly(demographics3Step()$plotL)
output$demoPop_RaceAge_table <- DT::renderDataTable(demographics3Step()$tableL, server = F)

observeEvent(input$plotButton_demoPop_RaceAge,{
  show("demoPop_RaceAge")
  hide("demoPop_RaceAge_table")

})

observeEvent(input$tableButton_demoPop_RaceAge,{
  show("demoPop_RaceAge_table")
  hide("demoPop_RaceAge")

})

# Trends

demographics4Step <- reactive(make_demoPop_trend(myCounty = input$myLHJ, trendType = input$plotSelect_demoPop_Trend))
output$demoPop_Trend <- renderPlotly(demographics4Step()$plotL)
output$demoPop_Trend_table <- DT::renderDataTable(demographics4Step()$tableL, server = F)

observeEvent(input$plotButton_demoPop_Trend,{
  show("plotSelect_demoPop_Trend")
  show("demoPop_Trend")
  hide("demoPop_Trend_table")

})

observeEvent(input$tableButton_demoPop_Trend,{
  show("demoPop_Trend_table")
  hide("demoPop_Trend")
  hide("plotSelect_demoPop_Trend")

})



# IHME ----------------------------------------------------------------------------------------------------
# Arrows Data and plot
output$arrowsTitles <- renderText({
  paste0('<div style="margin-bottom:-1em;">',
         '<h4 style="white-space:nowrap;">', paste(METRICS[[input$metric]]$name, "of", MEASURES[[input$measure]]$short_name, "for", SEXES[[input$sex]], "in all of California"), '</h4>',
         '<div style="float:left; margin-left: 70px; font-weight: bold;">', input$yearRange[1], " Rankings", '</div>',
         '<div style="position:relative; left: 250px; font-weight: bold;">', input$yearRange[2], " Rankings", '</div>',
         '</div>')
})

output$arrowsLegend <- renderText({   # TODO: lineheight styling, legend text reactive
  paste0('<div style="overflow:hidden;">',
         '<div style="background-color: #C6E2FF; float:left; margin-right:5px; border: 1px solid black; height: 15px; width:15px;"> </div>',
         '<div style="line-height:1; display: block;">', "Legend box text", '</div>',
         '<div style="background-color: #E9A291; float:left; margin-right:5px; border: 1px solid black; height: 20px; width:15px;"> </div>',
         '<div style="line-height:1; display: block;">', "Legend box text numero dos", '</div>',
         '</div>'
  )
  
})

output$network <- renderVisNetwork({
  nodes_and_edges <- create_nodes(input$level, input$measure, input$sex, input$metric,
                                  input$yearRange[1], input$yearRange[2], input$display)
  vis_network(nodes_and_edges, input$display)
})

# RiskByCause Data and plot
FilteredRiskByCause <- reactive({
  return(FilterRiskByCause( input$level, input$year, input$sex, input$metric, input$measure))
})

output$riskByCause <- renderPlotly({
  RiskByCausePlot(FilteredRiskByCause())
})


# ---------------------------------------------------------------------------------------------------------

# Hospital Discharge --------------------------------------------------------------------------------------

oshpdStep <- reactive(oshpdPlot1(input$myLHJ, input$myOSHPDtype, input$mySex, input$myN, input$myLiveborn))
output$OSHPD1 <- renderPlot(oshpdStep()$plotL)

observeEvent(current$tab,{
  if(current$tab %in% c("hospitalDischargeTab") ) {
    
    output$ourPNG <- downloadHandler(filename=function(){paste0(current$tab,"-",input$myLHJ,"-",Sys.Date(),".png")}, 
                                     content = function(file) {
                                       png(file, width = 18, height = 10, units = "in", pointsize = 10,res=100)
                                       print(oshpdStep()$plotL)
                                       dev.off() } )
    
    output$ourData <- downloadHandler(filename = function() {  paste0(current$tab,"-",input$myLHJ,"-",Sys.Date(),".csv")  }, 
                                      content = function(file) {
                                        write.csv(oshpdStep()$dataL, file,row.names = FALSE) } )
    
  } } )

# Hospital Discharge - Any Primary --------------------------------------------------

anyPrimaryStep <- reactive(anyprimary1(input$myLHJ,input$myPosition))
output$any_primary <- renderPlot(anyPrimaryStep()$plotL)

observeEvent(current$tab,{
  if(current$tab %in% c("hospitalPrimaryAnyTab") ) {
    
    output$ourPNG <- downloadHandler(filename=function(){paste0(current$tab,"-",input$myLHJ,"-",Sys.Date(),".png")}, 
                                     content = function(file) {
                                       png(file, width = 18, height = 10, units = "in", pointsize = 10,res=100)
                                       print(anyPrimaryStep()$plotL)
                                       dev.off() } )
    
    output$ourData <- downloadHandler(filename = function() {  paste0(current$tab,"-",input$myLHJ,"-",Sys.Date(),".csv")  }, 
                                      content = function(file) {
                                        write.csv(anyPrimaryStep()$dataL, file,row.names = FALSE) } )
    
  } } )


# output$OSHPD1      <- renderPlot(         oshpdPlot1(input$myLHJ, input$myOSHPDtype, input$mySex, input$myN, input$myVar))
# output$any_primary <- renderPlot(anyprimary1(input$myLHJ,input$myPosition))


output$scatter      <- renderPlotly( scatterSDOH(             input$myCAUSE, input$myMeasure,                    input$mySex,                  input$myGeo,input$myX))

output$rankCauseT   <- renderDataTable(rankCauseTab(input$myLHJ, input$myYear, input$mySex),
                                     option=list(columnDefs=list(list(targets=3:5, class="dt-right")), pageLength = 60)) #DT::





# Generate labels and titles for maps and charts --------------------------------

sexLabel   <- renderText({if (input$mySex == "Total")  sexLabel  <- ""      else sexLabel  <- paste0(", among ",input$mySex,"s")})
geoLabel   <- renderText({if (input$myLHJ==STATE)      geoLab    <- ""      else geoLab    <- paste0(" in ",input$myLHJ)})
timeLabel  <- renderText({if (input$myGeo != "County") timeLabel <- yearGrp5 else timeLabel <- paste(input$myYear)})

# output$map_title <- renderUI({h4(strong(
#                     HTML(paste0(   deathMeasuresNames[deathMeasures == input$myMeasure],
#                                    " from ",
#                                    deathCauseLink[deathCauseLink[,"causeCode"]==input$myCAUSE,"causeName"][1],     # FIX this [1] here now since second element is NA
#                                    " in ",span(timeLabel(),style="color:blue"),
#                                    " by ",input$myGeo,
#                                    sexLabel(), geoLabel(),
#                                    sep = " ")))) })


output$map_title <- renderUI({h4(strong(
  HTML(paste0(   deathMeasuresNames[deathMeasures == input$myMeasure],
                 " from ",
                # pull(filter( deathCauseLink,causeCode==input$myCAUSE),causeName),
                 deathCauseLink$causeName[deathCauseLink$causeCode==input$myCAUSE],
                 " in ",span(timeLabel(),style="color:blue"),
                 " by ",input$myGeo,
                 sexLabel(), geoLabel(),
                 sep = " ")))) })



# URL to tabs

# observe({
#   query <- parseQueryString(session$clientData$url_search)
#   print(names(query))
#   print(query)
#   query1 <- paste(names(query), query, sep = "=", collapse=", ")
#   print(query1)
#   if(query1 == "tab=education trend"){
#     updatePanels(navsID = "trends", tabID = "educationTrendTab")
#   }
#   
#   if(query1 == "tab=disparities"){
#     updatePanels(navsID = "disparities", tabID = "disparitiesTab")
#   }
#   
#   if(query1 == "tab=sex trend, county=alameda") {
#     updatePanels(navsID = "trends", tabID = "sexTrendTab")
#     updateSelectInput(session, inputId = "myLHJ", selected = "Alameda")
#   }
# })


observe({
  query <- parseQueryString(session$clientData$url_search)
  names(query) <- str_to_lower(names(query))
  storeQueryNames <- names(query)
  
  if ('tab' %in% storeQueryNames) {
    
    tFilter <- queryLink_tab %>% filter(queryName == str_to_lower(query$tab)) 
    
    updatePanels(navsID = pull(tFilter, tabID), tabID = pull(tFilter, sub_tabID))
    
  }
  
  if ('cause' %in% storeQueryNames) {
    
    updateSelectInput(session, "myCAUSE", selected = str_to_upper(query$cause))
    
  }
  
  if ('county' %in% storeQueryNames) {
    
    countyValue <- str_to_lower(query$county)
    
    queryCounty <- ifelse(countyValue == 'california', 'CALIFORNIA', str_to_title(query$county))
    updateSelectInput(session, inputId = 'myLHJ', selected = queryCounty)
    
  }
  
  if ('year' %in% storeQueryNames) {
    
    yearValue <- as.numeric(query$year)
    
    if ( !is.null(query$tab) ) {
      
      if ( str_to_lower(query$tab) == 'demographics' ) { updateSliderInput(session, inputId = 'myYearDemo', value = yearValue) } else { updateSliderInput(session, inputId = 'myYear', value = yearValue) }
      
    } else {
      updateSliderInput(session, inputId = 'myYear', value = yearValue)
    }
    
  }
  
  # if ('measure' %in% storeQueryNames) {
  #   
  #   measureValue <- str_to_lower(query$measure)
  #   measureValue <- queryLink_measure %>% filter(queryName == query$measure) %>% pull(ccbShortName)
  #   
  #   updateSelectInput(session, inputId = 'myMeasure', selected = queryMeasure)
  # 
  # }

})




# END of shinyServer ---------------------------------------------------------

}) 

# -- END ----------------------------------------------------------------------

