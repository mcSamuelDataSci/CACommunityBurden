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
source(paste0(myPlace,"/myFunctions/input_functions.R"))

shinyServer(function(input, output,session) {
  
  # IHME WORK (TEMP) ########################
  
  # #store old current tab as last tab reactive value
  # rv$last_tab = rv$current_tab
  # #store new current tab as cur tab reactive value
  # rv$current_tab = input$selected_tab
  
  #init reactive value storage
  curr_tab <- reactiveValues()
  
  #trigger event on tab selection changes
  observe({
    curr_tab$big = input$bigID
    curr_tab$plots = input$plotsID
    curr_tab$maps = input$mapID
    curr_tab$ranks = input$rankID
    curr_tab$trends = input$trendID
    updateTabItems(session, "plotsMenuItems", "tabInputs")
  })
  
  observe({
    if ((input$bigID != "vizTab") || (input$plotsID == "homeTab")) {
      hideAllInputs()
      hide("plotsMenu")
      show("textHomeTab")
    } else {
      show("plotsMenu")
      show("textNotHomeTab")
      inputID <- ifelse(input$plotsID == "maps", input$mapID,
                        ifelse(input$plotsID == "ranks", input$rankID,
                               ifelse(input$plotsID == "trends", input$trendID,
                                      ifelse(input$plotsID == "dataTableTab", "dataTableTab",
                                             ifelse(input$plotsID == "socialDeterminantsTab", "socialDeterminantsTab",
                                                    ifelse(input$plotsID == "hospitals", input$hospitalID, NULL)))))
      )
      updateInputsOnTabId(inputID, input$myGeo, input$myLHJ, input$myMeasure, input$myMultiRace)
      hide("textHomeTab")
    }
  })
  
  
  # IHME Arrows Data and plot ------------------------
  output$network <- renderVisNetwork({
    nodes_and_edges <- create_nodes(input$level, input$measure, input$sex, input$metric,
                                    input$yearRange[1], input$yearRange[2], input$display)
    vis_network(nodes_and_edges, input$display)
  })
  
  # IHME RiskByCause Data and plot -------------------
  FilteredRiskByCause <- reactive({
    return(
      FilterRiskByCause(
        input$level,
        input$year,
        input$sex,
        input$metric,
        input$measure)
    )
  })
  
  output$riskByCause <- renderPlotly({
    RiskByCausePlot(FilteredRiskByCause())
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
  #
  #
  
  
  # -------------------------------------------------------------------------------
  
  # "onclick" is a function from shinyjs package
  # used here to move to specified tab when specified image in clicked
  # first perameter is id associated with images from home page main pannel
  #  see ui.R  "tabPanel("Home Page"..."
  
  onclick("map1I",      c(updateTabsetPanel(session, inputId="plotsID", selected = "maps"),
                          updateTabsetPanel(session, inputId="mapID",   selected="interactiveMapTab")))
  onclick("map2I",      c(updateTabsetPanel(session, inputId="plotsID", selected = "maps"),
                          updateTabsetPanel(session, inputId="mapID",   selected="staticMapTab")))
  onclick("rankcauseI", c(updateTabsetPanel(session, inputId="plotsID", selected = "ranks"),
                          updateTabsetPanel(session, inputId="rankID",  selected="rankByCauseTab")))
  onclick("ranktableI",   updateTabsetPanel(session, inputId="plotsID", selected="dataTableTab"))  
  onclick("rankgeoI",   c(updateTabsetPanel(session, inputId="plotsID", selected = "ranks"),
                          updateTabsetPanel(session, inputId="rankID",  selected="rankByGeographyTab")))
  onclick("trendI",     c(updateTabsetPanel(session, inputId="plotsID", selected = "trends"),
                          updateTabsetPanel(session, inputId="trendID", selected="trendTab")))
  onclick("scatterI",     updateTabsetPanel(session, inputId="plotsID", selected="socialDeterminantsTab"))
  
  
  
  
  
  
  
  
  
  
  
  # -------------------------------------------------------------------------------
  
  # function used below as "shortcut" for formating each Modal
  myModal <- function(whatInfo) {
    showModal(modalDialog(HTML(whatInfo),
                          easyClose = TRUE,
                          footer = modalButton("Close")
    ))}
  
  # generates help "objects" used for "drop down" help buttons
  # single argument to each HTML function is a text object (vector of length 1)
  #   generated by source("...AppText.txt) in Global.R
  observeEvent(input$causeHelp,     {myModal(causeHelp)})
  observeEvent(input$cutmethodHelp, {myModal(cutmethodHelp)})
  observeEvent(input$statecutHelp,  {myModal(stateCutHelp)})
  observeEvent(input$measureHelp,   {myModal(measureHelp)})
  observeEvent(input$levelHelp,     {myModal(levelHelp)})
  
  # generates help "objects" used for tab help buttons, as above
  
  whoNeedsHelp <- reactive({
    if (curr_tab$big == "vizTab") {
      if (curr_tab$plots == "dataTableTab") { return(conditionTableTab) }
      if (curr_tab$plots == "maps") { return(mapTab) }
      if (curr_tab$plots == "socialDeterminantsTab") { return(sdohTab) }
      if (curr_tab$plots == "trends") { return(trendTab) }
      if (curr_tab$plots == "ranks") { 
        if (curr_tab$ranks == "rankByCauseTab") { return(conditionTab) }
        if (curr_tab$ranks == "rankByGeographyTab") { return(rankGeoTab) }
        if (curr_tab$ranks == "rankByCauseAndSexTab") { return(conditionSexTab) }
      } else (return("Help Info is not yet available for this tab."))
    }
  })
  
  output$currTabInfo <- renderText(whoNeedsHelp())
  observeEvent(input$tabHelp, {myModal(whoNeedsHelp())})
  
  observeEvent(input$plotsMenuItems, {
    if (input$plotsMenuItems == "tabInputs") {
      hide("tabHelpInfo")
      show("inputs")
    } else {
      show("tabHelpInfo")
      hide("inputs")
    }
  })
  
  # observeEvent(input$mapTab,            {myModal(mapTab)})
  # observeEvent(input$conditionTab,      {myModal(conditionTab)})
  # observeEvent(input$conditionTableTab, {myModal(conditionTableTab)})
  # observeEvent(input$conditionSexTab,   {myModal(conditionSexTab)})
  # observeEvent(input$rankGeoTab,        {myModal(rankGeoTab)})
  # observeEvent(input$trendTab,          {myModal(trendTab)})
  # observeEvent(input$sdohTab,           {myModal(sdohTab) })
  
  # generates text "object" used for news you can use buttons, from "...newsUseText.txt" as above
  observeEvent(input$newsUse,           {myModal(newsUse)})
  
  # -------------------------------------------------------------------------------
  
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
  
  observeEvent(input$myLHJ, {
    if(input$myLHJ != STATE & input$mapID %in% c("interactiveMapTab","staticMapTab")){updateSelectInput(session, "myGeo", selected = "Community") }
  })
  
  observeEvent(input$ID, {
    if(input$myLHJ != STATE & input$mapID %in% c("interactiveMapTab","staticMapTab")){updateSelectInput(session, "myGeo", selected = "Community") }
  })
  
  
  
  # Hard wire... Trend only COUNTY for now
  observeEvent(input$ID,{
    if(input$ID %in% c("trendTab") )
    {updateSelectInput(session, "myCAUSE", choices = fullList,selected=current_Cause()  )}
  })
  
  
  observeEvent(input$ID,{
    if(input$ID %in% c("rankByGeographyTab","raceTrendTab","educationTrendTab") &  input$myGeo=="Community")
    {updateSelectInput(session, "myCAUSE", choices = phCode,selected=current_Cause()  )}
  })
  
  
  
  observeEvent(input$ID,{
    if(input$ID %in% c("interactiveMapTab","staticMapTab")  & input$myGeo=="Census Tract" )
    {updateSelectInput(session, "myCAUSE", choices = bigCode )}
  })
  
  
  # not used now, but saved as examples of other reactivity
  # observeEvent(input$ID,{
  #  if(input$ID %in% c(33,34,44,45,55))
  #   {updateSelectInput(session, "myLHJ", choices = lList, selected=current_LHJ())}
  #  if(input$ID %in% c(22,23)  & current_LHJ() != "CALIFORNIA")
  #   {updateSelectInput(session, "myLHJ", choices = lListNoState, selected=current_LHJ())}
  #  if(input$ID %in% c(22,23)  & current_LHJ() == "CALIFORNIA")
  #   {updateSelectInput(session, "cZoom", selected=FALSE) }
  #
  # )
  #for two input use:
  # observeEvent(input$test1 | input$test2, {
  #    if(input$test1==0 && input$test2==0){
  
  
  
  
  
  
  
  #--------------------------------------------------------------------------------
  # Render Application Maps and Charts --------------------------------------------
  
  
  
  output$cbdMapTL     <- renderLeaflet(cbdMapXLeaf(input$myLHJ, input$myCAUSE, input$myMeasure,      input$myYear, input$mySex,input$myStateCut, input$myGeo, input$myLabName, input$myCutSystem))
  
  
  # ALL HELL BREAKS LOOSE WITH dbounce
  # use dbounce() below to "delay" rendering of may to avoid temporary error
  # output$cbdMapTL     <- debounce(renderLeaflet(cbdMapXLeaf(input$myLHJ, input$myCAUSE, input$myMeasure,      input$myYear, input$mySex,input$myStateCut, input$myGeo, input$myLabName, input$myCutSystem)),2000)
  
  # make's sure inputs are Truthy (read ?isTruthy)
  # mapJunk          <- reative({
  #   req(input$myLHJ, input$myCAUSE, input$myMeasure,      input$myYear, input$mySex,input$myStateCut, input$myGeo, input$myLabName, input$myCutSystem)
  #   cbdMapXLeaf(input$myLHJ, input$myCAUSE, input$myMeasure,      input$myYear, input$mySex,input$myStateCut, input$myGeo, input$myLabName, input$myCutSystem)})
  
  
  
  mapJunk          <- reactive(cbdMapXLeaf(input$myLHJ, input$myCAUSE, input$myMeasure,      input$myYear, input$mySex,input$myStateCut, input$myGeo, input$myLabName, input$myCutSystem))
  
  output$cbdMapTL   <- renderLeaflet(mapJunk())
  
  
  output$mapFigureI <- downloadHandler(filename=function(){paste0("MAP2",".png")},content = function(file) {
    png(file, width = 10, height = 7, units = "in", pointsize = 10,res=100)
    print(mapJunk())
    dev.off()
  })
  
  
  # output$mapFigureI <- downloadHandler(filename=function(){paste0("MAP2",".png")},content = function(file) {
  #   png(file, width = 10, height = 7, units = "in", pointsize = 10,res=100)
  #   print(cbdMapXLeaf(input$myLHJ, input$myCAUSE, input$myMeasure,      input$myYear, input$mySex,input$myStateCut, input$myGeo, input$myLabName, input$myCutSystem))
  #   dev.off()
  # })
  
  
  
  output$mapFigure <- downloadHandler(filename=function(){paste0("MAP",".png")},content = function(file) {
    png(file, width = 10, height = 7, units = "in", pointsize = 10,res=100)
    print(cbdMapXStat(input$myLHJ, input$myCAUSE, input$myMeasure,      input$myYear, input$mySex,input$myStateCut, input$myGeo, input$myLabName, input$myCutSystem))
    dev.off()
  })
  
  
  output$rankCauseFigure <- downloadHandler(filename=function(){paste0("CAUSE",".png")},content = function(file) {
    png(file, width = 10, height = 7, units = "in", pointsize = 10,res=100)
    print(rankCause(input$myLHJ,                input$myMeasureShort, input$myYear, input$mySex, input$myLev, input$myN))
    dev.off()
  })
  
  
  output$cbdMapTS     <- renderPlot(   cbdMapXStat(input$myLHJ, input$myCAUSE, input$myMeasure,      input$myYear, input$mySex,input$myStateCut, input$myGeo, input$myLabName, input$myCutSystem))
  
  
  output$rankCause    <- renderPlot(     rankCause(input$myLHJ,                input$myMeasureShort,  input$mySex, input$myLev, input$myN,input$myYear))
  output$rankCauseSex <- renderPlot(  rankCauseSex(input$myLHJ,                input$myMeasure     , input$myYear,              input$myLev, input$myN))
  output$rankGeo      <- renderPlot(       rankGeo(input$myLHJ, input$myCAUSE, input$myMeasure,      input$myYear, input$mySex, input$myCI,input$myRefLine))
  
  
  # Trend ----------------------------------------------------------------------------------------------------
  
  trendStep     <- reactive(trend(input$myLHJ, input$myCAUSE, input$myMeasure, input$myYearGrouping))
  output$trend  <- renderPlot(trendStep()$plot)
  
  #output$trend        <- renderPlotly(trendStep()$plot)
  
  
  output$trendPNG <- downloadHandler(filename=function(){paste0("trend",".png")},content = function(file) {
    png(file, width = 10, height = 7, units = "in", pointsize = 10,res=100)
    print(trendStep()$plot)
    dev.off()
  })
  
  output$trendData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(trendStep()$data, file)
    }
  )
  
  
  # ---------------------------------------------------------------------------------------------------------
  
  
  output$trendAge   <- renderPlot(         trendAge(input$myLHJ, input$myCAUSE, input$myLogTrans))
  
  output$trendRace    <- renderPlot(         trendRace(input$myLHJ, input$myCAUSE, input$myMeasure,input$myLogTrans,input$myMultiRace))
  
  # ---------------------------------------------------------------------------------------------------------
  
  disparityStep <- reactive(disparity(input$myLHJ, input$myCAUSE))
  
  output$trend  <- renderPlot(trendStep()$plot)
  
  myPlotly <- FALSE
  if (!myPlotly)  output$disparityRace <- renderPlot(disparityStep())
  if ( myPlotly)  output$disparityRace <- renderPlotly(disparityStep())
  
  
  # ---------------------------------------------------------------------------------------------------------
  
  
  
  output$trendEducation    <- renderPlot(         trendEducation(input$myLHJ, input$myCAUSE, input$mySex,input$myMeasure,input$myLogTrans))
  
  
  
  # output$OSHPD1    <- renderPlot(         oshpdPlot1(input$myLHJ, input$myOSHPDtype, input$mySex, input$myN))
  output$OSHPD1    <- renderPlot(         oshpdPlot1(input$myLHJ, input$myOSHPDtype, input$mySex, input$myN, input$myVar))
  
  output$OSHPD2    <- renderPlotly(         oshpdPlot2(input$myLHJ, input$myOSHPDtype, input$mySex, input$myN, input$myVar))
  
  
  output$mdcdrg   <- renderPlot(mdc_drg_plot(input$myLHJ, input$myOSHPDtype_mdcdrg, input$mySex, input$myN, input$myVar))
  
  output$any_primary <- renderPlot(anyprimary1(input$myLHJ, input$mySex, input$myprimetype))
  
  output$oshpdmap <- renderPlot(cbdOSHPDMap(input$myLHJ, input$mySex, input$myCause,input$myGeo))
  
  output$scatter      <- renderPlotly( scatterSDOH(             input$myCAUSE, input$myMeasure,                    input$mySex,                  input$myGeo,input$myX))
  
  output$rankCauseT   <- renderDataTable(rankCauseTab(input$myLHJ, input$myYear, input$mySex),
                                         option=list(columnDefs=list(list(targets=3:5, class="dt-right")), pageLength = 60)) #DT::
  
  
  output$lifeTable  <- renderPlot(         LEtrend(input$myLHJ))
  
  
  
  # Generate labels and titles for maps and charts --------------------------------
  
  sexLabel   <- renderText({if (input$mySex == "Total")  sexLabel  <- ""      else sexLabel  <- paste0(", among ",input$mySex,"s")})
  geoLabel   <- renderText({if (input$myLHJ==STATE)      geoLab    <- ""      else geoLab    <- paste0(" in ",input$myLHJ)})
  timeLabel  <- renderText({if (input$myGeo != "County") timeLabel <- yearGrp else timeLabel <- paste(input$myYear)})
  
  output$map_title <- renderUI({h4(strong(
    HTML(paste0(   deathMeasuresNames[deathMeasures == input$myMeasure],
                   " from ",
                   fullCauseList[fullCauseList[,"LABEL"]==input$myCAUSE,"nameOnly"][1],     # FIX this [1] here now since second element is NA
                   " in ",span(timeLabel(),style="color:blue"),
                   " by ",input$myGeo,
                   sexLabel(), geoLabel(),
                   sep = " ")))) })
  
  # END of shinyServer ---------------------------------------------------------
  
}) 

# -- END ----------------------------------------------------------------------


# previously needed - keeping just in case....
#yearGrp <- "2013-2017"   # why doesn't it find this from global?