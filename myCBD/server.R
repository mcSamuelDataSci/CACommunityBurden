#yearGrp <- "2013-2017"   # why doesn't it find this from global?





shinyServer(function(input, output,session) {

  

  
   
  
shinyjs::onclick("map1I",     updateTabsetPanel(session,inputId="ID",selected="22"))  
shinyjs::onclick("map2I",     updateTabsetPanel(session,inputId="ID",selected="23"))  
shinyjs::onclick("rankcauseI",updateTabsetPanel(session,inputId="ID",selected="33"))  
shinyjs::onclick("ranktableI",updateTabsetPanel(session,inputId="ID",selected="45"))  
shinyjs::onclick("rankgeoI",  updateTabsetPanel(session,inputId="ID",selected="44"))  
shinyjs::onclick("trendI",    updateTabsetPanel(session,inputId="ID",selected="55"))  
shinyjs::onclick("scatterI",  updateTabsetPanel(session,inputId="ID",selected="66"))  
  
  
observeEvent(input$causeHelp,     {showModal(modalDialog(HTML(  causeHelp),    easyClose = TRUE))})
observeEvent(input$cutmethodHelp, {showModal(modalDialog(HTML(cutmethodHelp),easyClose = TRUE))})
observeEvent(input$statecutHelp,  {showModal(modalDialog(HTML( stateCutHelp), easyClose = TRUE))})
observeEvent(input$measureHelp,   {showModal(modalDialog(HTML(measureHelp), easyClose = TRUE))})

observeEvent(input$mapTab,            {showModal(modalDialog(HTML(mapTab),            easyClose = TRUE))})
observeEvent(input$conditionTab,      {showModal(modalDialog(HTML(conditionTab),      easyClose = TRUE))})
observeEvent(input$conditionTableTab, {showModal(modalDialog(HTML(conditionTableTab), easyClose = TRUE))})
observeEvent(input$conditionSexTab,   {showModal(modalDialog(HTML(conditionSexTab),   easyClose = TRUE))})
observeEvent(input$rankGeoTab,        {showModal(modalDialog(HTML(rankGeoTab),        easyClose = TRUE))})
observeEvent(input$trendTab,          {showModal(modalDialog(HTML(trendTab),          easyClose = TRUE))})
observeEvent(input$sdohTab,           {showModal(modalDialog(HTML(sdohTab),           easyClose = TRUE))})


observeEvent(input$newsUse,           {showModal(modalDialog(HTML(newsUse),           easyClose = TRUE))})




# https://stackoverflow.com/questions/28379937/change-selectize-choices-but-retain-previously-selected-values
current_Cause <- reactiveVal(NULL)
# now store your current selection in the reactive value
observeEvent(input$myCAUSE, { current_Cause(input$myCAUSE) })

observeEvent(input$myGeo, {
    if(input$myGeo=="Census Tract"){updateSelectInput(session, "myCAUSE", choices = bigCode ) }
    if(input$myGeo=="Community")   {updateSelectInput(session, "myCAUSE", choices = phCode, selected=current_Cause()) } 
    if(input$myGeo=="County")      {updateSelectInput(session, "myCAUSE", choices = causeNum36,selected=current_Cause()  ) }
})

observeEvent(input$myLHJ, {
  if(input$myLHJ != STATE){updateSelectInput(session, "myGeo", selected = "Community") }
})


# current_LHJ <- reactiveVal(NULL)
# observeEvent(input$myLHJ, { current_LHJ(input$myLHJ) })
# 
# observeEvent(input$ID,{
#  if(input$ID %in% c(33,34,44,45,55)                        ) { updateSelectInput(session, "myLHJ", choices = lList,       selected=current_LHJ() ) }
#  if(input$ID %in% c(22,23)  & current_LHJ() != "CALIFORNIA") { updateSelectInput(session, "myLHJ", choices = lListNoState,selected=current_LHJ() ) }
#  if(input$ID %in% c(22,23)  & current_LHJ() == "CALIFORNIA") { updateSelectInput(session, "cZoom", selected=FALSE) }
#   
#   #                                                               updateSelectInput(session, "myLHJ", choices = lList) }
# }
# )

# current_LHJ <- reactiveVal(NULL)
# observeEvent(input$myLHJ, { current_LHJ(input$myLHJ) })
# 
# 
# observeEvent(input$cZoom,{
#   if(input$cZoom & input$myLHJ != STATE)    {updateSelectInput(session, "myLHJ", choices = lListNoState,selected=current_LHJ()) }
#   if(input$cZoom & input$myLHJ == STATE)    {updateSelectInput(session, "myLHJ", choices = lListNoState )}
#   if(!(input$cZoom)) {updateSelectInput(session, "myLHJ", choices = lList,selected="CALIFORNIA")}
# }
# )
# 
#  observeEvent(input$ID,{
#    if(input$ID %in% c(33,34,44,55,66)){updateSelectInput(session, "myLHJ", choices = lList,selected=current_LHJ()) }
#  })
# 
# 
# #observeEvent(input$ID,{
# #  if(input$ID %in% c(22,23)){updateSelectInput(session, "myLHJ", choices = lListNoState) }
# #})
# 
#  observeEvent(input$ID,{
#   if(input$ID %in% c(22,23)  & current_LHJ() != "CALIFORNIA") { updateSelectInput(session, "myLHJ", choices = lListNoState,selected=current_LHJ() ) }
#   if(input$ID %in% c(22,23)  & current_LHJ() == "CALIFORNIA") { updateCheckboxInput(session, "cZoom", value=FALSE)
#                                                                 updateSelectInput(session, "myLHJ", choices = lListNoState)}
#  })





 
 
 
#observeEvent(input$myGeo , input$myMeasure, {
#    if(input$myGeo != "County" && input$myMeasure=="SMR"){showModal(modalDialog(HTML(sdohTab),           easyClose = TRUE))}})

#for two input use:
# observeEvent(input$test1 | input$test2, {
#    if(input$test1==0 && input$test2==0){




#------------------------------------------------------------------------------------------------------------------------------------------

output$cbdMapTL     <- renderLeaflet(cbdMapXLeaf(input$myLHJ, input$myCAUSE, input$myMeasure, input$myYear, input$mySex,input$myStateCut, input$myGeo, input$myLabName, input$myCutSystem))
output$cbdMapTS     <- renderPlot(   cbdMapXStat(input$myLHJ, input$myCAUSE, input$myMeasure, input$myYear, input$mySex,input$myStateCut, input$myGeo, input$myLabName, input$myCutSystem))
output$rankCause    <- renderPlot(     rankCause(input$myLHJ,           input$myMeasureShort, input$myYear, input$mySex, input$myLev, input$myN))
output$rankCauseSex <- renderPlot(     rankCauseSex(input$myLHJ,        input$myMeasure     , input$myYear,              input$myLev, input$myN))

output$rankGeo    <- renderPlot(       rankGeo(input$myLHJ, input$myCAUSE, input$myMeasure, input$myYear, input$mySex,                               input$myCI,input$myRefLine))

output$trend      <- renderPlot(         trend(input$myLHJ, input$myCAUSE, input$myMeasure                                                                   ))
output$trend2      <- renderPlot(         trend(input$myLHJ, input$myCAUSE, input$myMeasure                                                                   ))

output$scatter    <- renderPlotly( scatterSDOH(             input$myCAUSE, input$myMeasure,               input$mySex,                  input$myGeo,input$myX))

output$rankCauseT <- renderDataTable(rankCauseTab(input$myLHJ, input$myYear, input$mySex),
                                     option=list(columnDefs=list(list(targets=3:5, class="dt-right")),pageLength = 60)) #DT::

sexLabel   <- renderText({if (input$mySex == "Total")  sexLabel  <- ""      else sexLabel  <- paste0(", among ",input$mySex,"s")})
geoLabel   <- renderText({if (input$myLHJ==STATE)        geoLab    <- ""      else geoLab    <- paste0(" in ",input$myLHJ)})
timeLabel  <- renderText({if (input$myGeo != "County") timeLabel <- yearGrp else timeLabel <- paste(input$myYear)})
### not sure why I can't use timeLabel <- yearGrp here?

# output$map_title <- renderUI({
#                     HTML(paste0("<div style='text-align:left;>",
#                                    lMeasuresC[lMeasures == input$myMeasure],
#                                    " from ",
#                                    causeList36[causeList36[,"LABEL"]==input$myCAUSE,"nameOnly"][1],     # FIX this [1] here now since second element is NA
#                                    " in ",span(timeLabel(),style="color:blue"),
#                                    " by ",input$myGeo,
#                                    sexLabel(), geoLabel(),
#                                    "</div>", sep = " ")) })


output$map_title <- renderUI({h4(strong(
                    HTML(paste0(   lMeasuresC[lMeasures == input$myMeasure],
                                   " from ",
                                   causeList36[causeList36[,"LABEL"]==input$myCAUSE,"nameOnly"][1],     # FIX this [1] here now since second element is NA
                                   " in ",span(timeLabel(),style="color:blue"),
                                   " by ",input$myGeo,
                                   sexLabel(), geoLabel(),
                                   sep = " ")))) })

# HTML(paste0( "<div style='font-weight:bold;>",  lMeasuresC[lMeasures == input$myMeasure],
#              " from ",
#              causeList36[causeList36[,"LABEL"]==input$myCAUSE,"nameOnly"][1],     # FIX this [1] here now since second element is NA
#              " in ",span(timeLabel(),style="color:blue"),
#              " by ",input$myGeo,
#              sexLabel(), geoLabel(),"</div>",
#              sep = " "))) })



#  {font-weight: bold;}
             
# "</div>"
                                          




                     })


# OLD junk, possibly archive some: -----------------------------------------------------------------------------------------

#output$cbdMap0   <- renderLeaflet(  cbdMap0Leaflet(input$myLHJ, input$myCAUSE, input$myMeasure, input$myYear, input$mySex,input$myGeo, input$myCutSystem))  
#output$cbdMap1   <- renderPlot(     cbdMap0(       input$myLHJ, input$myCAUSE, input$myMeasure, input$myYear, input$mySex,input$myStateCut, input$myGeo, input$cZoom,input$myLabName ))

#reactive({ validate(need( !(input$myGeo %in% c("Community","Census Tract") & input$myMeasure == "SMR" ),"Please select a data set"))

