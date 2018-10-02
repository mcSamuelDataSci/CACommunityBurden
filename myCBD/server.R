#yearGrp <- "2013-2017"   # why doesn't it find this from global?

shinyServer(function(input, output,session) {
 
observeEvent(input$causeHelp,     {showModal(modalDialog(     causeHelp,    easyClose = TRUE))})
observeEvent(input$cutmethodHelp, {showModal(modalDialog(     cutmethodHelp,    easyClose = TRUE))})
observeEvent(input$statecutHelp,  {showModal(modalDialog(     statecutHelp,    easyClose = TRUE))})
observeEvent(input$measureHelp,   {showModal(modalDialog(HTML(measureHelp), easyClose = TRUE))})

observeEvent(input$mapTab,            {showModal(modalDialog(HTML(mapTab),            easyClose = TRUE))})
observeEvent(input$conditionTab,      {showModal(modalDialog(HTML(conditionTab),      easyClose = TRUE))})
observeEvent(input$conditionTableTab, {showModal(modalDialog(HTML(conditionTableTab), easyClose = TRUE))})
observeEvent(input$conditionSexTab,   {showModal(modalDialog(HTML(conditionSexTab),   easyClose = TRUE))})
observeEvent(input$rankGeoTab,        {showModal(modalDialog(HTML(rankGeoTab),        easyClose = TRUE))})
observeEvent(input$trendTab,          {showModal(modalDialog(HTML(trendTab),          easyClose = TRUE))})
observeEvent(input$sdohTab,           {showModal(modalDialog(HTML(sdohTab),           easyClose = TRUE))})

observeEvent(input$myGeo, {
    if(input$myGeo=="Census Tract"){updateSelectInput(session, "myCAUSE", choices = bigCode) }
    if(input$myGeo=="Community")   {updateSelectInput(session, "myCAUSE", choices = phCode) }
    if(input$myGeo=="County")      {updateSelectInput(session, "myCAUSE", choices = causeNum36)}
}
)


observeEvent(input$ID,{
 if(input$ID %in% c(33,34,44,45,55)){updateSelectInput(session, "myLHJ", choices = lList,selected=input$myLHJ) }
}
)

observeEvent(input$cZoom,{
  if(input$cZoom){updateSelectInput(session, "myLHJ", choices = lListNoState) }
}
)



#observeEvent(input$myGeo , input$myMeasure, {
#    if(input$myGeo != "County" && input$myMeasure=="SMR"){showModal(modalDialog(HTML(sdohTab),           easyClose = TRUE))}})

#for two input use:
# observeEvent(input$test1 | input$test2, {
#    if(input$test1==0 && input$test2==0){




#------------------------------------------------------------------------------------------------------------------------------------------

output$cbdMapTL     <- renderLeaflet(cbdMapXLeaf(input$myLHJ, input$myCAUSE, input$myMeasure, input$myYear, input$mySex,input$myStateCut, input$myGeo, input$cZoom,input$myLabName, input$myCutSystem))
output$cbdMapTS     <- renderPlot(   cbdMapXStat(input$myLHJ, input$myCAUSE, input$myMeasure, input$myYear, input$mySex,input$myStateCut, input$myGeo, input$cZoom,input$myLabName, input$myCutSystem))
output$rankCause    <- renderPlot(     rankCause(input$myLHJ,           input$myMeasureShort, input$myYear, input$mySex, input$myLev, input$myN))
output$rankCauseSex <- renderPlot(     rankCauseSex(input$myLHJ,        input$myMeasure     , input$myYear,                           input$myN))

output$rankGeo    <- renderPlot(       rankGeo(input$myLHJ, input$myCAUSE, input$myMeasure, input$myYear, input$mySex,                               input$cZoom,input$myCI))
output$trend      <- renderPlot(         trend(input$myLHJ, input$myCAUSE, input$myMeasure                                                                   ))
output$scatter    <- renderPlotly( scatterSDOH(             input$myCAUSE, input$myMeasure,               input$mySex,                  input$myGeo,input$myX))

output$rankCauseT <- renderDataTable(rankCauseTab(input$myLHJ, input$myYear, input$mySex),
                                     option=list(columnDefs=list(list(targets=3:5, class="dt-right")),pageLength = 60)) #DT::

sexLabel   <- renderText({if (input$mySex == "Total")  sexLabel  <- ""      else sexLabel  <- paste0(", among ",input$mySex,"s")})
geoLabel   <- renderText({if (!input$cZoom)            geoLab    <- ""      else geoLab    <- paste0(", in ",input$myLHJ)})
timeLabel  <- renderText({if (input$myGeo != "County") timeLabel <- yearGrp else timeLabel <- paste(input$myYear)})
### not sure why I can't use timeLabel <- yearGrp here?



output$map_title <- renderUI({
                              HTML(paste0("<div style='text-align:center;font-size:18px'>",
                                   lMeasuresC[lMeasures == input$myMeasure]," - ",
                                   causeList36[causeList36[,"LABEL"]==input$myCAUSE,"nameOnly"],
                                   geoLabel()," ",span(timeLabel(),style="color:blue"),sexLabel(),
                                  "</div>", sep = " ") ) })
                     })


# OLD junk, possibly archive some: -----------------------------------------------------------------------------------------

#output$cbdMap0   <- renderLeaflet(  cbdMap0Leaflet(input$myLHJ, input$myCAUSE, input$myMeasure, input$myYear, input$mySex,input$myGeo, input$myCutSystem))  
#output$cbdMap1   <- renderPlot(     cbdMap0(       input$myLHJ, input$myCAUSE, input$myMeasure, input$myYear, input$mySex,input$myStateCut, input$myGeo, input$cZoom,input$myLabName ))

#reactive({ validate(need( !(input$myGeo %in% c("Community","Census Tract") & input$myMeasure == "SMR" ),"Please select a data set"))

