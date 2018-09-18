#library(DT)
shinyServer(function(input, output,session) {
 
Gettysburg <- "Four score and seven years ago our fathers brought forth on this continent, a new nation, conceived in Liberty, and dedicated to the proposition that all men are created equal"

observeEvent(input$causeHelp1, {showModal(modalDialog(Gettysburg, easyClose = TRUE))})

observeEvent(input$myGeo, {
    if(input$myGeo=="Census Tract"){updateSelectInput(session, "myCAUSE", choices = bigCode) }
    if(input$myGeo=="Community")   {updateSelectInput(session, "myCAUSE", choices = phCode) }
    if(input$myGeo=="County")      {updateSelectInput(session, "myCAUSE", choices = causeNum36)}
}
)

# observeEvent(input$cZoom,{
#   if(input$cZoom){updateSelectInput(session, "myLHJ", choices = lListNoState) }
# }
# )



observeEvent(input$cZoom,{
  if(input$cZoom){updateSelectInput(session, "myLHJ", choices = lListNoState) }
}
)


# 
# observeEvent(input$ID,{
#   if(input$ID == 6) {updateSelectInput(session, "myLHJ", choices = lListNoState,selected=input$myLHJ) }
# }
# )



observeEvent(input$ID,{
  if(!(input$ID %in% c(33,34))){updateSelectInput(session, "myLHJ", choices = lList,selected=input$myLHJ) }
}
)






#| input$tabs

# observeEvent(input$test1 | input$test2, {
#    if(input$test1==0 && input$test2==0){




# observeEvent(input.ID, {
#   if(input.ID== 5){updateSelectInput(session, "myLHJ", choices = lListNoState) }
# }
# )

output$homeText   <- renderText("Hello")  

output$cbdMapTL   <- renderLeaflet(cbdMapXLeaf(input$myLHJ, input$myCAUSE, input$myMeasure, input$myYear, input$mySex,input$myStateCut, input$myGeo, input$cZoom,input$myLabName, input$myCutSystem))
output$cbdMapTS   <- renderPlot(   cbdMapXStat(input$myLHJ, input$myCAUSE, input$myMeasure, input$myYear, input$mySex,input$myStateCut, input$myGeo, input$cZoom,input$myLabName, input$myCutSystem))
output$rankCause  <- renderPlot(     rankCause(input$myLHJ,                input$myMeasure, input$myYear, input$mySex,                                           input$myN))
output$rankGeo    <- renderPlot(       rankGeo(input$myLHJ, input$myCAUSE, input$myMeasure, input$myYear, input$mySex,                               input$cZoom,input$myCI))
output$trend      <- renderPlot(         trend(input$myLHJ, input$myCAUSE, input$myMeasure                                                                   ))
output$scatter    <- renderPlotly( scatterSDOH(             input$myCAUSE, input$myMeasure,               input$mySex,                  input$myGeo,input$myX))

output$rankCauseT <- renderDataTable(rankCauseTab(input$myLHJ, input$myYear, input$mySex),
                                     option=list(columnDefs=list(list(targets=3:5, class="dt-right")),pageLength = 60)) #DT::

sexLabel <- renderText({sexLabel <- "";
                        if (input$mySex != "Total") sexLabel <- paste0(", among ",input$mySex,"s")}
)
  
output$map_title <- renderUI({
                              HTML(paste0("<div style='text-align:center;font-size:18px'>",
                                   lMeasuresC[lMeasures == input$myMeasure]," - ",
                                   causeList36[causeList36[,"LABEL"]==input$myCAUSE,"nameOnly"],
                                   " in ",input$myLHJ,", ",input$myYear,sexLabel(),
                                  "</div>", sep = " ") ) })
                     })


# OLD junk, possibly archive some: -----------------------------------------------------------------------------------------

#output$cbdMap0   <- renderLeaflet(  cbdMap0Leaflet(input$myLHJ, input$myCAUSE, input$myMeasure, input$myYear, input$mySex,input$myGeo, input$myCutSystem))  
#output$cbdMap1   <- renderPlot(     cbdMap0(       input$myLHJ, input$myCAUSE, input$myMeasure, input$myYear, input$mySex,input$myStateCut, input$myGeo, input$cZoom,input$myLabName ))

#reactive({ validate(need( !(input$myGeo %in% c("Community","Census Tract") & input$myMeasure == "SMR" ),"Please select a data set"))

