#library(DT)
shinyServer(function(input, output) {
 
 output$cbdMap0   <- renderLeaflet(cbdMap0Leaflet(input$myLHJ,  input$myCAUSE, input$myMeasure, input$myYear, input$myGeo                                           ))  
 output$cbdMap1   <- renderPlot(   cbdMap0(       input$myLHJX, input$myCAUSE, input$myMeasure, input$myYear, input$myCon, input$myGeo, input$cZoom,input$myLabName ))
 output$rankCause <- renderPlot(   rankCause(     input$myLHJ,                 input$myMeasure, input$myYear,                           input$myN                   ))
 output$rankCauseT<- renderDataTable(rankCauseTab(input$myLHJ, input$myYear),option=list(columnDefs=list(list(targets=3:5, class="dt-right")),
                                                       pageLength = 60)) #DT::
 
   output$rankGeo   <- renderPlot(   rankGeo(       input$myLHJX, input$myCAUSE, input$myMeasure, input$myYear,                           input$gZoom,input$myCI      ))
 output$trend     <- renderPlot(   trend(         input$myLHJ,  input$myCAUSE, input$myMeasure                                                                      ))
 output$scatter   <- renderPlotly( ccbSESplot1(                 input$myCAUSE, input$myMeasure,               input$myGeo,input$myX                                 ))
 
  output$map_title <- renderUI({
                                HTML(paste("<div style='text-align:center;font-size:18px'>",
                                     names(lMeasures[lMeasures==input$myMeasure])," - ",causeList36[causeList36[,1]==input$myCAUSE,2],
                                     "</div>", sep = "") ) })
    })
