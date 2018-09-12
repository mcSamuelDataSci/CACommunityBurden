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

output$homeText  <- renderText("Hello")  
output$cbdMap0   <- renderLeaflet(  cbdMap0Leaflet(input$myLHJ, input$myCAUSE, input$myMeasure, input$myYear, input$mySex,input$myGeo, input$myCutSystem))  
output$cbdMap1   <- renderPlot(     cbdMap0(       input$myLHJ, input$myCAUSE, input$myMeasure, input$myYear, input$mySex,input$myStateCut, input$myGeo, input$cZoom,input$myLabName ))

output$cbdMapTL  <- renderLeaflet(cbdMapXLeaf(input$myLHJ, input$myCAUSE, input$myMeasure, input$myYear, input$mySex,input$myStateCut, input$myGeo, input$cZoom,input$myLabName, input$myCutSystem))
output$cbdMapTS  <- renderPlot(   cbdMapXStat(    input$myLHJ, input$myCAUSE, input$myMeasure, input$myYear, input$mySex,input$myStateCut, input$myGeo, input$cZoom,input$myLabName, input$myCutSystem))

output$rankCause <- renderPlot(     rankCause(     input$myLHJ,                input$myMeasure, input$myYear, input$mySex,                          input$myN                   ))
output$rankCauseT<- renderDataTable(rankCauseTab(  input$myLHJ, input$myYear,input$mySex),option=list(columnDefs=list(list(targets=3:5, class="dt-right")),pageLength = 60)) #DT::
output$rankGeo   <- renderPlot(     rankGeo(       input$myLHJ, input$myCAUSE, input$myMeasure, input$myYear,  input$mySex,                         input$cZoom,input$myCI      ))
output$trend     <- renderPlot(     trend(         input$myLHJ, input$myCAUSE, input$myMeasure, input$mySex                                                                     ))
output$scatter   <- renderPlotly(   scatterSDOH(                input$myCAUSE, input$myMeasure,               input$myGeo,input$myX ,  input$mySex                              ))
 


# tLHJ <- input$myLHJ
# if (!input$cZoom) tLHJ <- 'California'


output$map_title <- renderUI({
                              HTML(paste("<div style='text-align:center;font-size:18px'>",
                                   lMeasuresC[lMeasures == input$myMeasure]," - ",causeList36[causeList36[,1]==input$myCAUSE,2],"in",input$myLHJ
                                   
                                   
                                   ,",",input$myYear,
                                  "</div>", sep = " ") ) })
                     })



#reactive({ validate(need( !(input$myGeo %in% c("Community","Census Tract") & input$myMeasure == "SMR" ),"Please select a data set"))

