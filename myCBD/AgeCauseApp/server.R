server <- function(input, output) {
    output$agePlot <- renderPlot({rankStrataAge(input$myAgeG, input$myCounty,input$myData,)})
    output$racePlot <- renderPlot({rankStrataRace(input$myRace, input$myCounty)})
    
}