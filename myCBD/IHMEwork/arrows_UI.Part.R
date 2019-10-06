fC <- function(vec) {
  tRep <- length(vec)-1
  paste("input.ID == ",vec,    c(rep("|",tRep),""), collapse="")
}


# if(input$tab == "riskByCause")  {
#   hide("display")
#   show("level")
#   show("year")
#   show("sex")
#   show("metric")
#   show("measure")
#   hide("showtop")
# }
# 
#   



  # conditionalPanel(condition = fC(c(200)),
  #                  displayButtonInput(),
  #                  levelSliderInput("levelX"),
  #                  measureSelectInput(),
  #                  yearRangeSliderInput(),
  #                  sexButtonInput(),
  #                  metricButtonInput()
  # )
  
  
  
  conditionalPanel(condition = fC(c(201)),
                   levelSliderInput(),
                   measureSelectInput(),
                   yearSliderInput(),
                   sexButtonInput(),
                   metricButtonInput()
    )
  
  
  
#              
# 
#   
  conditionalPanel(condition = fC(c(200)),
                   selectInput("displayZ",
                               label = h4("Display:"),
                               choices = list("Cause" = "cause", "Risk" = "risk"),
                               selected = "risk"),

                   sliderInput("levelZ",
                               label = h4("Level:"),
                               min = 0,
                               max = 4,
                               value = 3),

                   selectInput("measureZ",
                               label = h4("Measure:"),
                               choices = list("Deaths" = 1, "DALYs (Disability-Adjusted Life Years)" = 2, "YLDs (Years Lived with Disability)" = 3, "YLLs (Years of Life Lost)" = 4),
                               selected = 3),

                   sliderTextInput("yearZ",
                                   label = h4("Years:"),
                                   choices = as.character(VALID_YEARS),
                                   selected = range(VALID_YEARS),
                                   grid = TRUE),

                   selectInput("sexZ",
                               label = h4("Sex:"),
                               choices = list("Male" = 1, "Female" = 2, "Both" = 3),
                               selected = 3),

                   selectInput("metricZ",
                               label = h4("Metric/Units:"),
                               choices = list("Number" = 1, "Percent" = 2, "Rate" = 3),
                               selected = 1)
  )
#   
#   
  
  

  

