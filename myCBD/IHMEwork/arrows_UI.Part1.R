fC <- function(vec) {
  tRep <- length(vec)-1
  paste("input.ID == ",vec,    c(rep("|",tRep),""), collapse="")
}



conditionalPanel(condition = fC(c(90)),

      selectInput("display",
                  label = h4("Display:"),
                  choices = list("Cause" = "cause", "Risk" = "risk"),
                  selected = "risk"),
      
      sliderInput("num_nodes",
                  label = h4("Show top:"),
                  min = 1,
                  max = 50,
                  value = 25),
      
      sliderInput("level",
                  label = h4("Level:"),
                  min = 0,
                  max = 4,
                  value = 3),
      
      selectInput("measure",
                  label = h4("Measure:"),
                  choices = list("Deaths" = 1, "DALYs (Disability-Adjusted Life Years)" = 2, "YLDs (Years Lived with Disability)" = 3, "YLLs (Years of Life Lost)" = 4),
                  selected = 3),
      
      uiOutput("available_years"),
      
      selectInput("sex",
                  label = h4("Sex:"),
                  choices = list("Male" = 1, "Female" = 2, "Both" = 3),
                  selected = 3),
      
      selectInput("metric", 
                  label = h4("Metric/Units:"),
                  choices = list("Number" = 1, "Percent" = 2, "Rate" = 3),
                  selected = 1)
    )
    