
ui <- fluidPage(

      # Application title
    titlePanel("Burden of Disease by AGE"),
    
    # Sidebar with a slider input for number of bins 
    
    
    fluidRow(column(3,selectInput( "myData", label = "Data Source:", choices = c("Hospital Discharge"="PDD","Emergency Department"="ED","Deaths"="Deaths"))),         
             column(2,selectInput( "myAgeG", label = "Age Group (only for age tab):", choices = ageList, selected = "35 - 44")),
             column(1,radioButtons( "myScale", label = "Scale:", choices = c("fixed","free_x"))),
             column(3,selectInput( "myRace", label = "RaCE/Ethnic Group (only for race tab):", choices = raceList, selected = "Black-NH")),
             column(3,selectInput( "myCounty", label = "County:", choices = countyList, selected = "CALIFORNIA"))
             ),
      

tabsetPanel(
  type = "tab",
  tabPanel("Age",   
           plotOutput("agePlot", height=700)
      ),
  tabPanel("Race/Ethnicity",   
           plotOutput("racePlot", height=700)
  )
)


)
