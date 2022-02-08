#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(findInFiles)
library(BurStMisc)
library(listviewer)

directories <- c('/mnt/projects/FusionData/0.CCB/myCCB', 
                   '/mnt/projects/FusionData/0.CCB/myUpstream', 
                   '/mnt/projects/FusionData/Excess Mortality 2020 Brief', 
                   '/mnt/projects/FusionData/Population Data', 
                   '/mnt/projects/FusionData/SDOH', 
                   '/mnt/projects/FusionData/State of Public Health')

initial_fileValues <- list.files(path = '/mnt/projects/FusionData/0.CCB/myUpstream', pattern = "\\.[rR]$")

initial_fif_object <- findInFiles(ext = 'R', 
                     pattern = 'datCounty.RDS', 
                     root = '/mnt/projects/FusionData/0.CCB/myUpstream')

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    tabsetPanel(
        
        tabPanel(title = 'Inputs-Outputs', 
                 
                 # Sidebar with a slider input for number of bins 
                 sidebarLayout(
                     sidebarPanel(
                         textInput(inputId = 'directory', 
                                   label = 'Directory', 
                                   value = '/mnt/projects/FusionData/0.CCB/myUpstream'), 
                         
                         actionButton(inputId = 'search', label = 'Search'),
                         
                         selectInput(inputId = 'file', 
                                     label = 'R Script', 
                                     choices = initial_fileValues, 
                                     )
                     ),
                     
                     # Show a plot of the generated distribution
                     mainPanel(
                         textOutput(outputId = 'header'),
                         shiny::h2('Inputs'), 
                         br(),
                         reactjsonOutput(outputId = 'input'),
                         shiny::h2('Outputs'), 
                         br(),
                         reactjsonOutput(outputId = 'output'),
                     )
                 ) # End of sidebarLayout
                 
        ), # End of tabPanel
        
        tabPanel(title = 'Find In Files', 
                 
                 # Sidebar with a slider input for number of bins 
                 sidebarLayout(
                     sidebarPanel(
                         textInput(inputId = 'root', 
                                   label = 'Search directory:', 
                                   value = '/mnt/projects/FusionData/0.CCB/myUpstream'), 
                         
                         textInput(inputId = 'keyword', 
                                   label = 'Search for Keyword', 
                                   value = 'datCounty.RDS'), 
                         
                         actionButton(inputId = 'searchKeyword', label = 'Search')
                     ),
                     
                     # Show a plot of the generated distribution
                     mainPanel(
                         FIFOutput(outputId = 'fif')
                     )
                 ) # End of sidebarLayout
                 
                 
        ) # End of tabPanel
        
        
    )

)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # DIRECTORY REACTIVE VALUE
    dirValue <- reactiveValues(value = '/mnt/projects/FusionData/0.CCB/myUpstream', 
                               keyword = 'datCounty.RDS', 
                               root = '/mnt/projects/FusionData/0.CCB/myUpstream', 
                               fif = initial_fif_object)
    
    # UPDATE REACTIVE VALUE AND SCRIPTS INPUT
    observeEvent(input$search, {
        
        dirValue$value <- input$directory
        
        scripts <- list.files(path = dirValue$value, pattern = "\\.[rR]$")
        updateSelectInput(inputId = 'file', choices = scripts)
        
    })
    
    observeEvent(input$keyword, {
        
        dirValue$keyword <- input$keyword
        dirValue$root <- input$root
        
        dirValue$fif <- findInFiles(ext = 'R', 
                        pattern = dirValue$keyword, 
                        root = dirValue$root)
        
    })
    
    
    # DIRECTORY HEADER
    output$header <- renderText({
        dirValue$value
    })
    
    observe({print(input$file)})

    # MAIN PANEL - INPUTS
    output$input <- renderReactjson({
        
        reactjson(scriptSearch(pattern = 'read|source|load', 
                     path = dirValue$value, 
                     suffix = paste(input$file, collapse = '|'),
                     commentsIncluded = F,
                     subdirs = F, 
                     verbose = T), 
                  enableClipboard = F, 
                  displayObjectSize = F,
                  displayDataTypes = F, 
                  onEdit = F, 
                  onAdd = F, 
                  onDelete = F, 
                  onSelect = F)
        
    })
    
    # MAIN PANEL - OUTPUTS
    output$output <- renderReactjson({
        
        reactjson(scriptSearch(pattern = 'save|write', 
                               path = dirValue$value, 
                               suffix = paste(input$file, collapse = '|'),
                               commentsIncluded = F,
                               subdirs = F, 
                               verbose = T), 
                  enableClipboard = F, 
                  displayObjectSize = F,
                  displayDataTypes = F, 
                  onEdit = F, 
                  onAdd = F, 
                  onDelete = F, 
                  onSelect = F)
        
    })
    
    
    # FIND IN FILES
    output$fif <- renderFIF({
        
        dirValue$fif
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
