# ADD visNetwork to "install packages" file

library(dplyr)
library(magrittr)
library(visNetwork)
library(stringr)
library(shiny)
library(shinyWidgets)

# Load and format data-----------------------------------------------------------------------
endpoints <- read.csv("API_endpoints.csv", header = TRUE)
cause_data <- readRDS("cause_data.rds")
risk_data <- readRDS("risk_data.rds") %>%
  select(., -cause_id)

cause_data$first_parent <- ifelse(cause_data$cause_id %in% c(295:409), 'Communicable, maternal, neonatal, and nutritional diseases',
                                  ifelse(cause_data$cause_id %in% c(409:686),'Non-communicable diseases',
                                         ifelse(cause_data$cause_id >= 687,'Injuries','0')))

risk_data$first_parent <- ifelse(risk_data$sort_order %in% c(2:34), 'Environmental/occupational risks',
                                  ifelse(risk_data$sort_order %in% c(35:78),'Behavioral risks',
                                         ifelse(risk_data$sort_order >= 79,'Metabolic risks','0')))

colnames(cause_data)[1] <- 'id_num'
colnames(risk_data)[1] <- 'id_num'
colnames(cause_data)[11] <- 'id_name'
colnames(risk_data)[11] <- 'id_name'

colnames(cause_data)[12] <- 'display'
colnames(risk_data)[12] <- 'display'
cause_data$display <- 'cause'
risk_data$display <- 'risk'

data <- bind_rows(cause_data, risk_data) %>%
  mutate(level = ifelse(id_num %in% setdiff(id_num, parent_id) & level == 2, paste(2,3,4, sep =","),
                        ifelse(id_num %in% setdiff(id_num, parent_id) & level == 3, paste(3,4, sep=","), level)))

# Define constants -----------------------------------------------------------------------
CAUSE_YEARS <- sort(unique(cause_data$year_id))
RISK_YEARS <- sort(unique(risk_data$year_id))
LABEL_LENGTH <- 25
LEFT_X <- -LABEL_LENGTH*25 + 425
RIGHT_X <- 800
HALF_BOX_WIDTH <- LABEL_LENGTH*13.4

Y_SPACE_FACTOR <- 80
FONT_SIZE <- 45
HEIGHT_FACTOR <- 6
HEIGHT_ADD <- 80
DRAG_ON <- FALSE
WIDTH <- '100%'
HEIGHT_CONSTRAINT <- 60

# Create nodes function -----------------------------------------------------------------------
create_nodes <- function(level_in, measure_id_in, sex_id_in, metric_id_in,
                         year_from, year_to, display_in, num_nodes) {

  left_nodes <- data %>%
    filter(display == display_in, grepl(level_in, level), year_id == year_from,
           measure_id == measure_id_in, sex_id == sex_id_in, metric_id == metric_id_in) %>%
    arrange(id_num) %>%
    mutate(rank = rank(-val, ties.method = 'first')) %>%
    filter(rank <= num_nodes)
  
  right_nodes <- data %>%
    filter(display == display_in, grepl(level_in, level), year_id == year_to,
           measure_id == measure_id_in, sex_id == sex_id_in, metric_id == metric_id_in,
           id_name %in% left_nodes$id_name) %>%
    arrange(id_num) %>%
    mutate(rank = rank(-val, ties.method = 'first'))
  
  # Reset number of nodes in case it is less than original number of nodes requested
  NUM_NODES <- nrow(left_nodes)
  
  # Determine longest id_name for node label formatting
  LONGEST <- max(nchar(left_nodes$id_name))+3
  
  # Combine left and right nodes into selected_data
  selected_data <- bind_rows(left_nodes, right_nodes) %>%
    group_by(year_id) %>%
    mutate(rank2 = rank(rank))  # rank2 for y location of nodes
  selected_data$change <- rep(round((right_nodes$val-left_nodes$val)/left_nodes$val, 4), 2)
  
  # Three groups of nodes: Labels, Edges, and Titles
  label_nodes <- data.frame(selected_data, id = (nrow(selected_data)+1):(2*nrow(selected_data)),
                            label = substr(str_pad(paste(selected_data$rank, selected_data$id_name),
                                                   width = LABEL_LENGTH, side = "right"),
                                           start = 1, stop = LABEL_LENGTH),
                            group = selected_data$first_parent,
                            title = paste(selected_data$id_name,
                                          "<br><b>Year: </b>", selected_data$year_id,
                                          "<br><b>Rank: </b>", selected_data$rank,
                                          "<br><b>Change: </b>", selected_data$change*100, "%", " (", year_from, " to ", year_to, ")",
                                          "<br><b>", endpoints$metric_name[selected_data$metric_id], ": </b>", selected_data$val,
                                          endpoints$mm_title[as.numeric(paste(selected_data$measure_id, selected_data$metric_id, sep = ""))],
                                          " (", selected_data$lower, "-", selected_data$upper, ")",
                                          sep =""),
                            x = c(rep(LEFT_X, NUM_NODES), rep(RIGHT_X, NUM_NODES)),
                            y = selected_data$rank2*Y_SPACE_FACTOR - NUM_NODES*15)
  
  edge_nodes <- data.frame(id = 1:nrow(selected_data), hidden = TRUE, group = selected_data$first_parent,
                           x = c(rep(LEFT_X+HALF_BOX_WIDTH, NUM_NODES),
                                 rep(RIGHT_X-HALF_BOX_WIDTH, NUM_NODES)),
                           y = label_nodes$y)
  
  title_nodes <- data.frame(label = c(paste(year_from, "Rank"), paste(year_to, "Rank")), rank = c(0,0),
                            x = c(LEFT_X, RIGHT_X), y = -NUM_NODES*15, id = 0:-1, shape = 'text',
                            font = list(face = 'Bold', size = 45))
  
  # suppressWarnings on this row bind because we want to ignore the coercing to character warnings.
  nodes <- suppressWarnings(bind_rows(title_nodes, label_nodes, edge_nodes))
  
  edges <- data.frame(from = c(1:NUM_NODES, 1:(2*NUM_NODES)),
                      to = c((NUM_NODES+1):(2*NUM_NODES), (2*NUM_NODES+1):(4*NUM_NODES)),
                      dashes = ifelse(selected_data$rank[1:NUM_NODES] < selected_data$rank[(NUM_NODES+1):(2*NUM_NODES)],
                                      "[20,15]", "false"))

  return(list("nodes" = nodes, "edges" = edges))
}

# Create vis network (and supplemental) functions -----------------------------------------------------------------------
vis_network <- function(nodes, edges, subtitle, display) {
  length <- nchar('and nutritional diseases')
  if (display == "cause") {
    groups <- c('Communicable, maternal, neonatal, and nutritional diseases',
                'Non-communicable diseases',
                'Injuries',
                'Communicable, maternal,\nneonatal, and nutritional\ndiseases',
                'Non-communicable               \ndiseases\n',
                '\nInjuries                                               \n')
  }
  else {
    groups <- c('Environmental/occupational risks',
                'Behavioral risks',
                'Metabolic risks',
                'Environmental/\noccupational risks',
                'Behavioral risks       \n',
                'Metabolic risks         \n')
  }
  visNetwork(nodes, edges, main = "California", submain = paste(subtitle)) %>%
    visOptions(height = (nrow(edges)+1)*HEIGHT_FACTOR + HEIGHT_ADD, width = WIDTH) %>%
    visNodes(heightConstraint = HEIGHT_CONSTRAINT, fixed = TRUE,
             shape = 'box', font = list(face = 'Monaco', size = FONT_SIZE)) %>%
    visEdges(width = 4, smooth = FALSE, hoverWidth = 0) %>%
    visLegend(width = .25, position = 'right', zoom = FALSE, useGroups = FALSE,
              addNodes = data.frame(shape = 'box', label = groups[4:6], color = c('#E9A291', '#C6E2FF', '#A0DCA4'),
                                    font = list(face = 'Bold', size = 40, align = 'left')), stepY = 150) %>%
    visInteraction(hover = TRUE, hoverConnectedEdges = TRUE, zoomView = FALSE, dragView = DRAG_ON, selectable = FALSE) %>%
    visGroups(groupname = groups[1], color = '#E9A291') %>%
    visGroups(groupname = groups[2], color = '#C6E2FF') %>%
    visGroups(groupname = groups[3], color = '#A0DCA4')
}

# Need to update years because cause and risk data sets have data available for different sets of years
valid_years <- function(display) {
  if (display == "cause") {
    years <- CAUSE_YEARS
  }
  else {
    years <- RISK_YEARS
  }
  return(years)
}

requirements <- function(display, years) {
  req(years[1] != years[2])
  if (req(display) == "risk") {
    req(years[1] %in% RISK_YEARS)
    req(years[2] %in% RISK_YEARS)
  }
  else {
    req(years[1] %in% CAUSE_YEARS)
    req(years[2] %in% CAUSE_YEARS)
  }
}


# SHINY-----------------------------------------------------------------------

# UI---------------------------------------------------------------------------
ui <- fluidPage(
  sidebarLayout(
    # Inputs: Select variables to plot
    sidebarPanel(
      
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
                  choices = list("Deaths" = 1, "DALYs (Disability-Adjusted Life Years)" = 2,
                                 "YLDs (Years Lived with Disability)" = 3, "YLLs (Years of Life Lost)" = 4),
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
    ),
    # Output: Show diagram
    mainPanel(
      visNetworkOutput("network")
    )
  )
)

# Server---------------------------------------------------------------------------

server <- function(input, output) {
  #  -----------------------------------------------------------------------
  # Years input must be defined in server because it is reactive to the Display input.
  # (Risk and Cause have different years of available data)
  output$available_years <- renderUI({
    sliderTextInput("year",
                    label = h4("Years:"),
                    choices = valid_years(input$display), # sort(unique(c(nodes_and_edges$years))),
                    selected = range(valid_years(input$display)), # range(nodes_and_edges$years),
                    grid = TRUE)
  })
  
  output$network <- renderVisNetwork({

    # We must require data input is valid before outputting plot so that temporary
    # error messages don't appear during processing lag time.
    requirements(input$display, input$year)

    nodes_and_edges <- create_nodes(input$level, input$measure, input$sex, input$metric,
                                    input$year[1], input$year[2], input$display, input$num_nodes)
    nodes <- nodes_and_edges$nodes
    edges <- nodes_and_edges$edges

    vis_network(nodes, edges,
                subtitle = paste(switch(input$metric,
                                        '1' = 'Number',
                                        '2' = 'Percent',
                                        '3' = 'Rate (per 100,000)'),
                                 switch(input$measure,
                                        '1' = 'Deaths',
                                        '2' = 'DALYs',
                                        '3' = 'YLDs',
                                        '4' = 'YLLs'),
                                 switch(input$sex,
                                        '1' = 'Males',
                                        '2' = 'Females',
                                        '3' = 'Both Sexes'),
                                 sep = ", "),
                input$display
    )
  })
}

shinyApp(ui = ui, server = server)
