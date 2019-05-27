# ADD visNetwork to "install packages" file
# Another change


# Load and format data-----------------------------------------------------------------------

endpoints <- read.csv(path(myPlace,"/myInfo/","IHME_API_endpoints.csv"), header = TRUE)

cause_data <- readRDS(path(myPlace,"/myData/cause_data.rds")) %>%
  rename('id_num' = 'cause_id', 'id_name' = 'cause_name', 'display' = 'acause') %>%
  mutate(display = 'cause')

risk_data <- readRDS(path(myPlace,"/myData/risk_data.rds")) %>%
  select(-cause_id) %>%
  rename('id_num' = 'risk_id', 'id_name' = 'risk_name', 'display' = 'risk_short_name') %>%
  mutate(display = 'risk')




cause_groups <- c('Communicable, maternal, neonatal, and nutritional diseases',
                  'Non-communicable diseases',
                  'Injuries')

risk_groups <- c('Environmental/ occupational risks',
                 'Behavioral risks',
                 'Metabolic risks')

cause_data$first_parent <- ifelse(cause_data$id_num %in% c(295:408), cause_groups[1],
                                  ifelse(cause_data$id_num %in% c(409:686), cause_groups[2],
                                         ifelse(cause_data$id_num >= 687, cause_groups[3],'0')))

risk_data$first_parent <- ifelse(risk_data$sort_order %in% c(2:34), risk_groups[1],
                                  ifelse(risk_data$sort_order %in% c(35:78), risk_groups[2],
                                         ifelse(risk_data$sort_order >= 79, risk_groups[3],'0')))

data <- bind_rows(cause_data, risk_data) %>%
  mutate(level = ifelse(id_num %in% setdiff(id_num, parent_id) & level == 2, paste(2,3,4, sep =","),
                        ifelse(id_num %in% setdiff(id_num, parent_id) & level == 3, paste(3,4, sep=","), level)))

# Define constants -----------------------------------------------------------------------
# Play with them at your own risk
MAX_NODES <- 25
LABEL_LENGTH <- 30
FONT_SIZE <- 15
TITLE_FONT_SIZE <- 15
WIDTH <- 800
HEIGHT <- '197%'
Y_SPACE_FACTOR <- 25
LEGEND_SPACE_FACTOR <- 35

CAUSE_YEARS <- sort(unique(cause_data$year_id))
RISK_YEARS <- sort(unique(risk_data$year_id))
EDGE_NODE_ADJUSTMENT <- LABEL_LENGTH*3.35
NODE_WIDTH_CONSTRAINT <- LABEL_LENGTH*7.2
LEGEND_NODE_WIDTH_CONSTRAINT <- 196
LEFT_X <- -(WIDTH/2 - 115)
RIGHT_X <- LEFT_X+350


# Create nodes function -----------------------------------------------------------------------
create_nodes <- function(level_in, measure_id_in, sex_id_in, metric_id_in,
                          year_from, year_to, display_in) {

  
  
  left_nodes <- data %>%
    filter(display == display_in, grepl(level_in, level), year_id == year_from,
           measure_id == measure_id_in, sex_id == sex_id_in, metric_id == metric_id_in) %>%
    arrange(id_num) %>%
    mutate(rank = rank(-val, ties.method = 'first')) %>%
    filter(rank <= MAX_NODES)
  
  right_nodes <- data %>%
    filter(display == display_in, grepl(level_in, level), year_id == year_to,
           measure_id == measure_id_in, sex_id == sex_id_in, metric_id == metric_id_in,
           id_name %in% left_nodes$id_name) %>%
    arrange(id_num) %>%
    mutate(rank = rank(-val, ties.method = 'first'))
  
  # Reset number of nodes in case it is less than original number of nodes requested
  NUM_NODES <- nrow(left_nodes)
  
  # Combine left and right nodes into selected_data
  selected_data <- bind_rows(left_nodes, right_nodes) %>%
    group_by(year_id) %>%
    mutate(rank2 = rank(rank))  # rank2 for y location of nodes
  selected_data$change <- rep(round((right_nodes$val-left_nodes$val)/left_nodes$val, 4), 2)
  
  # Four groups of nodes: label, edge, title, legend
  label_nodes <- data.frame(selected_data, id = (nrow(selected_data)+1):(2*nrow(selected_data)),
                            label = ifelse(nchar(paste(selected_data$rank, selected_data$id_name)) > LABEL_LENGTH,
                                           paste0(substr(paste(selected_data$rank, selected_data$id_name), 1, LABEL_LENGTH-4), "..."),
                            substr(paste(selected_data$rank, selected_data$id_name), 1, LABEL_LENGTH)),
                            margin = list('top' = 3, 'bottom' = 3, 'left' = 2, 'right' = -2),
                            widthConstraint = NODE_WIDTH_CONSTRAINT,
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
                            y = selected_data$rank2*Y_SPACE_FACTOR - (327))

  
  edge_nodes <- data.frame(id = 1:nrow(selected_data), hidden = TRUE, group = selected_data$first_parent,
                           x = c(rep(LEFT_X+EDGE_NODE_ADJUSTMENT, NUM_NODES),
                                 rep(RIGHT_X-EDGE_NODE_ADJUSTMENT, NUM_NODES)),
                           y = label_nodes$y)
  

  title_nodes <- data.frame(label = c("<b>California</b>",  # California Title is now off the chart. Can delete it here, or move it's y-coordinate down to show it.
                                      paste(switch(metric_id_in,
                                                   '1' = 'Number of total',
                                                   '2' = 'Percent of total',
                                                   '3' = 'Rate, per 100,000, of'),
                                            switch(measure_id_in,
                                                   '1' = 'Deaths',
                                                   '2' = 'Disability Adjusted Life Years',
                                                   '3' = 'Years Lived with Disability',
                                                   '4' = 'Years of Life Lost'),
                                            'for',
                                            switch(sex_id_in,
                                                   '1' = 'males',
                                                   '2' = 'females',
                                                   '3' = 'both sexes'),
                                            "in California",
                                            sep = " "),
                                      paste(year_from, "Rank"), paste(year_to, "Rank")), rank = c(-2,-1,0,0),
                            x = c(rep((LEFT_X+RIGHT_X)/2, 2), LEFT_X, RIGHT_X), y = c(-400, -365, 1-(650/2), 1-(650/2)), id = 0:-3, shape = 'text',
                            font = list(face = 'Bold', size = TITLE_FONT_SIZE))

  if (display_in == "cause") {
    groups <- data.frame(label = cause_groups,
                         y = c(0.7, 2.15, 3.15)*LEGEND_SPACE_FACTOR - (650/2),
                         margin = list('top' = 5, 'bottom' = c(-30, -16, -1), 'left' = 15, 'right' = -200))
  }
  else {
    groups <- data.frame(label = risk_groups,
                         y = c(0.7,1.7, 2.45)*LEGEND_SPACE_FACTOR - (650/2),
                         margin = list('top' = 5, 'bottom' = c(-15, 0, -1), 'left' = 15, 'right' = -200))
  }

  
  legend_nodes <- cbind(groups, data.frame(rank = c(1, 2, 3), shape = 'box', id = -4:-6,
                                           color = c('#E9A291', '#C6E2FF', '#A0DCA4'),
                                           font = list(face = 'Bold', size = TITLE_FONT_SIZE),
                                           widthConstraint = LEGEND_NODE_WIDTH_CONSTRAINT,
                                           x = RIGHT_X + NODE_WIDTH_CONSTRAINT/2 + 20))

  # suppressWarnings on this row bind because we want to ignore the coercing to character warnings.
  nodes <- suppressWarnings(bind_rows(title_nodes, label_nodes, edge_nodes, legend_nodes))
  
  edges <- data.frame(from = c(1:NUM_NODES, 1:(2*NUM_NODES)),
                      to = c((NUM_NODES+1):(2*NUM_NODES), (2*NUM_NODES+1):(4*NUM_NODES)),
                      dashes = ifelse(selected_data$rank[1:NUM_NODES] < selected_data$rank[(NUM_NODES+1):(2*NUM_NODES)],
                                      "[6,4.5]", "false"),
                      arrows = list(to = list(enabled = c(rep(TRUE, NUM_NODES), rep(FALSE, 2*NUM_NODES)), scaleFactor = 0.5)))



  return(list("nodes" = nodes, "edges" = edges))
}

# Create vis network (and supplemental) functions -----------------------------------------------------------------------
vis_network <- function(nodes, edges, display) {
  if (display == "cause") {
    groups <- cause_groups
  }
  else {
    groups <- risk_groups
  }

  visNetwork(nodes, edges) %>%
    visOptions(height = HEIGHT, width = WIDTH, autoResize = F) %>%
    visNodes(fixed = TRUE, shape = 'box', font = list(size = FONT_SIZE, align = 'left')) %>%
    visEdges(width = 1, smooth = FALSE, hoverWidth = 0) %>%
    visInteraction(hover = TRUE, hoverConnectedEdges = TRUE, zoomView = FALSE, dragView = FALSE, selectable = FALSE) %>%
    visGroups(groupname = groups[1], color = '#E9A291') %>%
    visGroups(groupname = groups[2], color = '#C6E2FF') %>%
    visGroups(groupname = groups[3], color = '#A0DCA4') %>%
    visHierarchicalLayout()
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
