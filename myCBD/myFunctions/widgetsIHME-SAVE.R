# Widgets

displayButtonInput <- function() {
  radioGroupButtons(
    "displayX",
    label = "Display:",
    choices = c("Cause" = "cause", "Risk" = "risk"),
    selected = "cause",
    justified = TRUE,
    status = "primary"
  )
}

levelSliderInput <- function() {
  sliderInput(
    "levelX",
    label = h4("Level:"),
    min = 0,
    max = 4,
    value = 1
  )
}

yearSliderInput <- function() {
  sliderInput(
    "yearZ",
    label = h4("Year:"),
    min = min(VALID_YEARS),
    max = max(VALID_YEARS),
    value = max(VALID_YEARS),
    sep = "",
    step = 1
    # animate = animationOptiointerval = 3000)
  )
}

yearRangeSliderInput <- function() {
  sliderTextInput(
    "yearX",
    label = h4("Years:"),
    choices = as.character(VALID_YEARS),
    selected = range(VALID_YEARS),
    grid = TRUE)
}


sexButtonInput <- function() {
  radioGroupButtons(
    "sexX",
    label = h4("Sex:"),
    choices = c(
      "Male" = 1,
      "Female" = 2,
      "Both" = 3
    ),
    selected = 3,
    justified = TRUE,
    status = "primary"
  )
}

metricButtonInput <- function() {
  radioGroupButtons(
    "metricX",
    label = h4("Metric:"),
    choices = c("#" = 1, "%" = 2, "Rate" = 3),
    selected = 1,
    justified = TRUE,
    status = "primary"
  )
}

measureSelectInput <- function() {
  selectInput(
    "measureX",
    label = h4("Measure:"),
    choices = c("Deaths" = 1,
                "Disability Adjusted Life Years (DALYs)" = 2,
                "Years Lived with Disability (YLDs)" = 3,
                "Years of Life Lost (YLLs)" = 4),
    selected = 1)
}

showtopSliderInput <- function() {
  sliderInput(
    "showtopX",
    label = h4("Show Top:"),
    min = 1,
    max = 10,
    value = 10
  )
}