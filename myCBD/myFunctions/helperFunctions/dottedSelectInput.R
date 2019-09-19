dottedSelectInput <- function(inputId, label, choices) {
  list(
    tags$style(
      ".selectize-dropdown {",
      "  width: auto !important;",
      "  min-width: 100% !important;",
      "  white-space: nowrap;",
      "}",
      ".selectize-dropdown ul {",
      "  margin: 0 !important;",
      "}",
      ".selectize-dropdown [data-selectable] {",
      "  padding: 5px 15px !important;",
      "}"
    ),
    selectizeInput(
      inputId = inputId,
      label = label,
      choices = choices,
      options = list(
        maxOptions = length(choices),
        render = I(
          paste(
            collapse = " ", sep = " ",
            "{ option: function(item, escape) { ",
            "if (item.label.substring(0, 1) !== '.') {",
            "return '<div>' + item.label + '</div>';",
            "}",
            "var dots = item.label.match(/^[.]+/)[0];",
            "var text = item.label.replace(/^[.]+/, '');",
            "var open = dots.replace(/[.]/g, '<ul>');",
            "var close = dots.replace(/[.]/g, '</ul>');",
            "return open + '<li><div>' + text + '</div></li>' + close;",
            "}, ",
            "item: function(item, escape) { ",
            "return '<div>' + item.label.replace(/^[.]+/, '') + '</div>';",
            "}" ,
            "}"
          )
        )
      )
    )
  )
}