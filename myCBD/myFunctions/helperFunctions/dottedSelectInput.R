dottedSelectInput <- function(inputId, label, choices, height = "500px") {
  list(
    tags$style(HTML(paste0(
      sep = "\n",
      ".selectize-dropdown {",
      "  width: auto !important;",
      "  min-width: 100% !important;",
      "  white-space: nowrap;",
      paste0("  height: ", height, " !important;"),   # height of the dropdown
      "}",
      ".selectize-dropdown > ul {",
      "  margin: 0 !important;",
      "}",
      ".selectize-dropdown [data-selectable] {",
      "  padding: 5px 15px !important;",
      "}",
      ".selectize-dropdown-content {",
      "  height: 100% !important;",
      "  max-height: 100% !important",
      "}",
      ".selectize-dropdown-content ul {",
      "  padding-left: 15px;",
      "  margin-bottom: 0;",
      "  list-style: none;",
      "  line-height: 18px;",
      "}",
      ".selectize-dropdown-content > ul {",                   # Level 0 font size, color, weight
      "  font-size: 18px;",                              
      "  font-weight: bold;",
      "  color: rgb(0,0,0);",
      "}",
      ".selectize-dropdown-content > ul > ul {",              # Level 1 font size, color, weight
      "  font-size: 16px;",
      "  font-weight: bold;",
      "  color: rgb(23,78,134);",
      "}",
      ".selectize-dropdown-content > ul > ul > ul {",          # Level 2 font size, color, weight 
      "  font-size: 14px;",
      "  font-weight: bold;",
      "  color: rgb(0,0,0);",
      "}",
      ".selectize-dropdown-content > ul > ul > ul > ul {",     # Level 3 font size, color, weight  
      "  font-size: 14px;",
      "  font-weight: normal;",
      "  color: rgb(23,78,134);",
      "}",
      ".selectize-dropdown-content li {",
      "  font-size: inherit;",
      "  color: inherit;",
      "}"
    ))),
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

