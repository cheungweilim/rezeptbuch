library(jsonlite)
library(htmltools)
library(readxl)

generate_ingredient_table <- function(ingredients) {
  # Convert the ingredient data to JSON for use in JavaScript
  json <- toJSON(ingredients[, c("id", "Quantity")], auto_unbox = TRUE)
  
  # Create the input box and ingredient table with German column headers
  html_content <- tagList(
    tags$div(
      tags$label("Anzahl der Portionen:", `for` = "servings"),  # Translate label
      tags$input(id = "servings", type = "number", value = "4", min = "1", oninput = "scaleIngredients()"),
      tags$div(id = "baseQuantities", style = "display:none;", json)
    ),
    tags$table(
      tags$thead(
        tags$tr(
          tags$th("Zutat"),  # Ingredient -> Zutat
          tags$th("Menge"),  # Quantity -> Menge
          tags$th("Einheit"),  # Unit -> Einheit
          tags$th("Kommentar")  # Comment -> Kommentar
        )
      ),
      tags$tbody(
        lapply(seq_len(nrow(ingredients)), function(i) {
          tags$tr(
            tags$td(ingredients$Ingredient[i]),
            tags$td(id = sprintf("quantity-%d", ingredients$id[i]), ingredients$Quantity[i]),
            tags$td(ingredients$Unit[i]),
            tags$td(ingredients$Comment[i])  # Use the Comment column from the ingredients data
          )
        })
      )
    )
  )
  
  htmltools::browsable(html_content)
}