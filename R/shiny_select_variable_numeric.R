#` Shiny module to select numeric variables
#'
#'
#'
#' @keywords internal
#' @noRd

selectVariablesUI <- function(id, label = "Select Variables") {
  ns <- NS(id)
  tagList(
    actionButton(ns("select_btn"), label),
    tags$head(
      tags$style(HTML("
        #shiny-notification-panel .shiny-notification {
          top: 0;
          right: 0;
          left: auto;
          bottom: auto;
          position: fixed;
        }
      "))
    )
  )
}

selectVariablesServer <- function(id, sfDataFrame) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input$select_btn, {
      sfdf <- req(sfDataFrame())
      # Transform to sf if not sf
      if (!inherits(sfdf, "sf")) {
        sfdf <- sf::st_as_sf(sfdf)
      }

      # Update choices every time the button is clicked
      updateSelectizeInput(session, "var_select", choices = names(sfdf)[!grepl("geometry", names(sfdf))])

      showModal(modalDialog(
        title = "Select Variables",
        selectizeInput(ns("var_select"), "Choose Variables", choices = names(sfdf)[!grepl("geometry", names(sfdf))], multiple = TRUE),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_btn"), "Confirm")
        ),
        easyClose = TRUE,
        size = "m"
      ))
    })

    validatedVars <- reactiveVal(NULL)

    observeEvent(input$confirm_btn, {
      selectedVars <- req(input$var_select)
      invalidVars <- character(0)

      sfdf <- req(sfDataFrame())
      # Transform to sf if not sf
      if (!inherits(sfdf, "sf")) {
        sfdf <- sf::st_as_sf(sfdf)
      }

      for (var in selectedVars) {
        if (!is.numeric(sfdf[[var]]) || any(is.na(sfdf[[var]]))) {
          invalidVars <- c(invalidVars, var)
        }
      }

      if (length(invalidVars) == 0) {
        validatedVars(selectedVars)
      } else {
        invalidVarsStr <- paste(invalidVars, collapse = ", ")
        showNotification(
          paste("Invalid selection for variables:", invalidVarsStr, "- must be numeric and contain no NAs"),
          type = "error", duration = NULL)
      }
      removeModal()
    })

    return(validatedVars)
  })
}



# library(shiny)
# library(sf)
#
# ui <- fluidPage(
#   selectVariablesUI("var_selector")
# )
#
# server <- function(input, output, session) {
#   sfDataFrame <- data.frame(
#     x = letters[1:10],
#     y = 1:10,
#     z = 1:10
#   )
#   selectedVars <- selectVariablesServer("var_selector", sfDataFrame)
#
#   observe({
#     vars <- selectedVars()
#     if (!is.null(vars)) {
#       # Do further processing with the selected variables
#     }
#   })
# }
#
# shinyApp(ui, server)
