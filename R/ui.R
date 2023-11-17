#` Shiny UI
#'
#'
#'
#' @keywords internal
#' @noRd


main_ui<-function(request){
  fluidPage(
    titlePanel("Geospatial Data Analysis and Clustering"),
    waiter::use_waiter(),
    sidebarLayout(
      sidebarPanel(
        geospatialUIside("geospatialModule1")
      ),
      mainPanel(
        geospatialUImain("geospatialModule1")
      )
    )
  )
}
