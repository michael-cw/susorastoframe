#` Shiny UI
#'
#'
#'
#' @keywords internal
#' @noRd


main_ui<-function(request){
  fpwww <- system.file("www", package = "susorastoframe")
  fluidPage(
    tags$header(
      style = "padding-bottom: 0px; background-color: #002244; color: white; text-align: center; height: 5vh",
      div(
        style = "float: left;",
        img(src = file.path("www", "logoWBDG.png"), height = "63vh")  # Adjust image path and size
      ),
      h2("Survey Solutions Raster-to-Frame Application", style = "margin-left: 60px;")  # Adjust margin to align with your image
    ),
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
