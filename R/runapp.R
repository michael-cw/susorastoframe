#' Start the Survey Solutions Raster-to-Frame Application
#'
#' @description A simple shiny application which allows to update a spatial polygons area sampling frame with one ore more
#' raster layers, which can be used for PPS sampling in the susospatsample app. Moreover, the application allows
#' also for the creation of a composite measure of size, using several input raster for a single MOS.
#'
#' @details
#' This application is part of the large set of tools, to facilitate survey implementation with
#' [Survey Solutions](https://docs.mysurvey.solutions/). The application is based on the
#' [terra](https://cran.r-project.org/web/packages/terra/index.html) and the
#' [sf](https://cran.r-project.org/web/packages/sf/index.html) package.
#'
#' @inherit shiny::runApp
#'
#'
#'
#' @export


runRasToFrameApp <- function(launch.browser = T) {
  # add resource path to www
  #addResourcePath("www", file.path(getwd(), "www"))
  shiny::addResourcePath("www", system.file("www", package = "susorastoframe"))
  # get original options
  original_options <- list(
    shiny.maxRequestSize = getOption("shiny.maxRequestSize")
  )
  # change options and revert on stop
  changeoptions <- function() {
    options(
      shiny.maxRequestSize = 500*1024^2
    )
    # shiny::shinyOptions(shiny.maxRequestSize=5000*1024^2)

    # revert to original state at the end
    shiny::onStop(function() {
      if (!is.null(original_options)) {
        options(original_options)
      }
    })
  }
  # create app & run
  appObj<-shiny::shinyApp(ui = main_ui, server = main_server, onStart = changeoptions)
  shiny::runApp(appObj, launch.browser = launch.browser, quiet = T)
}

