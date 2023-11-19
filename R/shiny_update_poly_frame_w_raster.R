#` Shiny module to extract raster data within polygons
#'
#'
#'
#' @keywords internal
#' @noRd


# Module UI
geospatialUIside <- function(id) {
  fpwww <- system.file("www", package = "susorastoframe")
  style<-sass::sass(sass::sass_file(file.path(fpwww, "styles.scss")))
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style((style))
    ),
    fileInput(ns("shapefile"), "Upload Shapefile (zip containing .shp and associated files)", accept = ".zip"),
    fluidRow(
      column(6,
             fileInput(ns("rasterfile"), "Upload Raster (.tif)", accept = c('.tif', '.tiff'))

      ),
      column(6,
             selectInput(ns("extractfun"), "Extract Function", choices = c("mean", "sum"), multiple = FALSE)
      )
    ),
    # show table of extracted raster files
    fluidRow(
             shiny::dataTableOutput(ns("raster_a"))
    ),
    br(),br(),
    actionButton(ns("extract_btn"), "Extract Data"),
    actionButton(ns("cluster_btn"), "K-means Clustering"),
    actionButton(ns("cluster_btn_sp"), "Spatial Clustering"),
    selectVariablesUI(ns("composite_btn"), "Composite MOS"),
    numericInput(ns("clusters"), "Number of clusters", 9),
    downloadButton(ns("download_shapefile"), "Download Clustered Shapefile"),
    downloadButton(ns("download_df"), "Download Clustered Table (.csv)")

  )
}

geospatialUImain <- function(id) {
  ns <- NS(id)
  tagList(
    # tags$head(
    #   tags$style(HTML("
    #   .dataTables_wrapper tfoot {
    #     display: none;
    #   }
    # "))
    # ),
    leaflet::leafletOutput(ns("map_shape")),
    br(),
    leaflet::leafletOutput(ns("map"))
  )
}

# Module Server
geospatialServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive_values <- reactiveValues(adm = NULL, raster = NULL, clustered = NULL)

    observeEvent(input$shapefile, {
      # 1. Load only the shape file
      inFile <- input$shapefile
      # create/clear temporary directory
      up_path <- file.path(tempdir(), "upload")
      dir.create(up_path, recursive = TRUE, showWarnings = FALSE)
      unlink(file.path(up_path, "*"), recursive = TRUE, force = TRUE)
      # Unzip the shapefile
      utils::unzip(inFile$datapath, exdir = up_path)
      shapefiles <- list.files(up_path, pattern = "\\.shp$", full.names = TRUE)
      print(shapefiles)
      if (length(shapefiles) != 1) {
        stop("The zip file must contain exactly one shapefile.")
      }

      # Read the shapefile
      reactive_values$adm <- vect(shapefiles[1])
      # Set counter to 0 with each shape upload
      counter(0)
      print(input$extractfun)
    })

    # counter for variable names
    counter<-reactiveVal(0)
    observeEvent(input$rasterfile, {
      # 1. Load only the raster file
      rasterFile <- input$rasterfile
      reactive_values$raster <- rast(rasterFile$datapath)
      counter(counter()+1)
    })

    output$map_shape <- leaflet::renderLeaflet({
      # 2. Plot the shape file
      shapein<-req(reactive_values$adm)
      shapein<-st_as_sf(shapein)
      shapein$group<-rownames(shapein)
      mapview(shapein, zcol = "group")@map
    })

    # reactive val function for raster_a input dataframe
    raster_aDF<-reactiveVal(data.frame("RasterFile" = character(0), "ExtractFunction" = character(0)))

    observeEvent(input$extract_btn, {
      req(reactive_values$adm)
      req(reactive_values$raster)
      rasdf_old<-req(raster_aDF())
      waiter::waiter_show(
        html = tagList(
          waiter::spin_fading_circles(),
          "Extracting Raster Cells ..."
        )
      )
      tryCatch({
        # Extract raster values
        varname<-names(reactive_values$raster)
        vaname_new<-sprintf("var_%d", counter())
        if(input$extractfun=="mean"){
          adm_lc <- terra::extract(reactive_values$raster, reactive_values$adm, fun = mean, na.rm = TRUE, exact = FALSE, bind = TRUE)
        } else if (input$extractfun=="sum"){
          adm_lc <- terra::extract(reactive_values$raster, reactive_values$adm, fun = sum, na.rm = TRUE, exact = FALSE, bind = TRUE)
        }
        adm_lc <- adm_lc %>% tidyterra::rename({{vaname_new}} := dplyr::all_of(varname))
        reactive_values$adm <- adm_lc
        # add raster file name and extract function to raster_a dataframe
        raster_aDF(rbind(rasdf_old, data.frame("RasterFile" = input$rasterfile$name, "ExtractFunction" = input$extractfun)))

      }, error = function(e) {
        # If there's an error in processing, return NULL
        reactive_values$clustered <- NULL
        showModal(modalDialog(
          title = "Error",
          paste("An error has occurred:", e$message)
        ))
      })
      waiter::waiter_hide()
    })

    output$raster_a <- shiny::renderDataTable({
      # create table of raster file names and extract function used after extraction is complete
      rasdf<-req(raster_aDF())
      return(rasdf)
    }, options = list(dom = 't'))

    # create kmeans cluster with add_kmeans_group
    observeEvent(input$cluster_btn, {
      adm_lc<-req(reactive_values$adm)
      req(counter()>0)

      # Execute extraction and clustering
      tryCatch({
        # Extract raster values
        # adm_lc <- terra::extract(reactive_values$raster, reactive_values$adm, fun = mean, na.rm = TRUE, exact = FALSE, bind = TRUE)
        # adm_lc <- adm_lc %>% rename(ag_land_share = label_mean)
        adm_lc <- adm_lc %>% st_as_sf()

        # Perform k-means clustering
        reactive_values$clustered <- add_kmeans_group(adm_lc, input$clusters, sprintf("var_%d", 1:counter()))

        # Plot results
        output$map <- leaflet::renderLeaflet({
          mapview(reactive_values$clustered, zcol = "group")@map
        })

      }, error = function(e) {
        # If there's an error in processing, return NULL
        reactive_values$clustered <- NULL
        showModal(modalDialog(
          title = "Error",
          paste("An error has occurred:", e$message)
        ))
      })
    })

    # create spatially constrained (Skater) cluster with add_skater_group
    observeEvent(input$cluster_btn_sp, {
      adm_lc<-req(reactive_values$adm)
      req(counter()>0)

      # Execute extraction and clustering
      tryCatch({
        # Extract raster values
        # adm_lc <- terra::extract(reactive_values$raster, reactive_values$adm, fun = mean, na.rm = TRUE, exact = FALSE, bind = TRUE)
        # adm_lc <- adm_lc %>% rename(ag_land_share = label_mean)
        adm_lc <- adm_lc %>% st_as_sf()

        # Perform k-means clustering
        adm_lc$ones<-1
        reactive_values$clustered <- add_skater_group(adm_lc, input$clusters-1, sprintf("var_%d", 1:counter()),
                                                      restr.val = round((nrow(adm_lc)/input$clusters)/2), restr.var = "ones")

        # Plot results
        output$map <- leaflet::renderLeaflet({
          mapview(reactive_values$clustered, zcol = "group")@map
        })

      }, error = function(e) {
        # If there's an error in processing, return NULL
        reactive_values$clustered <- NULL
        showModal(modalDialog(
          title = "Error",
          paste("An error has occurred:", e$message)
        ))
      })
    })

    # Composite Size Measure Calculation
    MOSvars<-selectVariablesServer("composite_btn", reactive(reactive_values$adm))

    output$download_shapefile <- downloadHandler(
      filename = function() {
        paste("clustered_data", Sys.Date(), ".zip", sep = "")
      },
      content = function(file) {
        req(reactive_values$clustered)
        # Define the temporary directory to save the shapefiles
        shp_dir <- tempdir()
        shp_path <- file.path(shp_dir, "clustered_shapefile.shp")
        dir.create(shp_path)

        # Save the sf object as a shapefile
        st_write(reactive_values$clustered, shp_path, delete_layer = TRUE)

        # Zip the shapefile
        zipfile <- paste0(shp_path, ".zip")
        zip::zip(zipfile, files = list.files(shp_path, full.names = TRUE), mode = "cherry-pick")

        # Copy the zipfile to the download location
        file.copy(zipfile, file, overwrite = TRUE)
      }
    )

    output$download_df <- downloadHandler(
      filename = function() {
        paste("clustered_data_tabular", Sys.Date(), ".zip", sep = "")
      },
      content = function(file) {
        req(reactive_values$clustered)
        # Define the temporary directory to save the shapefiles
        shp_dir <- tempdir()
        shp_path <- file.path(shp_dir, "clustered_shapefile.shp")
        dir.create(shp_path)

        # Save the sf object as a shapefile
        st_write(reactive_values$clustered, shp_path, delete_layer = TRUE)

        # Zip the shapefile
        zipfile <- paste0(shp_path, ".zip")
        zip::zip(zipfile, files = list.files(shp_path, full.names = TRUE), mode = "cherry-pick")

        # Copy the zipfile to the download location
        file.copy(zipfile, file, overwrite = TRUE)
      }
    )
  })
}

# # Main UI
#   ui <- fluidPage(
#     titlePanel("Geospatial Data Analysis and Clustering"),
#     waiter::use_waiter(),
#     sidebarLayout(
#       sidebarPanel(
#         geospatialUIside("geospatialModule1")
#       ),
#       mainPanel(
#         geospatialUImain("geospatialModule1")
#       )
#     )
#   )
#
# # Main Server
# server <- function(input, output, session) {
#   geospatialServer("geospatialModule1")
# }
#
# shinyApp(ui, server)
