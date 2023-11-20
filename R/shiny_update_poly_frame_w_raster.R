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
    numericInput(ns("cluSEED"), "Set Seed", value = floor(stats::runif(1, 1000, 9999)), min = 0, step = 1),
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
    # reactive val for seed
    cluster_seed <- reactiveVal(NULL)

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
        color = "rgba(13, 71, 161, 0.7)",
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

        # check if varname_new contains NA and if so, show notification and replace with 0
        if(any(is.na(adm_lc[[vaname_new]]))){
          showNotification("Extracted raster values contains NA values for some areas. Replacing with 0.", type = "warning")
          adm_lc[[vaname_new]][is.na(adm_lc[[vaname_new]])]<-0
        }

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
      req(input$clusters>0)
      req(input$cluSEED>0)

      waiter::waiter_show(
        color = "rgba(13, 71, 161, 0.7)",
        html = tagList(
          waiter::spin_fading_circles(),
          "Kmeans Cluster Creation ..."
        )
      )
      # Execute clustering
      tryCatch({
        # Convert to sf
        adm_lc <- adm_lc %>% st_as_sf()

        # set seed for reproducibility
        set.seed(input$cluSEED)
        cluster_seed(input$cluSEED)

        # Perform k-means clustering
        reactive_values$clustered <- add_kmeans_group(adm_lc, input$clusters, sprintf("var_%d", 1:counter()))

        # Plot results
        output$map <- leaflet::renderLeaflet({
          req(reactive_values$clustered)
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
      waiter::waiter_hide()
    })

    # create spatially constrained (Skater) cluster with add_skater_group
    observeEvent(input$cluster_btn_sp, {
      adm_lc<-req(reactive_values$adm)
      req(counter()>0)
      req(input$cluSEED>0)
      req(input$clusters>0)

      waiter::waiter_show(
        color = "rgba(13, 71, 161, 0.7)",
        html = tagList(
          waiter::spin_fading_circles(),
          "Spatial Cluster Creation ..."
        )
      )
      # Execute clustering
      tryCatch({
        # Convert to sf
        adm_lc <- adm_lc %>% st_as_sf()

        # set seed for reproducibility
        set.seed(input$cluSEED)
        cluster_seed(input$cluSEED)


        # Perform skater clustering
        adm_lc$ones<-1
        reactive_values$clustered <- add_skater_group(adm_lc, input$clusters-1, sprintf("var_%d", 1:counter()),
                                                      restr.val = round((nrow(adm_lc)/input$clusters)/2), restr.var = "ones")

        # Plot results
        output$map <- leaflet::renderLeaflet({
          req(reactive_values$clustered)
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
      waiter::waiter_hide()
    })

    # Composite Size Measure Calculation
    MOSvars<-selectVariablesServer("composite_btn", reactive(reactive_values$adm))

    output$download_shapefile <- downloadHandler(
      filename = function() {
        paste("clustered_data", Sys.Date(),"_seed_", cluster_seed(), ".zip", sep = "")
      },
      content = function(file) {
        req(reactive_values$clustered)
        # Define the temporary directory to save the shapefiles
        shp_dir <- file.path(tempdir(), "download")
        # if dir does not exist, create
        if(!dir.exists(shp_dir)) dir.create(shp_dir, recursive = TRUE, showWarnings = FALSE)
        # shape file path
        shp_path<-file.path(shp_dir, "clustered_shapefile.shp")

        # Save the sf object as a shapefile
        sf::st_write(reactive_values$clustered, shp_path, delete_layer = TRUE)

        # Zip the shapefile
        zipfile <- file.path(shp_dir, "zipfile.zip")
        zip::zip(zipfile,
                 files = list.files(shp_dir, full.names = TRUE, pattern = "(.dbf$)|(.prj$)|(.shp$)|(.shx$)"),
                 mode = "cherry-pick")

        # Copy the zipfile to the download location
        file.copy(zipfile, file, overwrite = TRUE)

        # Delete the temporary directory
        unlink(shp_dir, recursive = TRUE)
      }
    )

    output$download_df <- downloadHandler(
      filename = function() {
        paste("clustered_data_tabular", Sys.Date(),"_seed_", cluster_seed() ,".zip", sep = "")
      },
      content = function(file) {
        req(reactive_values$clustered)
        # Define the temporary directory to save the shapefiles
        shp_dir <- file.path(tempdir(), "download")
        # if dir does not exist, create
        if(!dir.exists(shp_dir)) dir.create(shp_dir, recursive = TRUE, showWarnings = FALSE)
        # shape file path
        shp_path <- file.path(shp_dir, "clustered_shapefile.csv")

        # Save the dataframe object as a csv with fwrite
        data.table::fwrite(reactive_values$clustered %>% sf::st_set_geometry(NULL), shp_path)

        # Zip the shapefile
        zipfile <- file.path(shp_dir, "zipfile.zip")
        zip::zip(zipfile, files = shp_path, mode = "cherry-pick")

        # Copy the zipfile to the download location
        file.copy(zipfile, file, overwrite = TRUE)

        # Delete the temporary directory
        unlink(shp_dir, recursive = TRUE)
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
