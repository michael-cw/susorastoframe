#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom data.table .BY
#' @importFrom data.table .EACHI
#' @importFrom data.table .GRP
#' @importFrom data.table .I
#' @importFrom data.table .N
#' @importFrom data.table .NGRP
#' @importFrom data.table .SD
#' @importFrom data.table :=
#' @importFrom data.table data.table
#'
#' @import shiny
#' @importFrom dplyr all_of any_of filter group_by rename select summarise
#' @importFrom leaflet leafletOutput renderLeaflet
#' @importFrom mapview mapview
#' @importFrom sf as_Spatial st_as_sf st_centroid st_distance st_set_geometry st_touches st_union st_write
#' @importFrom spdep mstree n.comp.nb nb2listw nbcosts poly2nb skater
#' @importFrom stats as.dist cutree hclust kmeans setNames
#' @importFrom terra extract modal rast vect
#' @importFrom tidyr all_of any_of
# #' @importFrom tidyterra filter group_by rename select summarise
#' @importFrom utils unzip zip

## usethis namespace: end
NULL



utils::globalVariables(c("group", "geometry", "graphid"))
