#` K-means Clustering function
#'
#'
#'
#' @keywords internal
#' @noRd
#'

# clustering function
add_kmeans_group <- function(sf_multipolygon, num_clusters, vars_to_cluster, mindist = TRUE) {
  # Ensure that sf_multipolygon is an sf object
  # if (!inherits(sf_multipolygon, "sf")) {
  #   sf_multipolygon <- st_as_sf(sf_multipolygon)
  # }
  # Ensure that vars_to_cluster is a character vector
  if (!is.character(vars_to_cluster)) {
    stop("vars_to_cluster must be a character vector of variable names.")
  }

  # Check if the specified variables exist in the sf object
  if (!all(vars_to_cluster %in% names(sf_multipolygon))) {
    stop("Not all variables specified are in the sf object.")
  }

  # Extract the variables for clustering
  data_to_cluster <- sf_multipolygon %>% select(any_of(vars_to_cluster)) %>% st_set_geometry(NULL)

  # Perform k-means clustering
  #set.seed(123)  # Set seed for reproducibility
  kmeans_result <- kmeans(data_to_cluster, centers = num_clusters, nstart = 20)

  # Add the resulting cluster as a new column to the sf object
  sf_multipolygon$group <- kmeans_result$cluster

  # Minimize distance
  if (mindist) {
    # Calculate centroids of each cluster
    centroids <- sf_multipolygon %>%
      group_by(group) %>%
      summarise(geometry = st_centroid(st_union(geometry)))

    # Calculate distance matrix between centroids
    distance_matrix <- st_distance(centroids)

    # Perform hierarchical clustering on the distance matrix
    hc <- hclust(as.dist(distance_matrix))

    # Cut the tree to obtain 'num_clusters' clusters considering spatial proximity
    groups_spatial <- cutree(hc, k = num_clusters)

    # Create a mapping from original to spatially-refined clusters
    cluster_mapping <- centroids$group %>% setNames(groups_spatial)

    # Update the group assignment with spatially refined clusters
    sf_multipolygon$group <- cluster_mapping[sf_multipolygon$group]
  }

  # Return the modified sf object
  return(sf_multipolygon)
}

#` Remove isolated polygons function
#'
#'
#'
#' @keywords internal
#' @noRd
#'

remove_isolated_polygons <- function(sf_multipolygon) {
  # Ensure that sf_multipolygon is an sf object
  if (!inherits(sf_multipolygon, "sf")) {
    stop("Input must be an 'sf' object.")
  }

  # Calculate neighbors for each polygon
  neighbors <- st_touches(sf_multipolygon)

  # Identify indices of polygons with no neighbors
  isolated_indices <- which(sapply(neighbors, length) == 0)

  # If there are isolated polygons, remove them
  if (length(isolated_indices) > 0) {
    sf_multipolygon <- sf_multipolygon[-isolated_indices, ]
  }

  return(sf_multipolygon)
}


#` SKATER Clustering function
#'
#'
#'
#' @keywords internal
#' @noRd
#'
add_skater_group <- function(sf_multipolygon, num_clusters, vars_to_cluster, restr.val = NULL, restr.var = NULL) {
  # Ensure that vars_to_cluster is a character vector
  if (!is.character(vars_to_cluster)) {
    stop("vars_to_cluster must be a character vector of variable names.")
  }

  # Check if the specified variables exist in the sf object
  if (!all(vars_to_cluster %in% names(sf_multipolygon))) {
    stop("Not all variables specified are in the sf object.")
  }


  # Ensure that sf_multipolygon is an sf object
  if (!inherits(sf_multipolygon, "sf")) {
    sf_multipolygon <- st_as_sf(sf_multipolygon)
  }

  # Remove isolated polygons
  sf_multipolygon<-remove_isolated_polygons(sf_multipolygon)

  # Extract the variables for clustering
  data_to_cluster <- sf_multipolygon %>%
    select(any_of(vars_to_cluster)) %>%
    st_set_geometry(NULL)


  # Create a spatial weights matrix
  neighbors <- poly2nb(as_Spatial(sf_multipolygon))
  # Remove disjoint subgraphs
  res<-n.comp.nb(neighbors)
  if (res$nc>1) {
    maingraph<-raster::modal(res$comp.id)
    sf_multipolygon$graphid<-res$comp.id
    sf_multipolygon<-sf_multipolygon %>% filter(graphid==maingraph)

    # Extract the variables for clustering
    data_to_cluster <- sf_multipolygon %>%
      select(any_of(vars_to_cluster)) %>%
      st_set_geometry(NULL)

    neighbors <- poly2nb(as_Spatial(sf_multipolygon))

  }
  #weights <- nb2listw(neighbors, style = "B", zero.policy = F)
  costs<-nbcosts(neighbors, data_to_cluster)
  nb_edge<-nb2listw(neighbors, costs, style = "B")
  nb_edge<-spdep::mstree(nb_edge)

  # Perform spatially constrained clustering
  # i. unconstrained
  if(is.null(restr.val) | is.null(restr.var)){
    skater_result <- skater(
      edges = nb_edge,
      data = data_to_cluster,
      ncuts = num_clusters
    )
  } else {
    skater_result <- skater(
      edges = nb_edge,
      data = data_to_cluster,
      ncuts = num_clusters,
      restr.val,
      sf_multipolygon[[restr.var]]
    )
  }


  # Add the resulting cluster as a new column to the sf object
  sf_multipolygon$group <- skater_result$group

  # Return the modified sf object
  return(sf_multipolygon)
}

# # 4.3. skater examples
# # 4.3.1. add vector of ones for min number
# adm2_lc_pop$ones<-1
# adm1_lc_pop$ones<-1
# # 4.3.2. actuall skater
# adm2_lc_pop_dist<-add_skater_group(adm2_lc_pop, 5, c("ag_land_share", "pop", "bfcount"), restr.val = 30, restr.var = "ones")
# adm1_lc_pop_dist<-add_skater_group(adm1_lc_pop, 5, c("ag_land_share", "pop", "bfcount"), restr.val = 2, restr.var = "ones")
