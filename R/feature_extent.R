#' @title Calculate Expanded Feature Extent
#'
#' @description Creates a new expanded map extent based on a supplied
#' map extent multiplying factor.
#'
#' @export
#' @param feature             sp::Spatial* feature or sf; A feature class.
#' @param extent_factor       numeric; A numeric value used to expand the extent
#'                            of the supplied feature class.
#'
#' @return a raster::extent object
#'
#' @importFrom raster extent
#'
feature_extent <- function(feature, extent_factor = 1) {
  # Calculate feature extent
  feature_extent <- raster::extent(feature)

  # Calculate centroid of the feature extent
  x_center <- ((feature_extent@xmax - feature_extent@xmin) / 2) + feature_extent@xmin
  y_center <- ((feature_extent@ymax - feature_extent@ymin) / 2) + feature_extent@ymin

  # Determine the extent furthest from the centroid
  x_center_dist <- feature_extent@xmax - x_center
  y_center_dist <- feature_extent@ymax - y_center
  max_dim <- max(c(x_center_dist, y_center_dist))

  # Create the new expanded extent
  xmin <- x_center - (max_dim * extent_factor)
  xmax <- x_center + (max_dim * extent_factor)
  ymin <- y_center - (max_dim * extent_factor)
  ymax <- y_center + (max_dim * extent_factor)
  extent <- c(xmin, xmax, ymin, ymax)

  # Create a list of parameters for QA testing
  params <- list(x_center = x_center,
                 y_center = y_center,
                 x_center_dist = x_center_dist,
                 y_center_dist = y_center_dist,
                 max_dim = max_dim,
                 extent = extent)

  # Calculate expanded feature extent
  feature_map_extent <- raster::extent(extent)
  return(feature_map_extent)
}
