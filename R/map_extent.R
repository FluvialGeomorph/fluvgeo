#' @title Calculate Expanded Map Extent
#'
#' @description Creates a new expanded map extent based on a supplied feature
#' and a map extent multiplying factor.
#'
#' @export
#' @param feature             sf; A feature class.
#' @param extent_factor       numeric; A numeric value used to expand the extent
#'                            of the supplied feature class.
#'
#' @return an sf::st_bbox object
#'
#' @importFrom sf st_bbox
#'
map_extent <- function(feature, extent_factor = 1) {
  # Check parameters
  assert_that(class(feature)[1] == "sf",
              msg = "feature must be an `sf` object.")
  assert_that(is.numeric(extent_factor) && length(extent_factor) == 1,
              msg = "extent_factor must be a single numeric value")

  # Calculate feature extent
  feature_extent <- sf::st_bbox(feature)


  # Calculate center of the feature extent
  x_center <- unname(((feature_extent$xmax - feature_extent$xmin) / 2) +
                       feature_extent$xmin)
  y_center <- unname(((feature_extent$ymax - feature_extent$ymin) / 2) +
                       feature_extent$ymin)

  # Determine the extent furthest from the center
  x_center_dist <- unname(feature_extent$xmax - x_center)
  y_center_dist <- unname(feature_extent$ymax - y_center)
  max_dim <- max(c(x_center_dist, y_center_dist))

  # Create the new expanded extent
  xmin <- x_center - (max_dim * extent_factor)
  ymin <- y_center - (max_dim * extent_factor)
  xmax <- x_center + (max_dim * extent_factor)
  ymax <- y_center + (max_dim * extent_factor)
  extent <- c("xmin" = xmin,
              "ymin" = ymin,
              "xmax" = xmax,
              "ymax" = ymax)

  # Calculate expanded feature extent
  feature_map_extent <- sf::st_bbox(extent,
                                    crs = sf::st_crs(feature))
  return(feature_map_extent)
}
