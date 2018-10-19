#' @title Calculate goodness-of-fit statistics
#'
#' @description Calculates goodness-of-fit statistics beteween cross section
#'     geometry and regional hydraulic geometry dimensions.
#'
#' @export
#' @param xs_dims             data frame; a data frame of cross section
#'                            dimensions
#' @param streams             character vector; The stream names in the study
#'                            area.
#' @param regions             character; The regions that a dimension will be
#'                            calculated for. See the regional_curves$region
#'                            field for a complete list.
#' @param bankfull_elevations numeric vector; The bankfull elevations (units:
#'                            detrended feet) that are used to calculate
#'                            hydraulic geometry.
#'
#' @return A data frame of goodness of fit scores  of the relationship
#'    between cross section geometry and regional hydraulic geometry
#'    dimensions.
#'    \describe{
#'        \item{reach_name}{character; The name of the stream.}
#'        \item{region}{character; The name of the region.}
#'        \item{bankfull_elevation}{numeric; The detrended bankfull elevation
#'                       (in feet) that is used to calculate hydraulic
#'                       geometry.}
#'        \item{rmse_area}{numeric; The root-mean-square-error between the
#'                         regional curve derived area and the cross section
#'                         derived area.}
#'        \item{rmse_width}{numeric; The root-mean-square-error between the
#'                          regional curve derived width and the cross section
#'                          derived width.}
#'        \item{rmse_depth}{numeric; The root-mean-square-error between the
#'                          regional curve derived depth and the cross section
#'                          derived depth.}
#'        \item{mae_area}{numeric; The mean absolute error between the
#'                        regional curve derived area and the cross section
#'                        derived area.}
#'        \item{mae_width}{numeric; The mean absolute error between the
#'                         regional curve derived width and the cross section
#'                         derived width.}
#'        \item{mae_depth}{numeric; The mean absolute error between the
#'                         regional curve derived depth and the cross section
#'                         derived depth.}
#'    }
#'
#' @importFrom Metrics rmse mae
#' @importFrom assertthat assert_that
#'
build_gof_stats <- function(xs_dims, streams, regions, bankfull_elevations) {
  # Check parameters
  assert_that(is.data.frame(xs_dims),
              msg = "'xs_dims' must be a data frame")
  assert_that("reach_name" %in% colnames(xs_dims),
              msg = "Required field 'reach_name' is missing from 'xs_dims'")
  assert_that("cross_section" %in% colnames(xs_dims),
              msg = "Required field 'cross_section' is missing from 'xs_dims'")
  assert_that("xs_type" %in% colnames(xs_dims),
              msg = "Required field 'xs_type' is missing from 'xs_dims'")
  assert_that("bankfull_elevation" %in% colnames(xs_dims),
              msg = "Required field 'bankfull_elevation' is missing from
                    'xs_dims'")
  assert_that("drainage_area" %in% colnames(xs_dims),
              msg = "Required field 'drainage_area' is missing from 'xs_dims'")
  assert_that("xs_area" %in% colnames(xs_dims),
              msg = "Required field 'xs_area' is missing from 'xs_dims'")
  assert_that("xs_width" %in% colnames(xs_dims),
              msg = "Required field 'xs_width' is missing from 'xs_dims'")
  assert_that("xs_depth" %in% colnames(xs_dims),
              msg = "Required field 'xs_depth' is missing from 'xs_dims'")
  assert_that(is.character(streams) && length(streams) > 0,
              msg = "streams must be a character vector with at least one
              item")
  assert_that(is.character(regions) && length(regions) > 0,
              msg = "regions must be a character vector with at least one
              item")
  assert_that(is.numeric(bankfull_elevations) &&
                length(bankfull_elevations) > 0,
              msg = "bankfull_elevations must be a numeric vector with at
              least one item")

  # Create a list to hold the cross section gof stats
  xs_stats <- list()
  f <- 1
  # Iterage through streams
  for (g in streams) {
    # Iterate through regions
    for (h in regions) {
      # Iterate through bankfull elevations
      for (i in bankfull_elevations) {
        # Subset xs_dims for the current reach, region, and bankfull elevation
        xs_region <- xs_dims[xs_dims$reach_name == g &
                             xs_dims$xs_type == h &
                             xs_dims$bankfull_elevation == i, ]
        # Subset xs_dims for the current reach, cross section, and bankfull el.
        xs_dim <- xs_dims[xs_dims$reach_name == g &
                          xs_dims$xs_type == "DEM derived cross section" &
                          xs_dims$bankfull_elevation == i, ]
        # Calculate goodness of fit statistics
        rmse_area  <- rmse(actual    = xs_region$xs_area,
                           predicted = xs_dim$xs_area)
        rmse_width <- rmse(actual    = xs_region$xs_width,
                           predicted = xs_dim$xs_width)
        rmse_depth <- rmse(actual    = xs_region$xs_depth,
                           predicted = xs_dim$xs_depth)
        mae_area   <- mae(actual     = xs_region$xs_area,
                          predicted  = xs_dim$xs_area)
        mae_width  <- mae(actual     = xs_region$xs_width,
                          predicted  = xs_dim$xs_width)
        mae_depth  <- mae(actual     = xs_region$xs_depth,
                          predicted  = xs_dim$xs_depth)
        # Combine individual goodness of fit stats into a data frame
        gfs <- data.frame(g, h, i,
                          rmse_area, rmse_width, rmse_depth,
                          mae_area, mae_width, mae_depth,
                          stringsAsFactors = FALSE)
        colnames(gfs) <- c("reach_name", "region", "bankfull_elevation",
                           "rmse_area", "rmse_width", "rmse_depth",
                           "mae_area", "mae_width", "mae_depth")
        # Add gfs data frame to xs_stats list
        xs_stats[[f]] <- gfs
        f <- f + 1
      }
    }
  }
  # Append all of the xs_stats data frames
  gof_stats <- dplyr::bind_rows(xs_stats)
  return(gof_stats)
}
