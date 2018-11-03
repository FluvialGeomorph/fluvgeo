#' @title Plot cross section profile
#'
#' @description Produces a cross section profile plot for the specified cross
#' section and displayes the specified bankfull elevation.
#'
#' @export
#' @param xs_points           data frame; a data frame of cross section points
#' @param stream              character; The name of the stream.
#' @param xs_number           integer; The cross section identifier of the
#'                            requested cross section.
#' @param bankfull_elevation  numeric; The detrended bankfull elevation (in
#'                            feet) that is used to calculate hydraulic
#'                            geometry.
#'
#' @return A ggplot2 object.
#'
#' @details This function is used to plot the cross section profile from a
#' \code{xs_points} data frame.
#'
#' @seealso The \code{xs_plot} function requires a \code{xs_points} dataframe.
#' See the \code{\link{sin_xs_points}} package dataset for an example of this
#' format of cross section data produced by the \code{FluvialGeomorph} ArcGIS
#' toolbox.
#'
#' @examples
#' # Extract attribute data from the fgm::sin_xs_points SpatialPointsDataFrame
#' sin_xs_points_df <- fgm::sin_xs_points@@data
#'
#' # Call the xs_plot function
#' sin_4_plot <- xs_plot(xs_points = sin_xs_points_df,
#'                       stream = "Sinsinawa",
#'                       xs_number = 4,
#'                       bankfull_elevation = 103.5)
#' # Print the graph
#' sin_4_plot
#'
#' @importFrom assertthat assert_that
#' @importFrom ggplot2 ggplot scale_y_continuous geom_line geom_hline theme_bw
#' theme labs aes sec_axis element_text
#'
xs_plot <- function(xs_points, stream, xs_number, bankfull_elevation) {
  # Check parameters
  assert_that(is.data.frame(xs_points),
              msg = "'xs_points' must be a data frame")
  assert_that("Seq" %in% colnames(xs_points),
              msg = "Required field 'Seq' is missing from 'xs_points'")
  assert_that("POINT_X" %in% colnames(xs_points),
              msg = "Required field 'POINT_X' is missing from 'xs_points'")
  assert_that("POINT_Y" %in% colnames(xs_points),
              msg = "Required field 'POINT_Y' is missing from 'xs_points'")
  assert_that("POINT_M" %in% colnames(xs_points),
              msg = "Required field 'POINT_M' is missing from 'xs_points'")
  assert_that("Watershed_Area_SqMile" %in% colnames(xs_points),
              msg = "Required field 'Watershed_Area_SqMile' is missing from
              'xs_points'")
  assert_that("km_to_mouth" %in% colnames(xs_points),
              msg = "Required field 'km_to_mouth' is missing from
              'xs_points'")
  assert_that("DEM_Z" %in% colnames(xs_points),
              msg = "Required field 'DEM_Z' is missing from 'xs_points'")
  assert_that("Detrend_DEM_Z" %in% colnames(xs_points),
              msg = "Required field 'Detrend_DEM_Z' is missing from
              'xs_points'")
  assert_that("ReachName" %in% colnames(xs_points),
              msg = "Required field 'ReachName' is missing from 'xs_points'")
  assert_that(is.character(stream) && nchar(stream) != 0 &&
                length(stream) == 1,
              msg = "stream must be a character vector of length one")
  assert_that(xs_number%%1 == 0 && length(xs_number) == 1,
              msg = "xs_number must be an integer vector of length one")
  assert_that(is.numeric(bankfull_elevation) &&
                length(bankfull_elevation) == 1,
              msg = "bankfull_elevation must be a numeric vector of
              length one")

  # Subset xs_points for the specified stream and cross section
  xs <- xs_points[xs_points$ReachName == stream & xs_points$Seq == xs_number,]
  # Calculate transform between detrend elevation and actual elevation
  eg <- mean(xs$DEM_Z - xs$Detrend_DEM_Z)
  # Draw the graph
  p <- ggplot(data = xs,
              aes(xs$POINT_M * 3.28084, xs$Detrend_DEM_Z, label = xs$Seq)) +
    scale_y_continuous(sec.axis = sec_axis(~. + eg,
                                           name = "Elevation (NAVD88 feet)")) +
    geom_line() +
    geom_hline(yintercept = bankfull_elevation, colour = "blue") +
    theme_bw() +
    theme(aspect.ratio = 2/5) +
    labs(title = paste("Cross Section ", as.character(xs_number)),
         x = "Station Distance (feet, from right descending bank)",
         y = "Detrended Elevation (feet)") +
    theme(plot.title = element_text(hjust = 0)
    )
  return(p)
}
