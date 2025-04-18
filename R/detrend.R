#' @title Detrend DEM
#' @description  Converts a Digital Elevation Model (DEM) to a Relative
#'               Elevation Model (REM). An REM is a DEM normalized to the
#'               baseline elevation of the stream reach.
#' @param dem              terra SpatRast object; A DEM.
#' @param flowline         sf object; A flowline object.
#' @param flowline_points  sf object; A flowline_points feature.
#' @param buffer_distance  numeric; Distance the flowline feature class
#'                         will be buffered to define the extent of the output
#'                         REM. Units are defined by the coordinate system of
#'                         the DEM.
#'
#' @return a list containing two terra SpatRaster objects:
#' * rem   - A raster Relative Elevation Model (REM) representing elevation
#'           above the baseline elevation of the reach.
#' * trend - A raster trend surface representing the baseline elevation of
#'           the flowline_points for the reach.
#' @export
#'
#' @importFrom dplyr %>% rename select
#' @importFrom sf st_buffer
#' @importFrom terra crop interpIDW focal
#'
detrend <- function(dem, flowline, flowline_points, buffer_distance) {
  assert_that("sf" %in% class(flowline),
              msg = "flowline must be an sf object")
  assert_that("sf" %in% class(flowline_points),
              msg = "flowline_points must be an sf object")
  assert_that("SpatRaster" %in% class(dem),
              msg = "dem must be a SpatRaster object")
  assert_that(st_crs(flowline) == st_crs(dem),
              msg = "flowline and dem must have the same crs")
  assert_that(st_crs(flowline_points) == st_crs(dem),
              msg = "flowline_points and dem must have the same crs")
  assert_that(is.numeric(buffer_distance),
              msg = "buffer_distance must be numeric")

  # Buffer the flowline to establish the extent
  fl_buffer <- st_buffer(flowline, dist = buffer_distance)

  # Crop the DEM by the flowline buffer
  dem_crop <- crop(dem, fl_buffer, mask = TRUE)

  # Create the trend raster
  fl_pts <- flowline_points %>%
    rename(x = POINT_X,
           y = POINT_Y,
           z = Z) %>%
    select(x, y, z) %>%
    st_drop_geometry()

  trend <- interpIDW(dem, as.matrix(fl_pts),
                     radius = buffer_distance,
                     power = 0,
                     smooth = 0)

  # Smooth the trend raster
  trend_smooth <- terra::focal(trend, w = 55, fun = "mean", na.rm = TRUE)

  # create the detrended raster
  rem <- (dem_crop - trend_smooth) + 100

  return(list(rem   = rem,
              trend = trend_smooth))
}
