#' @title Hydroflatten DEM
#' @description Hydroflattens a DEM.
#' @param dem                   terra SpatRaster object; A DEM. Elevation
#'                              units = feet.
#' @param trend                 terra SpatRaster object; A trend raster
#'                              produced using the detrend function.
#' @param relative_water_depth  numeric; The relative water depth expressed as
#'                              height above the trend surface. Depth
#'                              units = feet.
#' @returns A terra SpatRaster object representing a DEM hydroflattened to
#' the specified water depth.
#' @export
#'
#' @importFrom terra ifel
#'
hydroflatten_dem <- function(dem, trend, relative_water_depth) {
  assert_that("SpatRaster" %in% class(dem),
              msg = "dem must be a SpatRaster object")
  assert_that("SpatRaster" %in% class(trend),
              msg = "trend must be a SpatRaster object")
  assert_that(is.numeric(relative_water_depth),
              msg = "relative_water_depth must be numeric")

  # Raise water level above the trend surface
  watersurface <- trend + relative_water_depth

  # Combine watersurface with dem
  out_surface <- ifel(watersurface > dem, watersurface, dem)

  return(out_surface)
}
