#' @title Convert an ESRI raster to a RasterLayer
#'
#' @description Converts an ESRI raster to a raster::RasterLayer.
#'
#' @export
#' @param raster_path       character; A path to an ESRI raster.
#'
#' @details Rasters stored in ESRI Geodatabases can currently only be read using
#' the `arcgisbinding`
#'
#' @return A `raster::RasterLayer` object.
#'
#' @importFrom arcgisbinding arc.open as.raster arc.raster
#' @importFrom raster crs
#' @importFrom rgdal showSRID
#' @importFrom sp CRS
#'
esri_raster2RasterLayer <- function(raster_path) {
  # Open the path the the esri raster
  raster_arc <- arcgisbinding::arc.open(raster_path)

  # Convert the esri raster's CRS to valis WKT2
  raster_wkt2 <- rgdal::showSRID(raster_arc@sr$WKT)

  # Check spatial reference system WKT2 string
  testthat::expect_true(rgdal::checkCRSArgs_ng(raster_wkt2)[[1]])

  # Create CRS
  raster_CRS <- sp::CRS(SRS_string = raster_wkt2)

  # Convert to RasterLayer
  raster_layer <- arcgisbinding::as.raster(arcgisbinding::arc.raster(raster_arc))

  # Assign the correct CRS to the raster
  raster::crs(raster_layer) <- raster_CRS

  return(raster_layer)
}
