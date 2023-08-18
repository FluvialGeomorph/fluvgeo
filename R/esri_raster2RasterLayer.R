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
#' @importFrom sf st_crs
#'
esri_raster2RasterLayer <- function(raster_path) {
  # Open the path the the esri raster
  raster_arc <- arcgisbinding::arc.open(raster_path)

  # Convert the esri raster's CRS to valis WKT2
  raster_wkt2 <-raster::crs(raster_arc@sr$WKT)

  # Create CRS
  raster_CRS <- sf::st_crs(raster_wkt2)

  # Convert to RasterLayer
  raster_layer <- arcgisbinding::as.raster(arcgisbinding::arc.raster(raster_arc))

  # Assign the correct CRS to the raster
  raster::crs(raster_layer) <- raster_CRS$wkt

  return(raster_layer)
}
