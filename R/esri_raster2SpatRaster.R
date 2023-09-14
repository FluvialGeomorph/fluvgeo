#' @title Convert an ESRI raster to a SpatRaster
#'
#' @description Converts an ESRI raster to a terra::SpatRaster.
#'
#' @export
#' @param raster_path       character; A path to an ESRI raster.
#'
#' @details Rasters stored in ESRI Geodatabases can currently only be read using
#' the `arcgisbinding`
#'
#' @return A `terra::SpatRaster` object.
#'
#' @importFrom arcgisbinding arc.open as.raster arc.raster
#' @importFrom terra crs
#' @importFrom sf st_crs
#'
esri_raster2RasterLayer <- function(raster_path) {
  # Open the path the the esri raster
  raster_arc <- arcgisbinding::arc.open(raster_path)

  # Convert the esri raster's CRS to valid WKT2
  raster_wkt2 <- terra::crs(raster_arc@sr$WKT)

  # Create CRS
  raster_crs <- sf::st_crs(raster_wkt2)

  # Export from esri as raster::RasterLayer object
  raster_layer <- arcgisbinding::as.raster(arcgisbinding::arc.raster(raster_arc))

  # Convert to terra::SpatRaster
  spat_raster <- terra::rast(raster_layer)

  # Assign the correct CRS to the raster
  terra::crs(spat_raster) <- raster_crs$wkt

  return(spat_raster)
}
