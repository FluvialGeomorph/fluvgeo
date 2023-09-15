#' @title Convert an ESRI raster to a SpatRaster
#'
#' @description Converts an ESRI raster to a terra::SpatRaster.
#'
#' @export
#' @param raster_path       character; A path to an ESRI raster.
#'
#' @details Rasters stored in ESRI Geodatabases have historically only been
#' accessible using the `arcgisbinding` package. No open source drivers existed
#' to read file geodatabase rasters into R. However, since the release of GDAL
#' v3.7, the OpenFileGDB driver now provides support for GDB rasters. GDAL
#' access in R is currently best provided using `sf`. Check
#' `sf::sf_extSoftVersion()` for the version of GDAL bundled with `sf`. When
#' `sf` migrates to using this GDAL version and the OpenFileGDB raster driver
#' is determined to be working, this function will migrate to using it instead
#' of the proprietary `arcgisbinding` approach used up to now.
#'
#' @return A `terra::SpatRaster` object.
#'
#' @importFrom arcgisbinding arc.open as.raster arc.raster
#' @importFrom terra crs rast
#' @importFrom sf st_crs
#'
arc_raster2SpatRaster <- function(raster_path) {
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
