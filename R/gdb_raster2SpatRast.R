#' @title Convert an ESRI Geodatabase raster to a SpatRaster
#'
#' @description Converts an ESRI Geodatabase raster to a terra::SpatRaster.
#'
#' @param raster_path   character; A path to an ESRI raster.
#'
#' @return  A `terra::SpatRaster` object.
#' @export
#'
gdb_raster2SpatRast <- function(raster_path) {
  raster_folder <- dirname(raster_path)
  raster_name   <- basename(raster_path)
  raster_ext    <- tools::file_ext(raster_name)

  # Check if raster parent folder is an ESRI Geodatabase
  if (tools::file_ext(raster_folder) == "gdb") {
    spat_raster <- terra::rast(raster_folder, subds= raster_name)
  }
  # Check if raster_name has a file extension
  if (nchar(raster_ext) > 0) {
    spat_raster <- terra::rast(raster_path)
  }

  return(spat_raster)
}
