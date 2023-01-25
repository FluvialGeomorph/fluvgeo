#' @title Converts an \code{sp} object to an ArcGIS feature class
#'
#' @description Converts an \code{sp} object to an ArcGIS feature class in
#'     the specified geodatabase feature class.
#'
#' @export
#' @param sp_object     \code{sp} object
#' @param fc_path       character; Path to the ArcGIS output feature class. The
#'                      feature class must reside in a file geodatabase feature
#'                      dataset.
#'
#' @return Writes the \code{sp} object to an ArcGIS feature class specified
#' by fc_path.
#'
#' @details This function requires the prior installation of the
#' \code{arcgisbinding} package AND a licensed installation of ESRI
#' \code{ArcGIS Pro}. The \code{arcgisbinding} package can be installed
#' from within \code{ArcGIS Pro}.
#'
#' @seealso This function for loading data from an ESRI spatial dataset.
#'
#' @references
#' \describe{
#'   \item{ArcGIS Pro}{\url{https://pro.arcgis.com/}}
#'   \item{\code{arcgisbinding}}{
#'   \url{https://r-arcgis.github.io/assets/arcgisbinding-vignette.html}}
#' }
#'
#' @importFrom assertthat assert_that
#' @importFrom stringr str_detect
#' @importFrom arcgisbinding arc.write
#'
sp2arc <- function(sp_object, fc_path) {
  # Check parameters
  assert_that(stringr::str_detect(class(sp_object)[1], "Spatial*"),
              msg = "sp_object must be of class `Spatial*DataFrame`")

  # Use the `shapeinfo` object with arc.write to preserve crs
  # https://github.com/R-ArcGIS/r-bridge/issues/38

  # Get sp Spatial data class
  sp_shape_type <- class(sp_object)

  # Set ESRI shape type to sp shape type
  esri_shape_type <- switch(sp_shape_type,
                            "SpatialPointsDataFrame"   = "Point",
                            "SpatialLinesDataFrame"    = "Polyline",
                            "SpatialPolygonsDataFrame" = "Polygon")

  # Get the GDB `stream_network` shapeinfo object (used for all GDB objects)
  gdb_feature_dataset_path <- dirname(fc_path)
  stream_network_path <- file.path(gdb_feature_dataset_path,
                                   "stream_network")
  arcobj <- arcgisbinding::arc.open(stream_network_path)
  shape_info <- arcobj@shapeinfo

  # Set the `shapeinfo` type parameter (undocumented)
  shape_info$type <- esri_shape_type
  shape_info <- shape_info[c("type", "hasZ", "hasM", "WKID")]

  # Write the sp object to a geodatabase feature class
  arcgisbinding::arc.write(path = fc_path,
                           data = sp_object,
                           #coords = colnames(sp_object@data),
                           shape_info = shape_info,
                           validate = TRUE,
                           overwrite = TRUE)
}
