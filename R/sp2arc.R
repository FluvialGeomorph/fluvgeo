#' @title Converts an \code{sp} object to an ArcGIS feature class
#'
#' @description Converts an \code{sp} object to an ArcGIS feature class in
#'     the specified geodatabase feature class.
#'
#' @export
#' @param sp_object     \code{sp} object
#' @param fc_path       character; Path to the ArcGIS feature class.
#'
#' @return Writes the \code{sp} object to an ArcGIS feature class specified
#'     by path.
#'
#' @details The \code{sp2arc} function requires the prior installation of the
#' \code{arcgisbinding} package AND a licensed installation of ESRI
#' \code{ArcGIS Desktop} or \code{ArcGIS Pro}. The \code{arcgisbinding}
#' package can be installed from within \code{ArcGIS Pro} or by following the
#' instructions at
#' \url{https://r-arcgis.github.io/assets/arcgisbinding-vignette.html} for
#' \code{ArcGIS Desktop} users.
#'
#' @seealso The \code{\link{arc2sp}} function for loading data from an ESRI
#' spatial dataset.
#'
#' @references
#' \describe{
#'   \item{ESRI ArcGIS Desktop, ArcGIS Pro}{\url{https://pro.arcgis.com/}}
#'   \item{\code{arcgisbinding}}{
#'   \url{https://r-arcgis.github.io/assets/arcgisbinding-vignette.html}}
#' }
#'
#'@examples
#' \donttest{
#' library(sp)
#' library(arcgisbinding)
#' arc.check_product()
#'
#' # An `sp` object
#' fc_sp <- fluvgeo::sin_flowline_sp
#'
#' # Create a path to a shapefile in a temporary directory
#' temp_file <- tempfile("flowline", fileext = ".shp")
#'
#' # Convert the `sp` object to a shapefile
#' sp2arc(sp_object = fc_sp, fc_path = temp_file)
#' }
#'
#' @importFrom assertthat assert_that
#' @importFrom stringr str_detect
#' @importFrom arcgisbinding arc.write
#'
sp2arc <- function(sp_object, fc_path) {
  # Check parameters
  assert_that(stringr::str_detect(class(sp_object), "Spatial*"),
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

  # Get the GDB `flowline` shapeinfo object (used for all GDB objects)
  gdb_path <- dirname(fc_path)
  flowline_path <- file.path(gdb_path, "flowline")
  arcobj <- arcgisbinding::arc.open(flowline_path)
  shape_info <- arcobj@shapeinfo

  # Get the sp CRS object
  #sp_crs <- sp_object@proj4string

  # Set the `shapeinfo` type parameter (undocumented)
  shape_info$type <- esri_shape_type

  # Write the sp object to a geodatabase feature class
  arcgisbinding::arc.write(data = sp_object,
                           path = fc_path,
                           shape_info = shape_info,
                           overwrite = TRUE)
}
