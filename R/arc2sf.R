#' @title Converts ArcGIS feature class to an \code{sf} object
#'
#' @description Opens an ArcGIS feature class and converts it to the \code{sf}
#'     spatial data format.
#'
#' @export
#' @param fc_path      character; Path to the ArcGIS geodatabase feature class.
#'
#' @return The specified ArcGIS feature class as an \code{sf} object.
#'
#' @details The \code{arc2sf} function requires the prior installation of the
#' \code{arcgisbinding} package AND a licensed installation of ESRI
#' \code{ArcGIS Desktop} or \code{ArcGIS Pro}. The \code{arcgisbinding}
#' package can be installed from within \code{ArcGIS Pro} or by following the
#' instructions at
#' \url{https://r-arcgis.github.io/assets/arcgisbinding-vignette.html} for
#' \code{ArcGIS Desktop} users.
#'
#' @seealso The \code{\link{sf2arc}} function for saving data back to an ESRI
#' spatial dataset.
#'
#' @references
#' \describe{
#'   \item{ESRI ArcGIS Desktop, ArcGIS Pro}{\url{https://pro.arcgis.com/}}
#'   \item{\code{arcgisbinding}}{
#'   \url{https://r-arcgis.github.io/assets/arcgisbinding-vignette.html}}
#' }
#'
#' @examples
#' \donttest{
#' library(sf)
#' library(arcgisbinding)
#' arc.check_product()
#'
#' # Path to an ESRI geodatabase feature class
#' fc_path_in <- file.path(system.file("extdata", "testing_data.gdb",
#'                         package = "fluvgeo"),
#'                         "feature_dataset/riffle_channel")
#'
#' # Convert the ArcGIS polyline feature class to an `sp` object
#' fc_sf <- arc2sf(fc_path = fc_path_in)
#' }
#'
#' @importFrom arcgisbinding arc.data2sp arc.select arc.open
#' @importFrom sf st_crs st_as_text
#' @importFrom stringr str_replace
#' @importFrom methods slot<-
#'
arc2sf <- function(fc_path) {
  # Check if fc parent folder exists
  stopifnot(file.exists(dirname(dirname(fc_path))))

  # Create an arc.dataset-class object
  arcobj <- arcgisbinding::arc.open(fc_path)

  # Get WKT from arcobj
  arcobj_wkt <- arcobj@shapeinfo$WKT

  # Create CRS object
  arcobj_crs <- sf::st_crs(x = arcobj_wkt)

  # Make a selection of the ArcGIS data (all data) returned in arc.data format
  arc <- arcgisbinding::arc.select(arcobj)

  # Convert the arc.data format to the sp format
  sf <- arcgisbinding::arc.data2sf(arc)

  return(sf)
}
