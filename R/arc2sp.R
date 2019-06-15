#' @title Converts ArcGIS feature class to an \code{sp} object
#'
#' @description Opens an ArcGIS feature class and converts it to the \code{sp}
#'     spatial data format.
#'
#' @export
#' @param fc_path      character; Path to the ArcGIS geodatabase feature class.
#'
#' @return The specified ArcGIS feature class as an \code{sp} object.
#'
#' @details The \code{arc2sp} function requires the prior installation of the
#' \code{arcgisbinding} package AND a licensed installation of ESRI
#' \code{ArcGIS Desktop} or \code{ArcGIS Pro}. The \code{arcgisbinding}
#' package can be installed from within \code{ArcGIS Pro} or by following the
#' instructions at
#' \url{https://r-arcgis.github.io/assets/arcgisbinding-vignette.html} for
#' \code{ArcGIS Desktop} users.
#'
#' @seealso The \code{\link{sp2arc}} function for saving data back to an ESRI
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
#' \dontest{
#' library(arcgisbinding)
#' arc.check_product()
#'
#' # Path to an ESRI geodatabase feature class
#' fc_path_in <- file.path(getwd(), "data-raw/test.gdb/riffle")
#' fc_sp <- arc2sp(fc_path = fc_path_in)
#' }
#'
arc2sp <- function(fc_path) {
  # Open a connection to the specified ArcGIS feature class
  arcobj <- arcgisbinding::arc.open(fc_path)
  # Select the ArcGIS data
  arc <- arcgisbinding::arc.select(arcobj)
  # Convert the ArcGIS format to the sp format
  sp <- arcgisbinding::arc.data2sp(arc)
  return(sp)
}
