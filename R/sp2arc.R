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
#' @seealso The \code{\link{arc2sp}} function for loading data from and ESRI
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
sp2arc <- function(sp_object, fc_path) {
  # Write the sp object to a geodatabase feature class
  arcgisbinding::arc.write(data = sp_object, path = fc_path)
}
