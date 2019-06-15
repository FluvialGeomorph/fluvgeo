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
#' \dontest{
#' library(arcgisbinding)
#' arc.check_product()
#'
#' # Path to an ESRI geodatabase feature class to import
#' fc_path_in <- file.path(getwd(), "data-raw/test.gdb/riffle")
#' fc_sp <- arc2sp(fc_path = fc_path_in)
#'
#' ## Do some R operations here ##
#'
#' # Path to the ESRI geodatabase feature class to be saved
#' fc_path_out <- file.path(getwd(), "data-raw/test.gdb/sp2arc_test")
#' sp2arc(sp_object = fc_sp, fc_path = fc_path_out)
#' }
#'
sp2arc <- function(sp_object, fc_path) {
  # Convert the sp object to an ArcGIS data object
  arcobj <- arcgisbinding::arc.sp2data(sp_object)
  # Write the ArcGIS object to a geodatabase feature class
  arcgisbinding::arc.write(data = arcobj, path = fc_path)
}
