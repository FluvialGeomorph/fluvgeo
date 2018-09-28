#' @title Converts ArcGIS feature class to an \code{sp} object
#'
#' @description Opens an ArcGIS feature class and converts it to the \code{sp}
#'     spatial data format.
#'
#' @export
#' @param fc_path      string; Path to the ArcGIS feature class.
#'
#' @return The specified ArcGIS feature class as an \code{sp} object.
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
