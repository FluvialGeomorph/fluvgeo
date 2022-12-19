#' @title Get ArcGIS feature class CRS WKT
#'
#' @description Get ArcGIS feature class coordinate reference system (CRS) WKT
#'   string.
#'
#' @export
#' @param fc_path      character; Path to the ArcGIS geodatabase feature class.
#' @param WKID         logical; TRUE returns the CRS WKT string (default),
#'                     FALSE returns the CRS WKID integer.
#'
#' @return CRS WKT string.
#'
#' @importFrom arcgisbinding arc.open
#'
get_arc_wkt <- function(fc_path, WKID = FALSE) {
  # Create an arc.dataset-class object
  arcobj <- arcgisbinding::arc.open(fc_path)

  if(WKID) {
    wkt <- arcobj@shapeinfo$WKID
  } else {
    wkt <- arcobj@shapeinfo$WKT
  }

  return(wkt)
}
