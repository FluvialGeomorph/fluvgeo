#' Reverse line direction
#'
#' @description Reverses the directions of an sf object's lines.
#'
#' @param line_sf       sf object of type MULTILINESTRING, LINESTRING
#'
#' @return sf object
#' @export
#'
#' @importFrom sf st_cast st_geometry st_reverse st_set_geometry
#'
sf_line_reverse <- function(line_sf) {
  # convert to LINESTRING
  ls <- st_cast(line_sf, to = "LINESTRING", warn = FALSE)

  # extract geometry
  ls_geom <- st_geometry(ls)

  # reverse the LINESTRING geometry
  rev_ls_geom <- st_reverse(ls_geom)

  # assign the reversed geometry back to the original sf object
  rev_line_sf <- st_set_geometry(line_sf, rev_ls_geom)

  return(rev_line_sf)
}
