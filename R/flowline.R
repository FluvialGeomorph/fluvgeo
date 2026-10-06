#' @title Flowline
#' 
#' @description Validates and prepares one drawn or terrain-derived Flowline.
#' By default, DEM endpoint elevations orient it downstream-to-upstream. An
#' explicit topology-preserving mode retains direction already established by
#' a routed Stream Network.
#' @param flowline   sf object; One flowline with a defined CRS.
#' @param reach_name character; The name of the stream reach.
#' @param dem        terra SpatRaster object; A DEM in the Flowline CRS. In
#'   \code{"dem"} mode it supplies endpoint-elevation direction evidence; in
#'   \code{"preserve"} mode it remains part of the shared preparation contract.
#' @param direction Character; \code{"dem"} retains the historical endpoint-elevation
#'   orientation. \code{"preserve"} keeps an upstream-directed line whose direction
#'   has already been established by saved network topology.
#'
#' @returns One \code{sf} Flowline with only \code{ReachName} and geometry fields. Its
#'   coordinates begin downstream and end upstream when the selected direction
#'   method supplies or preserves that evidence.
#' @details Uses \code{orient_lines_from_dem()} for endpoint-based upstream orientation
#'   by default. \code{direction = "preserve"} is intended for linework whose
#'   downstream-to-upstream direction is already established by topology; it
#'   retains that direction while applying the same CRS, cardinality and output
#'   field contract.
#'   Browser-drawn WGS84/Web Mercator input retains the historical GeoJSON CRS
#'   repair. Other explicitly defined projected CRSs pass through unchanged. If
#'   direction cannot be resolved, returns unchanged linework with a warning.
#' @export
#' 
#' @importFrom assertthat assert_that
#' @importFrom sf st_crs st_within
#' @importFrom dplyr mutate select arrange
#' @importFrom fluvgeo sf_line_end_point sf_line_reverse

#' 
flowline <- function(flowline, reach_name, dem, direction = c("dem", "preserve")) {
  direction <- match.arg(direction)
  assert_that(inherits(flowline, "sf") && !is.na(st_crs(flowline)),
              msg = "flowline must be an sf object with a defined crs")
  if (isTRUE(st_crs(flowline)$epsg %in% c(3857, 4326)))
    flowline <- sf_fix_crs(flowline)
  assert_that(st_crs(flowline) == st_crs(dem), 
              msg = "flowline and dem must have the same crs")
  assert_that(nchar(reach_name) > 0,
              msg = "reach_name must be a non-empty string")
  assert_that(nrow(flowline) == 1,
              msg = "flowline must have only one feature")
  # assert_that(st_within(flowline, 
  #                       st_sf(st_as_sfc(st_bbox(dem))), sparse = FALSE),
  #             msg = "flowline must be within the dem")
  
  fl <- flowline %>%
    select() %>%
    mutate(ReachName = reach_name)
  
  if (identical(direction, "preserve")) return(fl)

  oriented <- orient_lines_from_dem(fl, dem)
  if (any(oriented$direction$action == "UNRESOLVED")) {
    warning("Flowline direction is unresolved: ",
            paste(unique(oriented$direction$reason_code), collapse = ", "),
            call. = FALSE)
  }
  return(oriented$lines)
}
