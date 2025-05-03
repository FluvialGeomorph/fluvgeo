#' @title Point Watershed Area
#' @description Calculates the upstream watershed area for the specified point.
#' @param point_sf   an sf object; The point to have a watershed determined.
#' @returns a list of sf objects.
#'  * pt - The point snapped to the NHD nearest downstream flowline
#'  * drainage_basing - The upstream drainage area of the snapped point.
#' @export
#' @importFrom sf st_sfc st_point st_as_sfc st_as_sf st_area
#' @importFrom nhdplusTools get_raindrop_trace get_split_catchment
#' @importFrom dplyr mutate filter select
#'
pt_watershed_area <- function(point_sf) {
  assert_that("sf" %in% class(point_sf),
              msg = "point_sf must be an sf object")

  # Find the nearest downslope NHD flowline
  trace <- get_raindrop_trace(st_as_sfc(point_sf))

  # Extract the point located along the NHD network
  snap_point <- st_sfc(st_point(trace$intersection_point[[1]]),
                       crs = 4326) |>
    st_as_sf() |>
    rename(geometry = x) |>
    mutate(gnis_name = trace$gnis_name[1]) |>
    mutate(comid = trace$comid[1]) |>
    mutate(reachcode = trace$reachcode[1]) |>
    mutate(raindrop_pathDist = trace$raindrop_pathDist[1])

  # Get the catchment and the upstream drainage basin
  catchment <- get_split_catchment(st_as_sfc(snap_point),
                                   upstream = TRUE)
  # Create the drainage area polygon
  drainage_basin <- catchment |>
    filter(id == "drainageBasin") |>
    mutate(gnis_name = trace$gnis_name[1]) |>
    mutate(comid = trace$comid[1]) |>
    mutate(reachcode = trace$reachcode[1]) |>
    select(-catchmentID)

  drainage_basin$area <- st_area(drainage_basin)

  # Project to 3857 on return
  return(list(
    pt             = st_transform(snap_point, crs = 3857),
    drainage_basin = st_transform(drainage_basin, crs = 3857)
  ))
}

