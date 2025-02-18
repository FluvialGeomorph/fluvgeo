#' Cross Section Bearing
#'
#' @description Calculate the flow direction bearing of the cross section.
#'
#' @export
#' @param cross_section       sf; A cross section lines feature class.
#'
#' @return cross_section sf object with a new field bearing
#'
#' @importFrom dplyr mutate case_when
#'
xs_bearing <- function(cross_section) {
  # Check data structure
  check_cross_section(cross_section, step = "assign_ids")

  xs_crs <- st_crs(cross_section)
  xs_update <- xs_upstream(cross_section)

  # Calculate bearings
  xs_dims <- xs_update %>%
    # https://math.stackexchange.com/questions/1596513/find-the-bearing-angle-between-two-points-in-a-2d-space
    mutate(x_diff = x_end - x_start) %>%
    mutate(y_diff = y_end - y_start) %>%
    mutate(xs_bearing_rad = atan2(y_diff, x_diff)) %>%
    mutate(xs_bearing_rad_pos = case_when(
                       xs_bearing_rad <  0 ~ xs_bearing_rad + (2 * pi),
                       xs_bearing_rad >= 0 ~ xs_bearing_rad)) %>%
    mutate(xs_bearing_deg = round(xs_bearing_rad_pos * (180/pi))) %>%
    mutate(flow_dir = xs_bearing_deg - 90)

  return(xs_dims)
}
