#' Calculate upstream cross section
#'
#' @description Calculates the upstream cross section coordinates, updates
#' cross section end points, reverses incorrectly digitized cross sections.
#'
#' @param cross_section   sf line object, The cross section feature.
#'
#' @return sf line object
#' @export
#'
#' @importFrom dplyr %>% arrange select one_of left_join mutate lead last
#'                   filter
#' @importFrom sf st_drop_geometry
#'
xs_upstream <- function(cross_section) {
  # Calculate xs start/end points
  xs_start <- sf_line_end_point(cross_section, end = "start")
  xs_end   <- sf_line_end_point(cross_section, end = "end")

  xs_update <- cross_section %>%
    arrange(Seq) %>%
    select(-one_of("x_start", "y_start", "x_end", "y_end",
                   "from_measure", "to_measure", "ReachName")) %>%
    left_join(y = st_drop_geometry(xs_start), by = "Seq") %>%
    select(-one_of("ReachName")) %>%
    left_join(y = st_drop_geometry(xs_end), by = "Seq") %>%

    # get next upstream flowline point
    mutate(upstream_x = lead(x = POINT_X, n = 1,
                             default = last(.$POINT_X))) %>%
    mutate(upstream_y = lead(x = POINT_Y, n = 1,
                             default = last(.$POINT_Y))) %>%

    # determine if the xs start point is on the left descending bank
    mutate(start_left = start_left(upstream_x, upstream_y,
                                   POINT_X, POINT_Y,
                                   x_start, y_start))
  # Assure XS's digitized beginning on the left descending bank
  # XS's that begin on the left descending bank
  xs_left <- xs_update %>%
    filter(start_left == TRUE) %>%
    mutate(fixed_start = start_left(upstream_x, upstream_y,
                                    POINT_X, POINT_Y,
                                    x_start, y_start))
  # XS's that do not begin on the left descending bank; need to be flipped
  xs_right <- xs_update %>%
    filter(start_left == FALSE) %>%
    sf_line_reverse()

  # update XS start and end points of the backward XS's
  xs_start <- sf_line_end_point(xs_right, end = "start")
  xs_end   <- sf_line_end_point(xs_right, end = "end")

  # check that the reverse works
  xs_right_rev <- xs_right %>%
    select(-one_of("x_start", "y_start", "x_end", "y_end", "ReachName")) %>%
    left_join(y = st_drop_geometry(xs_start), by = "Seq") %>%
    select(-one_of("ReachName")) %>%
    left_join(y = st_drop_geometry(xs_end), by = "Seq") %>%
    mutate(fixed_start = start_left(upstream_x, upstream_y,
                                    POINT_X, POINT_Y,
                                    x_start, y_start))
  # combine
  xs_fixed <- bind_rows(xs_left, xs_right_rev) %>%
    arrange(Seq)

  return(xs_fixed)
}
