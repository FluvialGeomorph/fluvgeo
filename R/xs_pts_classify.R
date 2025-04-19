#' @title Classify Cross Section Points
#' @description Classify cross section points that are located within the
#'              floodplain and channel.
#' @param xs_pts           sf object; A cross_section_points object.
#' @param channel_poly     sf object; A channel_poly object.
#' @param floodplain_poly  sf object; A floodplain_poly object.
#' @param buffer_distance  numeric; The buffer distance used to extend the
#'                         channel and floodplain polygons. Linear units of
#'                         sf objects.
#' @returns An sf object representing xs_pts whose points are classified as
#'          falling inside the floodplain or channel.
#' @export
#'
#' @importFrom dplyr select
#'
xs_pts_classify <- function(xs_pts, channel_poly, floodplain_poly,
                            buffer_distance) {
  assert_that("sf" %in% class(xs_pts),
              msg = "xs_pts must be an sf object")
  assert_that("sf" %in% class(channel_poly),
              msg = "channel_poly must be an sf object")
  assert_that("sf" %in% class(floodplain_poly),
              msg = "floodplain_poly must be an sf object")
  assert_that(is.numeric(buffer_distance),
              msg = "buffer_distance must be numeric")

  # Buffer channel and floodplain polygons
  channel_buf <- channel_poly %>%
    st_buffer(dist = buffer_distance)

  floodplain_buf <- floodplain_poly %>%
    st_buffer(dist = buffer_distance)

  # Identify xs_pts in floodplain
  xs_pts_flood <- xs_pts %>%
    st_zm() %>%
    mutate(floodplain = 0) %>%
    mutate(floodplain = if_else(st_intersects(x = ., y = st_zm(floodplain_buf),
                                              sparse = FALSE)[, 1],
                                1, 0))


  # Identify xs_pts in channel
  xs_pts_channel <- xs_pts_flood %>%
    st_zm() %>%
    mutate(channel = 0) %>%
    mutate(channel = if_else(st_intersects(x = ., y = st_zm(channel_buf),
                                           sparse = FALSE)[, 1],
                             1, 0))
  return(xs_pts_channel)
}
