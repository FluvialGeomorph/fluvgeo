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
#' @importFrom terra vect buffer relate
#' @importFrom dplyr mutate if_else
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
  floodplain_buf <- floodplain_poly %>%
    vect() %>%
    buffer(width = buffer_distance)
  channel_buf <- channel_poly %>%
    vect() %>%
    buffer(width = buffer_distance)

  # Identify xs_pts in floodplain
  floodplain_pts <- relate(vect(xs_pts), floodplain_buf, "intersects")
  xs_pts_flood <- xs_pts %>%
    mutate(floodplain = if_else(as.vector(floodplain_pts), 1, 0))

  # Identify xs_pts in channel
  channel_pts <- relate(vect(xs_pts), channel_buf, "intersects")
  xs_pts_channel <- xs_pts_flood %>%
    mutate(channel = if_else(as.vector(channel_pts), 1, 0))

  return(xs_pts_channel)
}
