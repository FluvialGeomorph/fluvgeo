#' @title Update the \code{xs_points} data frame
#'
#' @description Updates the \code{xs_points} SpatialPointsDataFrame object
#'     with terrain data for the specified reach.
#'
#' @export
#' @param reach_name       character; The reach name of the new xs_points
#'                         data.
#' @param xs_points_reach  \code{sp} object; The \code{sp} object containing
#'                         the \code{xs_points} for the specified reach.
#' @param xs_points        \code{sp} object; The \code{sp} object containing
#'                         the \code{xs_points} for the study area.
#'
#' @return An updated SpatialPointsDataFrame object containing the terrain data
#'     for the specified reach.
#'
update_xs_points <- function(reach_name, xs_points_reach, xs_points) {
  # Remove existing recods from xs_points for the specified reach
  xs_points <- xs_points[xs_points@data$ReachName != reach_name, ]
  # Append new xs points to xs_points
  xs_points <- rbind(xs_points, xs_points_reach)
  return(xs_points)
}
