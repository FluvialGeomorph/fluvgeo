#' Does point C fall on the left side of the AB line?
#'
#' @description Returns TRUE if the point C fall on the left side of the AB
#' line. Orientation of the AB line determines the left and right side (i.e,
#' starting at point A looking toward point B). Used to answer the question:
#' Is point C on the left of the AB line?
#'
#' @param a_x        numeric, Point A x coordinate
#' @param a_y        numeric, Point A y coordinate
#' @param b_x        numeric, Point B x coordinate
#' @param b_y        numeric, Point B y coordinate
#' @param c_x        numeric, Point C x coordinate
#' @param c_y        numeric, Point C y coordinate
#'
#' @details Uses the cross product method. Taken from:
#' [SO](https://stackoverflow.com/questions/1560492/how-to-tell-whether-a-point-is-to-the-right-or-left-side-of-a-line)
#'
#' @return logical
#' @export
#'
start_left <- function(a_x, a_y, b_x, b_y, c_x, c_y) {
  cp <- (b_x - a_x)*(c_y - a_y)-(b_y - a_y)*(c_x - a_x)
  return(cp > 0)
}
