#' @title Justify a tableGrob
#'
#' @description Correctly justifies a `gtable` object returned from the
#' `gridExtra::tableGrob` function.
#'
#' @details
#' This function was provided in https://stackoverflow.com/a/32111143/7454101
#' by the `gridExtra` author baptiste.
#'
#' @export
#' @param x       gtable; A gtable returned from `gridExtra::tableGrob`.
#' @param hjust   character; Horizontal justification. One of "left", "right",
#'                or "center" (default).
#' @param vjust   character; Vertical justification. One of "left", "right",
#'                or "center" (default).
#' @param draw    boolean; Should the table be drawn?
#'
#' @return A `gtable` object
#'
#' @importFrom ggplot2 unit
#' @importFrom grid viewport grid.draw
#'
justify_gtable <- function(x, hjust = "center", vjust = "center",
                              draw = FALSE){
  w <- sum(x$widths)
  h <- sum(x$heights)
  xj <- switch(hjust,
               center = 0.5,
               left = 0.5*w,
               right = unit(1,"npc") - 0.5*w)
  yj <- switch(vjust,
               center = 0.5,
               bottom = 0.5*h,
               top = unit(1,"npc") - 0.5*h)
  x$vp <- viewport(x = xj, y = yj)

  if(draw) grid.draw(x)
  return(x)
}
