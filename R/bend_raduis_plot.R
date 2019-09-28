#' @title Plot the bend radius of curvature
#'
#' @description Produces a plot of the raduis of curvature for the specified
#' loop and bend using a `bankline_points` data frame.
#'
#' @export
#' @param bankline_points  SpatialPointsDataFrame; an fgm `bankline_points`
#'                         data structure
#' @param loop             numeric; the loop to plot
#' @param bend             numeric; the bend to plot
#' @param coord_system     character; a brief text description of the coordinate
#'                         system. This gets added to the latitude and longitude
#'                         as an indicator of units.
#'
#' @return a ggplot2 object
#'
#' @importFrom testthat expect_true
#' @importFrom conicfit CircleFitByTaubin
#' @importFrom conicfit calculateCircle
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes geom_point coord_fixed theme_bw labs annotate
#'
bend_raduis_plot <- function(bankline_points, loop, bend, coord_system) {
  # Check parameters
  expect_true(check_bankline_points(bankline_points))

  # Convert Spatial*DataFrame to a data frame
  bankline_points <- bankline_points@data

  # Subset bankline_points the for the input loop and bend
  bend_pts <- bankline_points[which(bankline_points$loop ==
                                      loop & bankline_points$bend == bend), ]

  # Convert xy to a matrix for conicfit functions
  bend_xy <- bend_pts[, c("bank_POINT_X", "bank_POINT_Y")]
  bend_xy_m <- as.matrix(bend_xy)

  # Calculate circle center and radius
  center <- conicfit::CircleFitByTaubin(bend_xy_m)

  # Calculate circle
  circle <- conicfit::calculateCircle(center[1], center[2], center[3])
  circle_df <- as.data.frame(circle)

  # Plot the bend
  ggplot(bend_xy, aes(x = .data$bank_POINT_X, y = .data$bank_POINT_Y)) +
    geom_point() +
    coord_fixed(ratio = 1) +
    geom_point(aes(x = center[1], y = center[2]), colour="blue", size = 3) +
    geom_point(circle_df, mapping = aes(x = .data$V1, y = .data$V2),
               colour="red",
               inherit.aes = FALSE) +
    theme_bw() +
    labs(title = paste0("Loop: ", loop, " Bend: ", bend),
         x = paste0("Longitude (", coord_system, ")"),
         y = paste0("Latitude (", coord_system, ")")) +
    annotate(geom = "text", x = center[1],
             y = center[2] + (center[3] * 0.25),
             label = paste0("Radius = ", round(center[3]), " m"))
}
