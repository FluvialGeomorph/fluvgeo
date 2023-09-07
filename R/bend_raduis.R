#' @title Calculate the bend radius of curvature
#'
#' @description Calculates the radius of curvature for each bend in the input
#' `bankline_points` data frame.
#'
#' @export
#' @param bankline_points  sf data frame; an fluvgeo bankline_points
#'                         data structure
#'
#' @return Returns a data frame of bends with the calculated raduis of
#' curvature.
#'
#' @importFrom testthat expect_true
#' @importFrom conicfit CircleFitByTaubin
#'
bend_radius <- function(bankline_points) {
  # Check parameters
  expect_true(check_bankline_points(bankline_points))

  # Convert sf to a data frame
  bankline_points <- data.frame(bankline_points)



  # Remove bankline_points not assigned to loops
  bankline_points <- bankline_points[!is.na(bankline_points$loop), ]

  # Sort bankline_points by loop, bend, and POINT_M
  bankline_pts <- bankline_points[with(bankline_points, order(loop,
                                                              bend,
                                                              bank_POINT_M)), ]

  # Create a list to hold the bend dimensions
  bend_dims <- list()
  bend_num <- 0

  print("Calculate the bend radius of curvature")

  # Iterate through each loop
  for (l in sort(unique(bankline_pts$loop))) {
    print(paste0("Loop: ", l))

    # Subset bankline_points for the current loop
    bank_pts <- bankline_pts[bankline_pts$loop == l, ]

    # Iterate through each bend
    for (b in sort(unique(bank_pts[bank_pts$position != "apex", ]$bend))) {
      print(paste0("    Bend: ", b))

      # Increment bend counter
      bend_num <- bend_num + 1

      # Subset bankline_pts for the current bend
      bend_pts <- bank_pts[bank_pts$bend == b, ]

      # Convert xy to a matrix for conicfit functions
      bend_xy <- bend_pts[, c("bank_POINT_X", "bank_POINT_Y")]
      bend_xy_m <- as.matrix(bend_xy)

      # Calculate circle center and radius
      center <- conicfit::CircleFitByTaubin(bend_xy_m)

      print(paste("        bend_POINT_X", round(center[1], 2)))
      print(paste("        bend_POINT_Y", round(center[2], 2)))
      print(paste("        bend_radius",  round(center[3], 2)))

      bend_dims[[bend_num]] <- data.frame("bend_num" = bend_num,
                                          "loop" = l,
                                          "bend" = b,
                                          "bend_POINT_X" = center[1],
                                          "bend_POINT_Y" = center[2],
                                          "bend_radius"  = center[3])

    }
  }
  # Append the list of bend dimension data frames into a single data frame
  bend_dimensions_sf <- dplyr::bind_rows(bend_dims)
}
