#' @title Update the \code{xs_dims} data frame
#'
#' @description Updates the xs_dims data frame with cross section dimensions
#'     for the specified reach.
#'
#' @export
#' @param reach_name       character; The reach name of the new xs_points data.
#' @param xs_points_reach  sp object; The sp object containing the xs_points.
#' @param xs_dims          data frame; The data frame that holds cross section
#'                         dimensions.
#' @param regions         character vector; A vector of region names for which
#'                        GOF stats will be calculated.
#' @param bankfull_elevations numeric vector; A numeric vector of detrended
#'                            bankfull elevations for which GOF stats will be
#'                            calculated.
#'
#' @return An updated data frame of xs_dims for the specified reach.
#'
update_xs_dims <- function(reach_name, xs_points_reach, xs_dims, regions,
                           bankfull_elevations) {
  # Remove existing records from xs_dims for the specified reach
  xs_dims <- xs_dims[xs_dims$ReachName != reach_name, ]
  # Calculate xs dimensions for the specified reach
  xs_dims_reach <- xs_Dimensions(xs_points = xs_points_reach,
                                 streams = reach_name,
                                 regions = regions,
                                 bankfull_elevations = bankfull_elevations)
  # Append the new xs dimensions the to xs_dims data frame
  xs_dims <- rbind(xs_dims, xs_dims_reach)
  return(xs_dims)
}
