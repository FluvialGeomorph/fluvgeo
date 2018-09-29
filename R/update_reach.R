#' @title Update dimensions and stats for the input reach
#'
#' @description Update cross section dimensions and GOF stats for the input
#'     reach.
#'
#' @export
#' @param xs_points_path  character; The path to the file geodatabase feature
#'                        class "cross_section_points" for the specified site.
#' @param xs_points       sp object; The sp object containing the xs_points for
#'                        the study area.
#' @param xs_dims         data frame; The data frame that holds cross section
#'                        dimensions.
#' @param gof_stats       data frame; The data frame that holds the gof stats.
#' @param regions         character vector; A vector of region names for which
#'                        GOF stats will be calculated.
#' @param bankfull_elevations numeric vector; A numeric vector of detrended
#'                            bankfull elevations for which GOF stats will be
#'                            calculated.
#'
#' @return A list containing three data frames updated for the specified reach:
#'     1. xs_dims, 2. xs_points, 3. gof_stats
#'
update_reach <- function(xs_points_path, xs_points, xs_dims, gof_stats,
                         regions, bankfull_elevations) {
  # Import the "cross_section_points" feature class as an sp object
  xs_points_reach <- arc2sp(xs_points_path)
  # Determine ReachName
  reach_name <- unique(xs_points_reach$ReachName)
  # Update xs_points data frame
  xs_points <- update_xs_points(reach_name = reach_name,
                                xs_points_reach = xs_points_reach,
                                xs_points = xs_points)
  # Update xs_dims data frame
  xs_dims <- update_xs_dims(reach_name = reach_name,
                            xs_points_reach = xs_points_reach,
                            xs_dims = xs_dims)
  # Update gof_stats data frame
  gof_stats <- update_gof_stats(reach_name = reach_name,
                                xs_dims = xs_dims,
                                regions = regions,
                                bankfull_elevations = bankfull_elevations)
  # Return list containing: xs_points, xs_dims, gof_stats
  return(list(xs_points = xs_points,
              xs_dims = xs_dims,
              gof_stats = gof_stats))
}
