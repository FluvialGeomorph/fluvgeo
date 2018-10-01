#' @title Update the \code{gof_stats} data frame
#'
#' @description Updates the gof_stats data frame with goodness-of-fit scores
#'     for cross section dimensions for the specified reach.
#'
#' @export
#' @param reach_name      character; The reach name of the new xs_points data.
#' @param xs_dims         data frame; The data frame that holds cross section
#'                        dimensions.
#' @param gof_stats       data frame; The data frame that holds the goodnesss-
#'                        of-fit statistics.
#' @param regions         character vector; A vector of region names for which
#'                        GOF stats will be calculated.
#' @param bankfull_elevations numeric vector; A numeric vector of detrended
#'                            bankfull elevations (units detrended feet) for
#'                            which GOF stats will be calculated.
#'
#' @return An updated \code{gof_stats} data frame for the specifed reach.
#'
update_gof_stats <- function(reach_name, xs_dims, gof_stats, regions,
                             bankfull_elevations) {
  # Remove existing records from gof_stats for the specified reach
  gof_stats_reach <- gof_stats[gof_stats$ReachName != reach_name, ]
  # Calculate gof_stats for the specified reach
  updated_gof_stats_reach <- build_gof_stats(xs_dims = xs_dims,
                                    reach_name = reach_name,
                                    regions = regions,
                                    bankfull_elevations, bankfull_elevations)
  # Append new gof stats to gof_stats
  gof_stats <- rbind(gof_stats_reach, updated_gof_stats_reach)
  return(gof_stats)
}
