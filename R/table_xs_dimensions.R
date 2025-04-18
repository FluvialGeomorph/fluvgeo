#' @title Create a Cross Section Dimensions Table
#'
#' @description
#' Creates a cross section dimensions table for the channel portion of the
#' specified cross section and regions using the `RegionalCurve` R package.
#'
#' @export
#' @param xs_pts_sf     sf; A cross section lines feature class.
#' @param xs_number     integer; The cross section `Seq` number of the
#'                      requested cross section.
#' @param bf_estimate   numeric; Detrended bankfull estimate (units:
#'                      detrended feet).
#' @param regions       character vector; Regions to calculate hydraulic
#'                      dimensions for. See the `RegionalCurve` package for
#'                      a list of regions.
#'
#' @return a `gtable` object
#'
#' @importFrom dplyr filter .data distinct select mutate across recode arrange
#' @importFrom ggplot2 unit
#' @importFrom grid viewport textGrob
#' @importFrom gtable gtable_add_rows gtable_add_grob
#' @importFrom gridExtra tableGrob ttheme_default
#'
table_xs_dimensions <- function(xs_pts_sf, xs_number, bf_estimate, regions) {

  # Get the channel portion of the current cross section
  xs_pts_channel <- xs_pts_sf %>%
    filter(.data$Seq == xs_number) %>%
    filter(.data$channel == 1)

  # Calculate channel dimensions
  dims <- fluvgeo::xs_dimensions(xs_points = xs_pts_channel,
                                 streams = unique(xs_pts_channel$ReachName),
                                 regions = regions,
                                 bankfull_elevations = bf_estimate)
  # Wrangle the dimensions
  dims_table <- dims %>%
    distinct() %>%
    select(-c("reach_name", "cross_section", "bankfull_elevation",
              "discharge")) %>%
    mutate(across(2:5, \(x) round(x, 1))) %>%
    mutate(xs_type = recode(xs_type,
                            "DEM derived cross section" = "DEM derived")) %>%
    arrange(.data$xs_type) %>%
    arrange(match(xs_type, c("DEM derived")))

  # Create the table
  tt <- ttheme_default(base_size = 9,
                       padding = unit(c(2, 2), "mm"),
                       core = list(
                         fg_params = list(hjust = 0, x = 0.05)),
                       colhead = list(
                         fg_params = list(hjust = 0, x = 0.05, parse = TRUE)))

  col_header <- c("\nRegional Curve",
                  "Drainage Area\n[sq miles]",
                  "Area\n[sq feet]",
                  "Width\n[feet]",
                  "Depth\n[feet]")

  table <- tableGrob(dims_table,
                     rows = NULL,
                     cols = col_header,
                     theme = tt)
  #grid::grid.draw(table)

  # Add a title
  title <- textGrob("Cross Section Dimensions",
                    hjust = 0, x = 0,
                    gp = gpar(fontsize = 10, fontface = "bold"))
  table <- gtable_add_rows(table,
                           heights = grobHeight(title) + unit(2,"mm"),
                           pos = 0)
  table_grob <- gtable_add_grob(table, title,
                                t = 1, l = 1, b = 1,
                                r = ncol(table))
  table_left <- justify_gtable(table_grob, hjust = "left", vjust = "top")
  return(table_left)
}
