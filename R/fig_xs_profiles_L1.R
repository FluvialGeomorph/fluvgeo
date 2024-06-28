#' @title Cross Section Plot Figure
#'
#' @description
#' Constructs a figure of plots describing a single cross section.
#'
#' @export
#' @param cross_section       sf; A cross section lines feature class.
#' @param xs_number           integer; The cross section `Seq` number of the
#'                            requested cross section.
#' @param dem                 terra SpatRaster; A dem raster.
#' @param channel             sf; A channel polygon feature class (optional).
#' @param floodplain          sf; A floodplain polygon feature class (optional).
#' @param extent_factor       numeric; A numeric value used to expand the map
#'                            extent around each cross section.'
#' @param xs_pts_sf_list      list; a list of `sf` objects of cross
#'                            section points, one for each survey time period
#'                            to be graphed. Survey list items must be tagged
#'                            with the survey label to be used in the graph
#'                            legend.
#'
#' @return A patchwork figure.
#'
#' @importFrom tmap tmap_grob
#' @importFrom ggplot2 theme unit
#' @importFrom gtable gtable gtable_add_grob
#' @importFrom patchwork plot_layout wrap_elements
#'
fig_xs_profiles_L1 <- function(cross_section, xs_number, dem,
                               channel = NULL, floodplain = NULL,
                               extent_factor = 1,
                               xs_pts_sf_list) {

  stream <- unique(cross_section$ReachName)[1]

  # Create the cross section map
  dev.new(width = 6, height = 4, noRStudioGD = TRUE)        # control tmap size
  xs_map <- fluvgeo::map_xs(cross_section = cross_section,
                            xs_number = xs_number,
                            channel = channel,
                            floodplain = floodplain,
                            dem = dem,
                            extent_factor = extent_factor)
  # Convert the tmap object to a grob and align using a gtable
  map_grb <- tmap::tmap_grob(xs_map)
  gt <- gtable(widths = unit(6, c("in")),
               heights = unit(4, c("in")),
               name = "map")
  map_gtable <- gtable_add_grob(gt,
                                grobs = map_grb,
                                t = 1, l = 1)
  map_left <- justify_gtable(map_gtable, hjust = "left", vjust = "top")
  #grid::grid.draw(map_left)
  dev.off()

  # Create cross section plots for each extent
  p_all <- fluvgeo::xs_compare_plot_L1(stream = stream,
                                       xs_number = xs_number,
                                       xs_pts_sf_list = xs_pts_sf_list,
                                       extent = "all")
  p_fl  <- fluvgeo::xs_compare_plot_L1(stream = stream,
                                       xs_number = xs_number,
                                       xs_pts_sf_list = xs_pts_sf_list,
                                       extent = "floodplain")
  p_ch <- fluvgeo::xs_compare_plot_L1(stream = stream,
                                      xs_number = xs_number,
                                      xs_pts_sf_list = xs_pts_sf_list,
                                      extent = "channel")

  # Assemble cross section plots
  p_xs <- p_all + p_fl + p_ch +
    plot_layout(nrow = 3,
                guides = "collect",
                axes = "collect",
                axis_titles = "collect") &
    theme(legend.position = "right",
          plot.margin = unit(c(0.01, 0.01, 0.01, 0.01), "cm"))

  # Create patchwork figure
  xs_fig <- wrap_elements(panel = map_left, clip = TRUE) +
    p_xs +
    plot_layout(nrow = 2,
                heights = unit(c(4, 4), c('in', 'in')))

  return(xs_fig)
}
