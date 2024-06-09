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
#' @importFrom patchwork plot_layout
#'
fig_xs_profiles <- function(cross_section, xs_number, dem,
                            channel = NULL, floodplain = NULL,
                            extent_factor = 1,
                            xs_pts_sf_list) {

  stream <- unique(cross_section$ReachName)[1]

  # Create the cross section map
  dev.new(width = 6, height = 4, noRStudioGD = TRUE)
  xs_map <- fluvgeo::map_xs(cross_section = cross_section,
                            xs_number = xs_number,
                            channel = channel,
                            floodplain = floodplain,
                            dem = dem,
                            extent_factor = extent_factor)
  # Convert the tmap object to a graphics object
  map_grb <- tmap::tmap_grob(xs_map)
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
    patchwork::plot_layout(nrow = 3,
                           guides = "collect",
                           axes = "collect",
                           axis_titles = "collect") &
    ggplot2::theme(legend.position = "right")

  # Create patchwork figure
  xs_fig <- patchwork::wrap_elements(panel = map_grb, clip = TRUE) + p_xs +
    patchwork::plot_layout(nrow = 2,
                           heights = ggplot2::unit(c(4, 4), c('in', 'in')))

  return(xs_fig)
}
