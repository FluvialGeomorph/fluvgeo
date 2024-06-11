#' @title Cross Section Plot Figure, L2
#'
#' @description
#' Constructs a Level 2 figure of plots describing a single cross section.
#'
#' @export
#' @param cross_section       sf; A cross section lines feature class.
#' @param xs_number           integer; The cross section `Seq` number of the
#'                            requested cross section.
#' @param dem                 terra SpatRaster; A dem raster.
#' @param channel             sf; A channel polygon feature class (optional).
#' @param floodplain          sf; A floodplain polygon feature class (optional).
#' @param bf_estimate         numeric; Detrended bankfull estimate (units:
#'                            detrended feet).
#' @param regions             character vector; Regions to calculate hydraulic
#'                            dimensions for. See the `RegionalCurve` package for
#'                            a list of regions.
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
#' @importFrom dplyr %>% filter select arrange mutate recode across distinct
#' @importFrom grid textGrob grobHeight gpar
#' @importFrom gtable gtable_add_rows gtable_add_grob
#' @importFrom gridExtra ttheme_default tableGrob
#' @importFrom patchwork wrap_elements plot_layout plot_spacer
#'
fig_xs_profiles_L2 <- function(cross_section, xs_number, dem,
                               channel = NULL, floodplain = NULL,
                               bf_estimate = NULL, regions = NULL,
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
  p_all <- fluvgeo::xs_compare_plot_L2(stream = stream,
                                       xs_number = xs_number,
                                       xs_pts_sf_list = xs_pts_sf_list,
                                       bankfull_elevation = bf_estimate,
                                       aspect_ratio = NULL,
                                       extent = "all")
  p_fl  <- fluvgeo::xs_compare_plot_L2(stream = stream,
                                       xs_number = xs_number,
                                       xs_pts_sf_list = xs_pts_sf_list,
                                       bankfull_elevation = bf_estimate,
                                       aspect_ratio = NULL,
                                       extent = "floodplain")
  p_ch <- fluvgeo::xs_compare_plot_L2(stream = stream,
                                      xs_number = xs_number,
                                      xs_pts_sf_list = xs_pts_sf_list,
                                      bankfull_elevation = bf_estimate,
                                      aspect_ratio = NULL,
                                      extent = "channel")
  # Assemble cross section plots
  p_xs <- p_all + p_fl + p_ch +
    plot_layout(nrow = 3,
                guides = "collect",
                axes = "collect",
                axis_titles = "collect") &
    theme(legend.position = "right")
  #p_xs

  # Create xs dimensions table
  latest_survey <- length(xs_pts_sf_list)
  xs_pts_sf <- xs_pts_sf_list[[latest_survey]]

  t1 <- table_xs_dimensions(xs_pts_sf = xs_pts_sf,
                            xs_number = xs_number,
                            bf_estimate = bf_estimate,
                            regions = regions)

  # Create patchwork figure
  layout <-"AAA
            BBB
            CCC
            DDD"
  xs_fig <- wrap_elements(panel = map_grb, clip = TRUE) +
    p_xs +
    plot_spacer() +
    wrap_elements(full = t1, clip = TRUE) +
    plot_layout(#nrow = 4,
                design = layout,
                #widths  = unit(c(7, 7, 7, 7),
                #               rep('in', 4)),
                heights = unit(c(4, 4, 0.1, 1),
                               rep('in', 4)))

  #xs_fig
  return(xs_fig)
}
