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
#' @param bf_estimate        numeric; Detrended bankfull estimate (units:
#'                           detrended feet).
#' @param regions            character vector; Regions to calculate hydraulic
#'                           dimensions for. See the `RegionalCurve` package for
#'                           a list of regions.
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
  p_xs

  # Calculate cross section geometry
  latest_survey <- length(xs_pts_sf_list)
  xs_pts_channel <- xs_pts_sf_list[[latest_survey]] %>%
    filter(Seq == xs_number) %>%
    filter(channel == 1)

  dims <- fluvgeo::xs_dimensions(xs_points = xs_pts_channel,
                                 streams = unique(xs_pts_channel$ReachName),
                                 regions = regions,
                                 bankfull_elevations = bf_estimate)
  # Create table
  dims_table <- dims %>%
    distinct() %>%
    select(-c("reach_name", "cross_section", "bankfull_elevation",
              "discharge")) %>%
    mutate(across(2:5, \(x) round(x, 1))) %>%
    mutate(xs_type = recode(xs_type,
                            "DEM derived cross section" = "DEM derived")) %>%
    arrange(xs_type) %>%
    arrange(match(xs_type, c("DEM derived")))

  tt <- ttheme_default(base_size = 10,
                       padding = unit(c(3, 3), "mm"),
                       core = list(
                         fg_params = list(hjust = 0, x = 0.05)),
                       colhead = list(
                         fg_params = list(hjust = 0, x = 0.05, parse = TRUE)))

  table <- tableGrob(dims_table, rows = NULL,
                     cols = c("\nRegional Curve",
                              "Drainage Area\n[sq miles]",
                              "Area\n[sq feet]",
                              "Width\n[feet]",
                              "Depth\n[feet]"),
                     theme = tt)
  title <- textGrob("Cross Section Dimensions",
                    hjust = 0, x = 0,
                    gp = gpar(fontsize = 12, fontface = "bold"))
  table <- gtable_add_rows(table,
                           heights = grobHeight(title) + unit(2,"mm"),
                           pos = 0)
  table_grob <- gtable_add_grob(table, title,
                                t = 1, l = 1, b = 1,
                                r = ncol(table))
  #grid::grid.draw(table_grob)

  # Create patchwork figure
  xs_fig <- wrap_elements(panel = map_grb, clip = TRUE) +
    p_xs +
    plot_spacer() +
    wrap_elements(plot = table_grob, clip = FALSE) +
    plot_layout(nrow = 4,
                heights = unit(c(4, 4, 0.1, 1),
                               rep('in', 4)))

  #xs_fig
  return(xs_fig)
}
