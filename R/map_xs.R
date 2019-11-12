#' @title Cross section location map
#'
#' @description Produces a cross section location map for the specified cross
#' section.
#'
#' @export
#' @param cross_section       SpatialLinesDataFrame; a cross section lines
#'                            feature class
#' @param xs_number           integer; The cross section identifier of the
#'                            requested cross section.
#' @param dem                 character; path to a dem raster
#' @param banklines           SpatialLinesDataFrame; a banklines feature class
#' @param extent_factor       numeric; A numeric value used to expand the map
#'                            extent around each cross section.
#'
#' @return a tmap object
#'
#' @importFrom arcgisbinding arc.open arc.raster
#' @importFrom raster extent crop terrain hillShade
#' @importFrom grDevices colorRampPalette
#' @importFrom tmap tm_shape tm_raster tm_lines tm_text tm_add_legend
#' tm_compass tm_scale_bar tm_layout
#'
map_xs <- function(cross_section, xs_number, dem, banklines,
                   extent_factor = 7) {

  # Check data structure
  check_cross_section(cross_section, step = "assign_ids")
  check_banklines(banklines)

  # Subset cross_section for the requested xs_number
  xs_i <- cross_section[cross_section$Seq == xs_number, ]

  # Calculate the map extent for the current cross section
  map_extent <- fluvgeo::feature_extent(feature = xs_i,
                                    extent_factor = extent_factor)

  # Clip the dem to the cross section map extent
  dem_arc <- arcgisbinding::arc.open(dem)
  dem_i <- raster::crop(as.raster(arc.raster(dem_arc)), map_extent)

  # Create a hillshade from dem_i
  slp <- raster::terrain(dem_i, opt = "slope", unit = "radians")
  asp <- raster::terrain(dem_i, opt = "aspect", unit = "radians")
  hill <- raster::hillShade(slope = slp, aspect = asp)

  # Create a topo color ramp
  esri_topo <- grDevices::colorRampPalette(colors = c("cadetblue2", "khaki1",
                                                      "chartreuse4", "goldenrod1",
                                                      "orangered4", "saddlebrown",
                                                      "gray70", "white"),
                                           bias = 1,
                                           space = "Lab",
                                           interpolate = "linear")
  # Create the cross section map
  xs_map <- tm_shape(shp = hill,
                     name = "Hillshade") +
              tm_raster(style = "cont",
                        palette = grey(0:100/100),
                        legend.show = FALSE) +
            tm_shape(shp = dem_i,
                     name = "Elevation",
                     unit = "ft",
                     is.master = TRUE) +
              tm_raster(col = "Band_1",
                        style = "cont",
                        palette = esri_topo(1000),
                        alpha = 0.8,
                        title = "Elevation (NAVD88, ft)",
                        legend.show = TRUE) +
            tm_shape(shp = cross_section,
                     name = "Cross Section") +
              tm_lines(col = "black", lwd = 5) +
              tm_text(text = "Seq",
                      col = "black",
                      remove.overlap = FALSE,
                      along.lines = TRUE,
                      overwrite.lines = TRUE) +
            tm_shape(shp = banklines,
                     name = "Banklines") +
              tm_lines(col = "blue", lwd = 1,
                       legend.lwd.show = ) +
              tm_add_legend(type = "line",
                            labels = "Bankfull",
                            col = "blue",
                            lwd = 1) +
            tm_compass(type = "arrow",
                       position = c("right", "bottom")) +
            tm_scale_bar(width = 0.25,
                         position = c("left", "bottom")) +
            tm_layout(main.title = paste("Cross Section", xs_number),
                      legend.outside = TRUE,
                      legend.outside.position = "right",
                      frame.lwd = 3)
  return(xs_map)
}
