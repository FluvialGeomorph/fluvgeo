#' @title Cross section location map
#'
#' @description Produces a cross section location map for the specified cross
#' section.
#'
#' @export
#' @param cross_section       sf; A cross section lines feature class.
#' @param xs_number           integer; The cross section identifier of the
#'                            requested cross section.
#' @param dem                 terra SpatRaster; A dem raster.
#' @param channel             sf; A channel polygon feature class (optional).
#' @param floodplain          sf; A floodplain polygon feature class (optional).
#' @param extent_factor       numeric; A numeric value used to expand the map
#'                            extent around each cross section.
#'
#' @return a tmap object
#'
#' @importFrom dplyr %>% filter
#' @importFrom terra crop terrain shade
#' @importFrom grDevices colorRampPalette grey.colors
#' @importFrom grDevices colorRampPalette grey.colors
#' @importFrom tmap tm_shape tm_raster tm_lines tm_text tm_scale_continuous
#'             tm_layout tm_legend tm_pos_out opt_tm_text
#'             tm_compass tm_scalebar tm_title_out tm_add_legend tm_borders
#'
#'
map_xs <- function(cross_section, xs_number, dem,
                   channel = NULL,
                   floodplain = NULL,
                   extent_factor = 1) {
  # Check data structure
  check_cross_section(cross_section, step = "assign_ids")

  # Get DEM spatial reference system
  dem_CRS <- sf::st_crs(dem)

  # Reproject so all layers in the same coordinate system as the DEM
  cross_section_dem <- sf::st_transform(cross_section, crs = dem_CRS)
  if(!is.null(channel)) {
    channel_dem <- sf::st_transform(channel, crs = dem_CRS)
  }
  if(!is.null(floodplain)) {
    floodplain_dem <- sf::st_transform(floodplain, crs = dem_CRS)
  }

  # Subset cross_section for the requested xs_number
  xs_i <- cross_section_dem %>%
    filter(.data$Seq == xs_number)

  # Calculate the map extent for the current cross section
  xs_extent <- fluvgeo::map_extent(feature = xs_i,
                                   extent_factor = extent_factor)
  xs_extent_poly <- sf::st_as_sf(sf::st_as_sfc(xs_extent))

  # Crop the dem to the cross section map extent (+ 20 pixels)
  dem_i <- terra::crop(x = dem,
                       y = terra::ext(xs_extent_poly) + 20)

  # Create a hillshade from dem_i
  slp <- terra::terrain(dem_i, v = "slope", unit = "radians")
  asp <- terra::terrain(dem_i, v = "aspect", unit = "radians")
  hill <- terra::shade(slope = slp, aspect = asp)

  # Create a topo color ramp
  esri_topo <- grDevices::colorRampPalette(colors = c("cadetblue2", "khaki1",
                                                      "chartreuse4", "goldenrod1",
                                                      "orangered4", "saddlebrown",
                                                      "gray70", "white"),
                                           bias = 1,
                                           space = "Lab",
                                           interpolate = "linear")
  # Set the encoding to handle multibyte symbols for flow direction indicator
  pdf.options(encoding = 'CP1250')

  # Specify legend position
  legend_pos <- tm_pos_out(cell.h = "right",
                           cell.v = "center",
                           pos.v = "top",
                           pos.h = "left")

  # Create the cross section map
  # tmap_design_mode()
  xs_map <-
    tm_shape(shp = hill,
             name = "Hillshade") +
    tm_raster(col.scale = tm_scale_continuous(values = "hcl.grays"),
              col.legend = tm_legend(show = FALSE)) +
    tm_shape(shp = dem_i,
             name = "Elevation",
             unit = "ft") +
    tm_raster(col.scale = tm_scale_continuous(values = esri_topo(1000)),
              col_alpha = 0.7,
              col.legend = tm_legend(
                title = "Elevation \n(NAVD88, ft)",
                reverse = TRUE,
                frame = FALSE,
                position = legend_pos)) +
    tm_shape(shp = cross_section_dem,
             name = "Cross Section",
             is.main = TRUE,
             bbox = xs_extent) +
    tm_lines(col = "grey40",
             col_alpha = 0.5,
             lwd = 7) +
    tm_text(text = "\U25BC",    #\\dl, \\gr \\da \\#H0834 \\#H0854 \\U25BC
            size = 3.5,
            col = "dodgerblue",
            col_alpha = 0.4,
            ymod = -0.08,
            options = opt_tm_text(along_lines = TRUE)) +
    tm_text(text = "Seq",
            col = "black",
            size = 1.1,
            fontface = "bold",
            options = opt_tm_text(remove_overlap = FALSE,
                                  shadow = FALSE)) +
    tm_scalebar(width = 25,
                text.size = 0.8,
                position = c("right", "bottom"),
                margins = c(0, 0, 0, 0)) +
    tm_compass(position = c("left", "bottom"),
               margins = c(0, 0, 0, 0)) +
    tm_title_out(text = paste("Cross Section", xs_number),
                 fontface = "bold",
                 padding = c(0, 0.01, 0, 0)) +
    tm_layout(outer.margins = c(0.01, 0, 0, 0),
              outer.bg.color = "white",
              frame.lwd = 3)
  #xs_map

  if(!is.null(channel)) {
    channel_map <- tm_shape(shp = channel_dem,
                            name = "Channel") +
      tm_borders(col = "blue", lwd = 2) +
      tm_add_legend(type = "lines",
                    labels = "Channel",
                    col = "blue",
                    lwd = 2,
                    position = legend_pos)
  }
  #xs_map + channel_map

  if(!is.null(floodplain)) {
    floodplain_map <- tm_shape(shp = floodplain_dem,
                               name = "Floodplain") +
      tm_borders(col = "darkgreen", lwd = 2) +
      tm_add_legend(type = "lines",
                    labels = "Floodplain",
                    col = "darkgreen",
                    lwd = 2,
                    position = legend_pos)
  }
  #xs_map + channel_map + floodplain_map

  # Return the plot
  if(!is.null(channel) & !is.null(floodplain)) return(xs_map + channel_map + floodplain_map)
  if(is.null(channel)) return(xs_map)
}
