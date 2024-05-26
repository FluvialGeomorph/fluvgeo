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
#' @importFrom tmap tm_shape tm_raster tm_lines tm_text tm_add_legend
#'             tm_compass tm_scale_bar tm_layout tm_borders
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
  # Create the cross section map
  xs_map <- tm_shape(shp = hill,
                     name = "Hillshade") +
    tm_raster(style = "cont",
              palette = gray.colors(100, 0, 1),
              legend.show = FALSE) +
    tm_shape(shp = dem_i,
             name = "Elevation",
             unit = "ft") +
    tm_raster(col = "Band_1",
              style = "cont",
              palette = esri_topo(1000),
              alpha = 0.7,
              title = "Elevation \n (NAVD88, ft)",
              legend.show = TRUE) +
    tm_shape(shp = cross_section_dem,
             name = "Cross Section",
             bbox = xs_extent,
             is.master = TRUE) +
    tm_lines(col = "grey50", lwd = 7) +
    tm_text(text = "Seq",
            col = "black",
            size = 1.2,
            fontface = "bold",
            remove.overlap = FALSE,
            shadow = TRUE) +
    tm_compass(type = "arrow",
               position = c("right", "bottom")) +
    tm_scale_bar(width = 0.25,
                 position = c("left", "bottom")) +
    tm_layout(main.title = paste("Cross Section", xs_number),
              legend.outside = TRUE,
              legend.outside.position = "right",
              outer.bg.color = "white",
              outer.margins = c(0, 0, 0, 0),
              frame.lwd = 3)

  if(!is.null(channel)) {
    channel_map <- tm_shape(shp = channel_dem,
                            name = "Channel") +
      tm_borders(col = "blue", lwd = 2) +
      tm_add_legend(type = "line",
                    labels = "Channel",
                    col = "blue",
                    lwd = 2)
  }
  if(!is.null(floodplain)) {
    floodplain_map <- tm_shape(shp = floodplain_dem,
                               name = "Floodplain") +
      tm_borders(col = "forestgreen", lwd = 2) +
      tm_add_legend(type = "line",
                    labels = "Floodplain",
                    col = "forestgreen",
                    lwd = 2)
  }

  # Return the plot
  if(!is.null(channel) & !is.null(floodplain)) return(xs_map + channel_map + floodplain_map)
  if(is.null(channel)) return(xs_map)
}

