#' @title Cross section location map
#'
#' @description Produces a cross section location map for the specified cross
#' section.
#'
#' @export
#' @param cross_section       SpatialLinesDataFrame or sf; A cross section lines
#'                            feature class.
#' @param xs_number           integer; The cross section identifier of the
#'                            requested cross section.
#' @param dem                 RasterLayer; A dem raster.
#' @param banklines           SpatialLinesDataFrame or sf; A banklines feature
#'                            class (optional).
#' @param extent_factor       numeric; A numeric value used to expand the map
#'                            extent around each cross section.
#'
#' @return a tmap object
#'
#' @importFrom arcgisbinding arc.open arc.raster
#' @importFrom rgdal showSRID
#' @importFrom raster extent as.raster raster crop resample terrain hillShade
#' @importFrom grDevices colorRampPalette grey
#' @importFrom tmap tm_shape tm_raster tm_lines tm_text tm_add_legend
#' tm_compass tm_scale_bar tm_layout
#'
map_xs <- function(cross_section, xs_number, dem,
                   banklines = NULL,
                   extent_factor = 1) {
  # Check data structure
  check_cross_section(cross_section, step = "assign_ids")
  if(!is.null(banklines)) {
    check_banklines(banklines)
  }

  # Convert to sf
  if(class(cross_section)[1] == "SpatialLinesDataFrame") {
    cross_section_sf <- sf::st_as_sf(cross_section)
  }
  if(class(cross_section)[1] == "sf") {
    cross_section_sf <- cross_section
  }
  if(!is.null(banklines) & class(banklines)[1] == "SpatialLinesDataFrame") {
    banklines_sf <- sf::st_as_sf(banklines)
  }
  if(!is.null(banklines) & class(banklines)[1] == "sf") {
    banklines_sf <- banklines
  }

  # Get valid DEM spatial reference system
  dem_wkt2 <- rgdal::showSRID(dem@crs@projargs)          # convert to valid WKT2
  dem_CRS <- sp::CRS(SRS_string = dem_wkt2)

  # Reproject so all layers in the same coordinate system as the DEM
  cross_section_dem <- sf::st_transform(cross_section_sf, crs = dem_CRS)
  if(!is.null(banklines)) {
    banklines_dem <- sf::st_transform(banklines_sf, crs = dem_CRS)
  }

  # Subset cross_section for the requested xs_number
  xs_i <- cross_section_dem[cross_section_dem$Seq == xs_number, ]

  # Calculate the map extent for the current cross section
  xs_extent <- fluvgeo::feature_extent(feature = xs_i,
                                        extent_factor = extent_factor)
  xs_extent_esri <- c(xmin = xs_extent@xmin,
                      ymin = xs_extent@ymin,
                      xmax = xs_extent@xmax,
                      ymax = xs_extent@ymax)

  # Clip the dem to the cross section map extent
  dem_i <- raster::crop(dem, xs_extent)

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
                         palette = gray.colors(100, 0, 1),
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
            tm_shape(shp = cross_section_dem,
                     name = "Cross Section") +
              tm_lines(col = "grey50", lwd = 7) +
              tm_text(text = "Seq",
                      col = "black",
                      size = 1.2,
                      fontface = "bold",
                      remove.overlap = FALSE,
                      shadow = TRUE,
                      #along.lines = TRUE,                # appears to be broken
                      #overwrite.lines = TRUE             # appears to be broken
                      ) +
            tm_compass(type = "arrow",
                       position = c("right", "bottom")) +
            tm_scale_bar(width = 0.25,
                         position = c("left", "bottom")) +
            tm_layout(main.title = paste("Cross Section", xs_number),
                      legend.outside = TRUE,
                      legend.outside.position = "right",
                      frame.lwd = 3)

  if(!is.null(banklines)) {
    banklines_map <- tm_shape(shp = banklines_dem,
                              name = "Banklines") +
                       tm_lines(col = "blue", lwd = 1,
                                legend.lwd.show = ) +
                       tm_add_legend(type = "line",
                                     labels = "Bankfull",
                                     col = "blue",
                                     lwd = 1)
  }

  # Return the plot
  if(!is.null(banklines)) return(xs_map + banklines_map)
  if(is.null(banklines)) return(xs_map)
}
