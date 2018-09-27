# Graphs


xs_plot <- function(xs_points, stream, xs_number, bankfull_elevation) {
  # Produces a cross section profile plot for the specified cross section and
  # displayes the specified bankfull elevation.
  #
  # Args:
  #    xs_points           character; a data frame of cross section points
  #    stream              character; The name of the stream.
  #    xs_number           integer; The cross section identifier of the
  #                        requested cross section.
  #    bankfull_elevation  numeric; The bankfull elevation (in feet) that is
  #                        used to calculate hydraulic geometry.
  #
  # Returns:
  #    a ggplot object.
  #
  # Subset xs_points for the specified stream and cross section
  xs <- xs_points[xs_points$ReachName == stream & xs_points$Seq == xs_number, ]
  # Calculate transform
  eg <- mean(xs$DEM_Z - xs$Detrend_DEM_Z)
  # Draw the graph
  p <- ggplot(data = as.data.frame(xs),
              aes(POINT_M * 3.28084, Detrend_DEM_Z, label = Seq)) +
    scale_y_continuous(sec.axis = sec_axis(~. + eg,
                                           name = "Elevation (NAVD88 feet)")) +
    geom_line() +
    geom_hline(yintercept = bankfull_elevation, colour = "blue") +
    theme_bw() +
    theme(aspect.ratio = 2/5) +
    labs(title = paste("Cross Section ", as.character(xs_number)),
         x = "Station Distance (feet, from right descending bank)",
         y = "Detrended Elevation (feet)") +
    theme(plot.title = element_text(hjust = 0)
    )
  return(p)
}

gof_graph <- function(gof_stats_gather, stream, bankfull_elevation,
                      stat = "MAE") {
  # Produces a goodness of fit graph for the current reach by analysis
  # regions.
  #
  # Args:
  #    gof_stats_gather    data frame;
  #    stream              character; The name of the stream.
  #    bankfull_elevation  numeric; the detrended bankfull elevation to
  #                        to display on the graph
  #    stat                character; The statistic to graph "RMSE", "MAE" the
  #                        default.
  #
  # Returns:
  #    a ggplot object.
  #
  # Subset gof_stats_gather to include only the specified stream
  gof_stats_stream <- filter(gof_stats_gather, ReachName == stream)
  # Use the stat parameter to select which statistics to graph
  if(stat == "MAE") {
    # Subset gof_stats_stream to select MAE stats
    gsg <- filter(gof_stats_stream, stats == "XS_Area_mae" | stats == "XS_Width_mae" | stats == "XS_Depth_mae")
    # Control facet names
    stat_names <- c(`XS_Area_mae`  = "Area (sq feet)",
                    `XS_Width_mae` = "Width (feet)",
                    `XS_Depth_mae` = "Depth (feet)")
  }
  if(stat == "RMSE") {
    # Subset gof_stats_stream to select RMSE stats
    gsg <- filter(gof_stats_stream, stats == "XS_Area_rmse" | stats == "XS_Width_rmse" | stats == "XS_Depth_rmse")
    # Control facet names
    stat_names <- c(`XS_Area_rmse`  = "Area (sq feet)",
                    `XS_Width_rmse` = "Width (feet)",
                    `XS_Depth_rmse` = "Depth (feet)")
  }
  # Draw the plot
  q <- ggplot(data = gsg,
              aes(x = Bankfull_Elevation, y = measure, color = Regions)) +
    geom_line(size = 0.75) +
    facet_grid(facets = stats ~ .,
               scales = "free",
               labeller = as_labeller(stat_names)) +
    theme_bw() +
    geom_vline(xintercept = bankfull_elevation, colour = "black") +
    scale_color_brewer(palette = "Set1") +
    theme(legend.direction = "vertical",
          legend.position = c(0.15, 0.9),
          legend.key.size = unit(0.3, "cm"),
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 9)
    ) +
    labs(x = "Detrended Bankfull Elevation (feet)",
         y = stat)
  return(q)
}

reach_RHG_graph <- function(xs_dims, stream, bf_elevation, xs_trend = FALSE, log_scale = FALSE) {
  # Produces a regional hydraulic geometry graph for the specified stream(s).
  #
  # Args:
  #    xs_dims:            data frame; a data frame of cross section
  #                        dimensions
  #    stream              character; Either a single value or vector. If a
  #                        single value, then only that stream will be
  #                        displayed. If a vector of stream names is specified
  #                        all streams in the vector will be displayed.
  #    bf_elevation        numeric; The bankfull elevation (in feet) that is
  #                        used to calculate hydraulic geometry for each
  #                        specified stream. Either a single value or vector.
  #                        If a single value, then the
  #
  # Returns:
  #    a ggplot object.
  #
  # Create a data frame to hold only the specified stream(s) and the respective
  # bankfull elevation(s).
  xs_dims_graph <- data.frame()
  # Iterate through stream(s)
  for ( i in 1:length(stream) ) {
    # Filter to select only the current stream.
    xs_dims_stream <- filter(xs_dims, ReachName == stream[i])
    # Filter by the respective bankfull elevation for the current stream.
    xs_dims_stream_bankfull <- filter(xs_dims_stream, Bankfull_Elevation == bf_elevation[i])
    # Append the current stream and bankfull records to the running total
    xs_dims_graph <- rbind(xs_dims_graph, xs_dims_stream_bankfull)
  }
  # Subset xs_dims_graph to select regional curve data (no DEM derived cross sections) at the specified bankfull_elevation
  xsd_regions <- filter(xs_dims_graph, XS_Type != "DEM derived cross section")
  # Subset xs_dims_stream to selct only DEM derived cross sections at the specified bankfull_elevation
  xsd_dem <- filter(xs_dims_graph, XS_Type == "DEM derived cross section")
  # Gather xs dimensions into key-value fields (convert wide to long format) for graphing
  xsd_regions_gather <- gather(data = xsd_regions,
                               key = stats, value = measure,
                               XS_Area, XS_Width, XS_Depth)
  xsd_dem_gather     <- gather(data = xsd_dem,
                               key = stats, value = measure,
                               XS_Area, XS_Width, XS_Depth)
  # Control facet names
  stat_names <- c(`XS_Area`  = "Area (sq feet)",
                  `XS_Width` = "Width (feet)",
                  `XS_Depth` = "Depth (feet)")
  # Define the base plot
  p <- ggplot(data = unique(xsd_regions_gather),
              aes(x = Drainage_Area, y = measure, color = XS_Type)) +
    scale_color_brewer(palette = "Set1") +
    facet_grid(facets = stats ~ .,
               scales = "free",
               labeller = as_labeller(stat_names)) +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.direction = "vertical") +
    labs(x = "Drainage Area (square miles)", y = "")
  # Regional curves
  regional_curves <- geom_line(size = 1)
  # Define cross section points
  xs_pts <- geom_point(inherit.aes = FALSE,
                       data = unique(xsd_dem_gather),
                       aes(x = Drainage_Area, y = measure),
                       size = 1,
                       show.legend = FALSE)
  xs_labels <- geom_text_repel(inherit.aes = FALSE,
                               data = unique(xsd_dem_gather),
                               aes(x = Drainage_Area, y = measure, label = Cross_Section),
                               show.legend = FALSE,
                               size = 2)
  # Draw a smooth line through cross section points
  xs_trend_line <- geom_smooth(inherit.aes = FALSE,
                               data = unique(xsd_dem_gather),
                               aes(x = Drainage_Area, y = measure))
  # Transform to log scales
  log_scale_y <- scale_y_log10()
  log_scale_x <- scale_x_log10()
  # Define title
  title <- labs(title = paste("Detrended Bankfull Elevation:", bf_elevation, "(feet)"))
  # If only one stream, display cross section points, labels, and title
  if (length(bf_elevation) == 1) {
    return(p + regional_curves + xs_pts + xs_labels + title)
  }
  # If more than one stream, display cross section points
  if (length(bf_elevation) > 1 & xs_trend == FALSE) {
    return(p + regional_curves + xs_pts)
  }
  if (length(bf_elevation) > 1 & xs_trend == TRUE & log_scale == FALSE) {
    return(p + regional_curves + xs_pts + xs_trend_line)
  }
  if (length(bf_elevation) > 1 & xs_trend == TRUE & log_scale == TRUE) {
    return(p + regional_curves + xs_pts + xs_trend_line + log_scale_y + log_scale_x)
  }
}

