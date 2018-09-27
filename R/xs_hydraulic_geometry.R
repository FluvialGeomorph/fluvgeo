# XS Hydraulic Geometry

RHG <- function(region = "Eastern United States", drainageArea, dimensionType) {
  # Computes a hydraulic dimension (cross sectional area, width, or depth)
  # from a regional hydraulic curve.
  #
  # Args:
  #    region:         character; The region that a dimension will be
  #                    calculated for. See the regional_curves$region field
  #                    for a complete list.
  #    drainageArea:   numeric; The upstream drainage area of the location of
  #                    the dimension.
  #    dimensionType:  character; The dimension type: "area", "depth", "width"
  #
  # Returns:
  #    the value of the requested hydraulic dimension
  #
  # Subset the RHG curve for the selected region and dimension
  rc <- regional_curves[regional_curves$region == region & regional_curves$dimension == dimensionType,]
  # Calculate the hydrologic geometry for the selected region and dimension
  dimension <- rc$y_1 * drainageArea^(log( rc$y_2 / rc$y_1 ) / (log(rc$x_2 / rc$x_1)))
  # Return the calculated dimensions in a data frame
  return(dimension)
}

xs_GeometryTable <- function(xs_points, stream, xs_number, bankfull_elevation) {
  # Calculate hydraulic geometry for the specified cross section at the
  # specified bankfull elevation.
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
  #    a data frame of hydraulic dimensions for the specified cross section
  #    at the specified bankfull elevation.
  #
  # Subset xs_points for the current cross section
  xs <- na.omit(xs_points[xs_points$ReachName == stream & xs_points$Seq == xs_number,])
  xsStations   <- xs$POINT_M * 3.28084          # Convert meters to feet
  xsElevations <- xs$Detrend_DEM_Z              # Elevations already in feet
  drainageArea <- unique(xs$Watershed_Area_SqMile)
  ## Calculate the cross sectional area under a proposed bankfull elevation
  # Approach: the area between two curves is equal to the integral of the difference between the two curves
  # Calculate the difference between the two curves. This results in a new curve.
  # Then approximate a function for this "difference" curve.
  f1 <- approxfun(x = xsStations , y = bankfull_elevation - xsElevations)
  # Remove values above the bankfull elevation (negative values)
  f2 <- function(x) ifelse(f1(x) < 0, 0, f1(x))
  # Calculate the cross sectional area by taking the integral of the area under the "difference" curve.
  xsArea <- integrate(f = f2, lower = min(xsStations), upper = max(xsStations), subdivisions = 100000, stop.on.error = FALSE)
  # Calculate cross sectional width (cross section spacing * # of xs depths > 0)
  d1 <- f2(xsStations)
  xsWidth <- mean(diff(xsStations)) * length(d1[d1>0])
  # Calculate cross sectional mean depth
  xsDepth <- mean(f1(xsStations)[f1(xsStations) > 0])
  # Calculate width-depth ratio
  xsWidthDepth <- xsWidth * xsDepth
  # Define type of cross section
  xs_type <- c("DEM derived cross section")
  # build data frame of results
  dims <- data.frame(stream, xs_number, xs_type, bankfull_elevation, drainageArea, xsArea$value, xsWidth, xsDepth, xsWidthDepth)
  colnames(dims) <- c("ReachName","Cross_Section","XS_Type","Bankfull_Elevation","Drainage_Area","XS_Area","XS_Width","XS_Depth","XS_Width_Depth_Ratio")
  return(dims)
}

xs_RegionalGeometry <- function(xs_points, stream, xs_number, bankfull_elevation, region) {
  # Calculates cross section geometry and regional hydraulic geometry
  # dimensions for the specified cross section at the specified bankfull
  # elevation.
  #
  # Args:
  #    xs_points           character; a data frame of cross section points
  #    stream              character; The name of the stream.
  #    xs_number           integer; The cross section identifier of the
  #                        requested cross section.
  #    bankfull_elevation  numeric; The bankfull elevation (in feet) that is
  #                        used to calculate hydraulic geometry.
  #    region:             character; The region that a dimension will be
  #                        calculated for. See the regional_curves$region
  #                        field for a complete list.
  #
  # Returns:
  #    a data frame of hydraulic dimensions for the specified cross section
  #    at the specified bankfull elevation and the regional hydraulic
  #    dimensions.
  #
  # Extract drainage area for the current xs from `XS_points` attribute table
  drainage_area <- unique(xs_points[xs_points$ReachName == stream & xs_points$Seq == xs_number,]$Watershed_Area_SqMile)
  # Calculate xs dimensions for current cross section
  xs_geom <- xs_GeometryTable(xs_points = xs_points, stream = stream, xs_number = xs_number, bankfull_elevation = bankfull_elevation)
  # Calculate RHG channel dimensions for current cross section
  RHG_xs_area <- RHG(region, drainage_area, "area")
  RHG_width   <- RHG(region, drainage_area, "width")
  RHG_depth   <- RHG(region, drainage_area, "depth")
  # Build a data frame of RHG results
  rhg <- data.frame(stream, xs_number, region, bankfull_elevation, drainage_area, RHG_xs_area, RHG_width, RHG_depth)
  colnames(rhg) <- c("ReachName","Cross_Section","XS_Type","Bankfull_Elevation","Drainage_Area","XS_Area","XS_Width","XS_Depth")
  # Build a data frame of xs geometry results
  xsg <- xs_geom[,c("ReachName","Cross_Section","XS_Type","Bankfull_Elevation","Drainage_Area","XS_Area","XS_Width","XS_Depth")]
  # rbind rhg dimensions to xsg
  dims <- rbind(rhg, xsg)
  return(dims)
}

xs_Dimensions <- function(xs_points, streams, regions, bankfull_elevations) {
  # Creates a data frame to hold hydraulic geometry dimensions (area, width,
  # depth) by cross section, xs type, and bankfull elevation.
  #
  # Args:
  #    xs_points             character; a data frame of cross section points
  #    streams:              character vector; The stream names in the study
  #                          area.
  #    regions:              character vector; The regions that dimensions
  #                          will be calculated for. See the
  #                          regional_curves$region field for a complete list.
  #    bankfull_elevations:  numeric vector; The bankfull elevations (in feet)
  #                          that are used to calculate hydraulic geometry.
  #
  # Returns:
  #    a data frame of cross section hydraulic geometry dimensions (area,
  #    width, depth) by cross section, xs type, and bankfull elevation.
  #
  xs_dims <- tibble(ReachName         = character(),
                    Cross_Section      = integer(),
                    XS_Type            = character(),
                    Bankfull_Elevation = double(),
                    Drainage_Area      = double(),
                    XS_Area            = double(),
                    XS_Width           = double(),
                    XS_Depth           = double())
  # Iterate through stream reaches
  for (g in streams) {
    # Iterate through regions
    for (h in regions) {
      # Iterate through bankfull elevations
      for (i in bankfull_elevations) {
        # Iterate through cross sectiions
        for (j in as.integer(levels(as.factor(xs_points[xs_points$ReachName == g,]$Seq)))) {
          # Calculate current cross section geometry
          xs_dim <- xs_RegionalGeometry(xs_points = xs_points, stream = g, xs_number = j, bankfull_elevation = i, region = h)
          # Append current xs geometry to xs_dims data frame
          xs_dims <- rbind(xs_dims, xs_dim)
        }
      }
    }
  }
  return(xs_dims)
}

Build_GOF_Stats <- function(xs_dims, streams, regions, bankfull_elevations) {
  # Creates a data frame of goodness of fit scores of the relationship
  # between cross section geometry and regional hydraulic geometry
  # dimensions.
  #
  # Args:
  #    xs_dims:              data frame; a data frame of cross section
  #                          dimensions
  #    streams:              character vector; The stream names in the study
  #                          area.
  #    regions:              character vector; The regions that dimensions
  #                          will be calculated for. See the
  #                          regional_curves$region field for a complete list.
  #    bankfull_elevations:  numeric vector; Detrended bankfull elevations
  #                          (in feet) that are used to calculate hydraulic
  #                          geometry.
  #
  # Returns:
  #    a data frame of goodness of fit scores  of the relationship
  #    between cross section geometry and regional hydraulic geometry
  #    dimensions.
  #
  # Create a data frame to hold goodness of fit scores
  gof_stats <- tibble(ReachName          = character(),
                      Regions            = character(),
                      Bankfull_Elevation = double(),
                      XS_Area_rmse       = double(),
                      XS_Width_rmse      = double(),
                      XS_Depth_rmse      = double(),
                      XS_Area_mae        = double(),
                      XS_Width_mae       = double(),
                      XS_Depth_mae       = double())
  # Calculate goodness of fit statistics
  # Iterage through streams
  for (g in streams) {
    # Iterate through regions
    for (h in regions) {
      # Iterate through bankfull elevations
      for (i in bankfull_elevations) {
        # Calculate goodness of fit statistics
        XS_Area_rmse  <- rmse(actual    = xs_dims[xs_dims$ReachName == g & xs_dims$XS_Type == h                           & xs_dims$Bankfull_Elevation == i,]$XS_Area,
                              predicted = xs_dims[xs_dims$ReachName == g & xs_dims$XS_Type == "DEM derived cross section" & xs_dims$Bankfull_Elevation == i,]$XS_Area)
        XS_Width_rmse <- rmse(actual    = xs_dims[xs_dims$ReachName == g & xs_dims$XS_Type == h                           & xs_dims$Bankfull_Elevation == i,]$XS_Width,
                              predicted = xs_dims[xs_dims$ReachName == g & xs_dims$XS_Type == "DEM derived cross section" & xs_dims$Bankfull_Elevation == i,]$XS_Width)
        XS_Depth_rmse <- rmse(actual    = xs_dims[xs_dims$ReachName == g & xs_dims$XS_Type == h                           & xs_dims$Bankfull_Elevation == i,]$XS_Depth,
                              predicted = xs_dims[xs_dims$ReachName == g & xs_dims$XS_Type == "DEM derived cross section" & xs_dims$Bankfull_Elevation == i,]$XS_Depth)
        XS_Area_mae   <- mae(actual     = xs_dims[xs_dims$ReachName == g & xs_dims$XS_Type == h                           & xs_dims$Bankfull_Elevation == i,]$XS_Area,
                             predicted  = xs_dims[xs_dims$ReachName == g & xs_dims$XS_Type == "DEM derived cross section" & xs_dims$Bankfull_Elevation == i,]$XS_Area)
        XS_Width_mae  <- mae(actual     = xs_dims[xs_dims$ReachName == g & xs_dims$XS_Type == h                           & xs_dims$Bankfull_Elevation == i,]$XS_Width,
                             predicted  = xs_dims[xs_dims$ReachName == g & xs_dims$XS_Type == "DEM derived cross section" & xs_dims$Bankfull_Elevation == i,]$XS_Width)
        XS_Depth_mae  <- mae(actual     = xs_dims[xs_dims$ReachName == g & xs_dims$XS_Type == h                           & xs_dims$Bankfull_Elevation == i,]$XS_Depth,
                             predicted  = xs_dims[xs_dims$ReachName == g & xs_dims$XS_Type == "DEM derived cross section" & xs_dims$Bankfull_Elevation == i,]$XS_Depth)
        # Combine individual goodness of fit stats into a data frame
        gfs <- data.frame(g, h, i, XS_Area_rmse, XS_Width_rmse, XS_Depth_rmse, XS_Area_mae, XS_Width_mae, XS_Depth_mae)
        colnames(gfs) <- c("ReachName","Regions","Bankfull_Elevation","XS_Area_rmse","XS_Width_rmse","XS_Depth_rmse","XS_Area_mae","XS_Width_mae","XS_Depth_mae")
        # Append current region and bankfull elevation scores to gof_stats table
        gof_stats <- rbind(gof_stats, gfs)
      }
    }
  }
  return(gof_stats)
}
