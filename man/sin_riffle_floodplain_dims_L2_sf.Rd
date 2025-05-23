% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sin_riffle_floodplain_dims_L2_sf.R
\docType{data}
\name{sin_riffle_floodplain_dims_L2_sf}
\alias{sin_riffle_floodplain_dims_L2_sf}
\title{Sinsinawa Creek, IL stream riffle cross section lines}
\format{
A \code{sf} with 10 observations and 61 variables.

\describe{
    \item{OBJECTID}{numeric; The ArcGIS feature class assigned unique
                    identifier for each point in the dataset.}
    \item{Seq}{numeric; The cross section unique identifier.}
    \item{OBJECTID_1}{numeric; The ArcGIS feature class assigned unique
                    identifier for each point in the dataset.}
    \item{Watershed_Area_SqMile}{numeric; The upstream watershed drainage
                                 area.}
    \item{from_measure}{numeric; The start longitudinal stationing value
                       for the reach in km.}
    \item{to_measure}{numeric; The end longitudinal stationing value for the
                       reach in km.}
    \item{ReachName}{character; The name of the reach.}
    \item{POINT_X}{numeric; The x-coordinate value (longitude).}
    \item{POINT_Y}{numeric; The y-coordinate value (latitude).}
    \item{POINT_M}{numeric; The m-coordinate value (route position).}
    \item{Z}{numeric;The z-coordinate value (elevation).}
    \item{km_to_mouth}{numeric; The distance to the mouth of the site in km.}
    \item{Z_smooth}{numeric; The smoothed z-coordinate value (elevation).}
    \item{upstream_x}{numeric; The x-coordinate value (longitude) of the next
                      upstream point in the moving window.}
    \item{upstream_y}{numeric; The y-coordinate value (latitude) of the next
                     upstream point in the moving window.}
    \item{downstream_x}{numeric; The x-coordinate value (longitude) of the
                       next downsteam point in the moving window.}
    \item{downstream_y}{numeric; The y-coordinate value (latitude) of the
                       next downsteam point in the moving window.}
    \item{upstream_z}{numeric; The z-coordinate value (elevation) of the
                     next upstream point in the moving window.}
    \item{downstream_z}{numeric; The z-coordinate value (elevation) of
                       the next downstream point in the moving window.}
    \item{upstream_m}{numeric; The m-coordinate value (route position) of the
                     next upstream point in the moving window.}
    \item{downstream_m}{numeric; The m-coordinate value (route position) of
                       the next downstream point in the moving window.}
    \item{rise}{numeric; The elevation difference between the downstream and
               upstream point in the moving window, units are in feet.}
    \item{run}{numeric; The longitudinal distance along the stream network
               between the downstream and upstream point in the moving window,
               units are in feet.}
    \item{stream_length}{numeric; 	The longitudinal distance along the stream
                         network between the downstream and upstream point in
                         the moving window, units are in feet.}
    \item{valley_length}{numeric; The longitudinal distance along the
                         valleyline between the downstream and upstream
                         stream network point in the moving window, units are
                         in feet.}
    \item{sinuosity}{numeric; The sinuosity of the stream within the moving
                   window (valley_length/stream_length).}
    \item{sinuosity_gte_one}{numeric;The sinuosity metric, values greater than
                           or equal to one.}
    \item{slope}{numeric; The slope of the stream within the moving window
               (rise/run).}
    \item{slope_gte_zero}{numeric; The slope metric, values greater than of
                         equal to zero.}
    \item{bankfull_elevation}{numeric; The detrended bankfull elevation
                      (in feet) that is used to calculate hydraulic
                      geometry.}
    \item{drainage_area}{numeric;The drainage area (single value or vector)
                         used to calculate discharge.}
    \item{xs_area}{numeric; The cross sectional area at the specified
                 detrended bankfull elevation, units: square feet.}
    \item{xs_width}{numeric; The cross section width at the specified
                   detrended bankfull elevation, units: feet.}
    \item{xs_depth}{numeric; The maximum depth at the specified
                   detrended bankfull elevation, units: detrended
                   feet.}
    \item{discharge}{numeric; The estimated discharge at the specified
                       drainage area.}
    \item{fp_area}{numeric; The cross sectional area at the flood prone
                  elevation, units: square feet.}
    \item{fp_width}{numeric; The cross section width at the flood prone
                   elevation, units: feet.}
    \item{fp_depth}{numeric; The maximum depth at the flood prone
                   elevation, units: feet.}
    \item{xs_width_depth_ratio}{numeric; The bankfull width to bankfull
                               depth ratio.}
    \item{xs_width_depth_ratio_gte_one}{numeric; The bankfull width to bankfull
                               depth ratio, greater than or equal to one.}
    \item{xs_entrenchment_ratio}{numeric; The entrenchment ratio (flood
                               prone width) / (bankfull width).}
    \item{xs_entrenchment_ratio_gte_one}{numeric; The entrenchment ratio (flood
                               prone width) / (bankfull width), greater than
                               or equal to one.}
    \item{watersurface_elev}{numeric; The water surface elevation,
                           units: NAVD88 feet.}
    \item{bankfull_elev}{numeric; The bankfull elevation, units: NAVD88
                       feet.}
    \item{floodprone_elev}{numeric; The flood prone elevation, units:
                          NAVD88 feet.}
    \item{xs_mean_depth}{numeric; The cross section mean depth in feet at the
                         estimated bankfull water surface elevation.}
    \item{shear_stress_density}{numeric; The fluid shear stress, calculated
                               using the density of water.}
    \item{shear_stress_density_gte_zero}{numeric; The fluid shear stress
                                         density metric, greater than or
                                         equal to zero.}
    \item{shear_stress_weight}{numeric; The fluid shear stress, calculated
                             using the specific weight of water.}
    \item{shear_stress_weight_gte_zero}{numeric; The fluid shear stress
                                       specific weight metric, greater than
                                       or equal to zero.}
    \item{shear_stress_lane}{numeric; The fluid shear stress, calculated
                             using the Lane’s Balance method.}
    \item{shear_stress_lane_gte_zero}{numeric; The fluid shear stress Lane’s
                                     metric, greater than or equal to zero.}
    \item{stream_power}{numeric; The amount of energy the water is exerting on
                       the channel.}
    \item{stream_power_gte_zero}{numeric; The stream power, greater than or
                                 equal to zero.}
    \item{stream_power_lane}{numeric; The stream power, calculated using the
                             Lane's Balance method.}
    \item{stream_power_lane_gte_zero}{numeric; The fluid shear stress Lane’s
                                     metric, greater than or equal to zero. }
    \item{unit_stream_power}{numeric; The unit stream power.}
    \item{unit_stream_power_gte_zero}{numeric; The unit stream power metric,
                                     greater than or equal to zero.}
    \item{loop}{numeric; The loop number.}
    \item{bend}{numeric; The bend number.}
}
}
\source{
This dataset was produced using the \code{FluvialGeomorph} ArcGIS
toolbox.
}
\usage{
sin_riffle_floodplain_dims_L2_sf
}
\description{
A \code{sf} object containing level 2 cross section dimensions
for Sinsinawa Creek, IL. Cross section dimension polyline feature classes are
used to store the hydraulic dimensions calculated for each cross section.
}
\keyword{datasets}
