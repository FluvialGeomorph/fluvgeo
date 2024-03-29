% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map_reach_metric.R
\name{map_reach_metric}
\alias{map_reach_metric}
\title{Reach Metric Map}
\usage{
map_reach_metric(
  metric,
  flowline_sf,
  xs_dimensions_sf,
  xs_label_freq = 2,
  background = "none",
  exaggeration = 20,
  extent_factor = 1.1
)
}
\arguments{
\item{metric}{MetricThreshold object; the fluvial geomorphic
metric to be mapped}

\item{flowline_sf}{sf object; a flowline feature class.}

\item{xs_dimensions_sf}{sf object; a cross section dimensions feature
class.}

\item{xs_label_freq}{numeric; An integer indicating the frequency of
cross section labels.}

\item{background}{character; The type of map background. One of
"aerial", "elevation", or "none".}

\item{exaggeration}{numeric; The degree of terrain exaggeration.}

\item{extent_factor}{numeric; The amount the extent is expanded around
the cross section feature class. Values greater
than one zoom out, values less than one zoom in.}
}
\value{
a tmap object

# Define geomorphic metric
wdr <- new(Class = "FluvialGeomorphicMetric",
           metric = "Width Depth Ratio",
           definition = "bankfull width / bankfull depth",
           variable = "xs_width_depth_ratio",
           threshold_breaks = c(0, 10, 20, Inf),
           threshold_labels = c("Incised",
                                "Stable",
                                "Overwidened"),
           source = "Dunn & Leopold, 1978")

# Create the reach metric map
wdr_map <- map_reach_metric(wdr,
                            fluvgeo::sin_flowline_sf,
                            fluvgeo::sin_riffle_floodplain_dims_L3_sf)
print(wdr_map)
}
\description{
Produces a reach fluvial geomorphic metric thematic map
  displaying the metric values at each cross section.
}
