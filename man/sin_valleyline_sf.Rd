% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sin_valleyline_sf.R
\docType{data}
\name{sin_valleyline_sf}
\alias{sin_valleyline_sf}
\title{Sinsinawa Creek, IL stream valleyline}
\format{
A \code{sf} with 1 observations and 7
    variables.
\describe{
    \item{OBJECTID}{numeric; The ArcGIS feature class assigned unique
                    identifier for each point in the dataset.}
    \item{ReachName}{character; 	Name of the reach.}
    \item{from_measure}{numeric; .The start longitudinal stationing value for
                      the reach in km.}
    \item{to_measure}{numeric; The end longitudinal stationing value for the
                       reach in km.}
}
}
\source{
This dataset was produced using the \code{FluvialGeomorph} ArcGIS
toolbox.
}
\usage{
sin_valleyline_sf
}
\description{
A \code{sf} object containing stream
gradient line for Sinsinawa Creek, IL. This polyline sf object represents
the trend line of the down-valley axis. It bisects stream meanders as the
 stream swings from side to side across its floodplain.
}
\keyword{datasets}
