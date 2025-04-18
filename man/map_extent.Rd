% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map_extent.R
\name{map_extent}
\alias{map_extent}
\title{Calculate Expanded Map Extent}
\usage{
map_extent(feature, extent_factor = 1)
}
\arguments{
\item{feature}{sf; A feature class.}

\item{extent_factor}{numeric; A numeric value used to expand the extent
of the supplied feature class.}
}
\value{
an sf::st_bbox object
}
\description{
Creates a new expanded map extent based on a supplied feature
and a map extent multiplying factor.
}
