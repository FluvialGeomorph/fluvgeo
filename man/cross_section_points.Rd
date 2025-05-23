% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cross_section_points.R
\name{cross_section_points}
\alias{cross_section_points}
\title{Cross Section Points}
\usage{
cross_section_points(cross_section, dem, rem, station_distance)
}
\arguments{
\item{cross_section}{sf object; A cross_section object.}

\item{dem}{terra SpatRast object; A DEM.}

\item{rem}{terra SpatRast object; An REM (aka detrended DEM).}

\item{station_distance}{numeric; Desired distance between the
flowline_points (units: meters)}
}
\value{
an sf object containing a cross_section_points object
}
\description{
Converts a cross section object into a cross section points
             object by creating a series of points along each cross section
             at the specified station distance.
}
