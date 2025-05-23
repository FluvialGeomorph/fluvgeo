% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hydroflatten_dem.R
\name{hydroflatten_dem}
\alias{hydroflatten_dem}
\title{Hydroflatten DEM}
\usage{
hydroflatten_dem(dem, trend, relative_water_depth)
}
\arguments{
\item{dem}{terra SpatRaster object; A DEM. Elevation
units = feet.}

\item{trend}{terra SpatRaster object; A trend raster
produced using the detrend function.}

\item{relative_water_depth}{numeric; The relative water depth expressed as
height above the trend surface. Depth
units = feet.}
}
\value{
A terra SpatRaster object representing a DEM hydroflattened to
the specified water depth.
}
\description{
Hydroflattens a DEM.
}
