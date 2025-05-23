% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sf_point_attributes.R
\name{sf_point_attributes}
\alias{sf_point_attributes}
\title{Calculate x and y coordinate values as new fields.}
\usage{
sf_point_attributes(points, field_names = c("X", "Y"))
}
\arguments{
\item{points}{sf object of type POINT, The input point features.}

\item{field_names}{character, Provide custom names for the coordinate
fields to be created. Names must be provided in the
sf Dimension order (i.e,, XY, XTM, XYZ, XYZM).}
}
\value{
sf object of type POINT
}
\description{
Calculate x and y coordinate values as new fields.
}
\details{
based on jmlondon suggestion for an `sf_as_cols` function:
[sf/issues/231](https://github.com/r-spatial/sf/issues/231)
}
