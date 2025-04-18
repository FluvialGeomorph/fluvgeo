% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_flowline_points.R
\name{check_flowline_points}
\alias{check_flowline_points}
\title{Check the validity of an `fluvgeo` `flowline_points` data structure}
\usage{
check_flowline_points(flowline_points)
}
\arguments{
\item{flowline_points}{sf object; a `flowline_points` data
structure used by the fluvgeo package.}
}
\value{
Returns TRUE if the `flowline_points` data structure matches the
requirements. The function throws an error for a data structure not matching
the data specification. Returns errors describing how the the data structure
doesn't match the requirement.
}
\description{
Checks that the input data structure `flowline_points` meets
the requirements for this data structure.
}
