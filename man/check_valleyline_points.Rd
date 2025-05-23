% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_valleyline_points.R
\name{check_valleyline_points}
\alias{check_valleyline_points}
\title{Check the validity of an `fluvgeo` `valleyline_points` data structure}
\usage{
check_valleyline_points(valleyline_points)
}
\arguments{
\item{valleyline_points}{sf: a `valleyline_points`
data structure used by the fluvgeo package.}
}
\value{
Returns TRUE if the `valleyline_points` data structure matches the
requirements. The function throws an error for a data structure not matching
the data specification. Returns errors describing how the the data structure
doesn't match the requirement.
}
\description{
Checks that the input data structure `valleyline_points` meets
the requirements for this data structure.
}
