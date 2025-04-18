% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_flowline.R
\name{check_flowline}
\alias{check_flowline}
\title{Check the validity of an `fluvgeo` `flowline` data structure}
\usage{
check_flowline(flowline, step = c("create_flowline", "profile_points"))
}
\arguments{
\item{flowline}{sf: a `flowline` data structure
used by the fluvgeo package.}

\item{step}{character; last completed processing step. One of
"create_flowline", "profile_points"}
}
\value{
Returns TRUE if the `flowline` data structure matches the
requirements. The function throws an error for a data structure not matching
the data specification. Returns errors describing how the the data structure
doesn't match the requirement.
}
\description{
Checks that the input data structure `flowline` meets
the requirements for this data structure.
}
