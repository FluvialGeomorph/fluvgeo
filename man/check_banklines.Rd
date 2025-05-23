% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_banklines.R
\name{check_banklines}
\alias{check_banklines}
\title{Check the validity of an `fluvgeo` `banklines` data structure}
\usage{
check_banklines(banklines)
}
\arguments{
\item{banklines}{sf object; a `banklines` data structure used
by the fluvgeo package.}
}
\value{
Returns TRUE if the `banklines` data structure matches the
requirements. The function throws an error for a data structure not matching
the data specification. Returns errors describing how the the data structure
doesn't match the requirement.
}
\description{
Checks that the input data structure `banklines` meets
the requirements for this data structure.
}
