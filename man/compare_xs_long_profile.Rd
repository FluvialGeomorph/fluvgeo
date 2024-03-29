% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare_xs_long_profile.R
\name{compare_xs_long_profile}
\alias{compare_xs_long_profile}
\title{Compare cross section longitudinal profiles from multiple surveys}
\usage{
compare_xs_long_profile(
  stream,
  xs_pts_sf_list,
  features_sf = NULL,
  profile_units = "feet",
  label_xs = TRUE,
  xs_label_freq = 10
)
}
\arguments{
\item{stream}{character; The name of the stream.}

\item{xs_pts_sf_list}{list; a list of `sf` objects of cross
section points, one for each survey time period
to be graphed. Survey list items must be tagged
with the survey label to be used in the graph
legend.}

\item{features_sf}{`sf` object` of infrastructure features.}

\item{profile_units}{character; the units of the longitudinal profile.
One of "kilometers", "meters", "miles", or "feet"}

\item{label_xs}{logical; Draw the cross section labels?}

\item{xs_label_freq}{numeric; An integer indicating the frequency of
cross section labels.}
}
\value{
A ggplot2 object.
}
\description{
Compare cross section longitudinal profiles from multiple
surveys.
}
\details{
This function is used to plot the cross section longitudinal
profile from a series of \code{xs_points} data frames representing
multiple surveys.
}
\seealso{
This function requires a
\code{flowline_points} sf object. See the
\code{sin_flowline_points_sf} package dataset for an example of this format
of cross section data produced by the \code{FluvialGeomorph} ArcGIS toolbox.
}
