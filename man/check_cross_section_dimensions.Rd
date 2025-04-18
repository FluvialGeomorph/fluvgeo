% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_cross_section_dimensions.R
\name{check_cross_section_dimensions}
\alias{check_cross_section_dimensions}
\title{Check the validity of an `fluvgeo` `cross_section_dimension` data
structure}
\usage{
check_cross_section_dimensions(
  cross_section_dimensions,
  step = c("level_1", "cross_section_dimensions", "shear_stress", "stream_power",
    "planform", "metric_ratios")
)
}
\arguments{
\item{cross_section_dimensions}{data frame, sf: A
`cross_section_dimension` data structure
used by the fluvgeo package.}

\item{step}{character; Last completed processing step. One of
"level_1",
"cross_section_dimensions", "shear_stress,
"stream_power", "planform", "metric_ratios"}
}
\value{
Returns TRUE if the `cross_section_dimension` data structure matches
the requirements. The function throws an error for a data structure not
matching the data specification. Returns errors describing how the the data
structure doesn't match the requirement.
}
\description{
Checks that the input data structure `cross_section_dimension`
meets the requirements for this data structure.
}
\details{
Cross section dimension feature classes evolve as different steps
are performed on them. The `step` parameter allows a `cross section_dimension`
data structure to be checked throughout its lifecycle. Each step defines a
changing set of requirements for the `cross section_dimension` data structure.
}
\examples{
# Step: cross_section_dimensions
xs_dims <- cross_section_dimensions_L2(xs = fluvgeo::sin_riffle_channel_sf,
                                       xs_points = fluvgeo::sin_riffle_channel_points_sf,
                                       bankfull_elevation = 103,
                                       lead_n = 1,
                                       use_smoothing = TRUE,
                                       loess_span = 0.5,
                                       vert_units = "ft")

check_cross_section_dimensions(xs_dims, "cross_section_dimensions")

# Step: shear stress
xs_dims_ss <- shear_stress(xs_dims)

check_cross_section_dimensions(xs_dims_ss, "shear_stress")

# Step: stream_power
xs_dims_sp <- stream_power(xs_dims_ss,
                           discharge_method = "regional_curve",
                           region = "Illinois River",
                           drainage_area = 41)

check_cross_section_dimensions(xs_dims_sp, "stream_power")

}
