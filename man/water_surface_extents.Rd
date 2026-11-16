\name{water_surface_extents}
\alias{water_surface_extents}
\title{Channel and flood-prone water surface extents}
\usage{water_surface_extents(rem, flowline, channel_elevation, floodplain_elevation)}
\arguments{
\item{rem}{A single-layer terra SpatRaster containing relative elevations.}
\item{flowline}{An sf flowline in the same coordinate reference system.}
\item{channel_elevation, floodplain_elevation}{Finite REM elevations in the raster's vertical units. The channel elevation must be strictly lower.}
}
\value{A list of sf polygons named \code{channel}, \code{floodplain}, \code{channel_outer}, and \code{floodplain_outer}. The first two preserve interior holes; the outer footprints fill holes without bridging disconnected components.}
\description{Extract unsimplified, flowline-connected inundation footprints at two analyst-selected elevations.}
\details{This uses the bathtub threshold and retains components intersecting the flowline. Footprints describe inundation, not measured flow paths or bank lines.}
