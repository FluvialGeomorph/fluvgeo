\name{burn_hydro_cutlines}
\alias{burn_hydro_cutlines}
\title{Lower DEM cutline zones to their minimum elevation}
\usage{burn_hydro_cutlines(source, cutlines, filename)}
\arguments{
\item{source}{Single-band projected elevation GeoTIFF path.}
\item{cutlines}{An sf object of polylines with a declared CRS.}
\item{filename}{New output GeoTIFF path; existing files are never overwritten.}}
\description{Uses terra touched-cell rasterization and per-zone minimum elevations, without widening. Lowest cutline order wins shared cells. Original grid, CRS, units and NoData mask are retained.}
\value{Output path, method and reference evidence, zone minima and software versions.}
