\name{prepare_hydro_dem_display_cache}
\alias{prepare_hydro_dem_display_cache}
\title{Precompute a Hydro DEM display cache}
\usage{prepare_hydro_dem_display_cache(source, cache_directory, source_id = NULL)}
\arguments{
\item{source}{Path to an elevation GeoTIFF.}
\item{cache_directory}{Persistent display cache directory.}
\item{source_id}{Optional immutable content identity retained across an atomic move.}}
\value{Paths to the prepared elevation and hillshade cache rasters.}
\description{Builds full-resolution elevation and hillshade Cloud Optimized GeoTIFFs with internal overviews and statistics. This is intended for a producer worker; interactive maps should call \code{prepare_hydro_dem_view()} with \code{build_cache=FALSE}.}
