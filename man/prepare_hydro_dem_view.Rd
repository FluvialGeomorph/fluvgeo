\name{prepare_hydro_dem_view}
\alias{prepare_hydro_dem_view}
\title{Prepare a viewport for close inspection of a DEM}
\usage{prepare_hydro_dem_view(source, bounds, directory, pixels = 600000L, cache_directory = NULL, build_cache = TRUE)}
\arguments{
\item{source}{Path to an elevation GeoTIFF.}
\item{bounds}{Named west, south, east, north longitude/latitude bounds.}
\item{directory}{Existing job-owned output directory.}
\item{pixels}{Display cell budget, not an analytical processing limit.}
\item{cache_directory}{Persistent display cache directory, or NULL for a job-local cache.}
\item{build_cache}{Build missing full-raster display pyramids. If FALSE, reuse existing pyramids or prepare only the visible window without waiting for a cache.}}
\value{Display raster paths, local elevation range, source resolution and aggregation status. Empty views return empty=TRUE and a message.}
\description{Crops before display preparation, computes hillshade, and creates Web Mercator display rasters without changing analytical inputs.}
