\name{prepare_hydro_dem_view}
\alias{prepare_hydro_dem_view}
\title{Prepare a viewport for close inspection of a DEM}
\usage{prepare_hydro_dem_view(source, bounds, directory, pixels = 600000L, cache_directory = NULL, build_cache = TRUE, source_id = NULL)}
\arguments{
\item{source}{Path to an elevation GeoTIFF.}
\item{bounds}{Named west, south, east, north longitude/latitude bounds.}
\item{directory}{Existing job-owned output directory.}
\item{pixels}{Display cell budget, not an analytical processing limit.}
\item{cache_directory}{Persistent bounded display cache directory, or NULL for a job-local cache. Persistent caches reuse both source pyramids and recently prepared viewports without retaining raster values in R or browser memory.}
\item{build_cache}{Build missing full-raster display pyramids. If FALSE, reuse existing pyramids or prepare only the visible window without waiting for a cache.}
\item{source_id}{Optional immutable content identity supplied by a managed producer. It permits a cache prepared before an atomic file move to be reused from the published path.}}
\value{Display raster paths, local elevation range, source resolution and aggregation status. Empty views return empty=TRUE and a message.}
\description{Crops before display preparation, computes hillshade, and creates Web Mercator display rasters without changing analytical inputs.}
