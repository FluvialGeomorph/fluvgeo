\name{read_stream_dem_selection}
\alias{read_stream_dem_selection}
\title{Read saved Stream DEM file choices without network access}
\usage{read_stream_dem_selection(dsn)}
\arguments{\item{dsn}{Existing selection GeoPackage.}}
\value{Inventory plus selected IDs and context_revision. Does not validate current study state.}
\description{Read a versioned file-choice snapshot without fetching raster data.}
