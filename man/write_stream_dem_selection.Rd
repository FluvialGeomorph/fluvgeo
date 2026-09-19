\name{write_stream_dem_selection}
\alias{write_stream_dem_selection}
\title{Save explicit Stream DEM file choices}
\usage{write_stream_dem_selection(inventory, selected, dsn, context_revision)}
\arguments{\item{inventory}{Result of discover_stream_dem_files().}
\item{selected}{Character file IDs from that result; empty selection is allowed.}
\item{dsn}{New GeoPackage path; existing files are never replaced.}
\item{context_revision}{Opaque caller context revision label.}}
\value{Path invisibly. Records intent, not downloads or accepted terrain.}
\description{Save immutable file choices and their Stream, collection and query evidence.}
