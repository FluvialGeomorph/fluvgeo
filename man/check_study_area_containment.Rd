\name{check_study_area_containment}
\alias{check_study_area_containment}
\title{Check child areas against a proposed Study Area boundary}
\usage{check_study_area_containment(boundary, streams = NULL, reaches = NULL)}
\arguments{
\item{boundary}{One valid CRS-defined XY Study Area polygon.}
\item{streams,reaches}{Optional saved child tables or polygon sf objects.}
}
\description{Read-only sf/GEOS outside-difference check in the parent's local
metric CRS, with a 1 mm overlay grid and 2 mm numerical precision margin.
No saved geometry is expanded or modified. Uses the regional processing domain
and geographic densification described in \code{preview_stream_corridor()}.
Named records without geometry are unknown.}
\value{Data frame with level, name and status: inside, outside or unknown.}
