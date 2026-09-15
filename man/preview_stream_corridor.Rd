\name{preview_stream_corridor}
\alias{preview_stream_corridor}
\title{Preview an explicitly selected Stream corridor}
\usage{preview_stream_corridor(lines, distance, unit = "m", boundary = NULL)}
\arguments{
\item{lines}{sf with 1 to 500 valid nonempty finite XY line features and a CRS.}
\item{distance}{Positive buffer distance on EACH side of the lines, at most 10,000 m.}
\item{unit}{"m" or international "ft" (exactly 0.3048 m).}
\item{boundary}{Optional regional Study Area polygon. Clip selected lines first,
buffer their retained portions, then clip the area. NULL returns an unrestricted buffer.}
}
\description{Boundary-aware processing uses sf/GEOS in a local WGS84 azimuthal
equidistant CRS in metres. Geographic edges are densified to at most 100 m before
projection, using sf/lwgeom. A 1 mm overlay grid and 2 mm line-clipping/containment
precision margin handle shared edges; the final area clips to the unexpanded
parent. Line remnants of at most 4 mm are discarded. These numerical settings
are not source-accuracy claims. Supports regional parents at most 6 degrees
wide/high between 80 S and 80 N; not dateline-crossing areas. Without a boundary,
uses an sf/S2 spherical outer buffer (10,000 cells). Neither method delineates
a floodplain, repairs a network, or chooses the terrain analysis CRS.}
\value{List with area (one CRS-defined MULTIPOLYGON), distance_m, selected_features,
polygon_parts, method, unclipped_area, clipped and removed_area_m2. Boundary
previews also return clipped_lines (source attributes and projected retained
geometry), source_rows (indices into the input), line_lengths (source_row,
original_m, retained_m, including zero-length discarded rows), retained_features,
line_clipped and processing_crs (WKT). Boundary output retains its metric CRS;
unrestricted output is WGS84. No files are written.}
