\name{add_study_stream_corridor}
\alias{add_study_stream_corridor}
\title{Save one selected Stream corridor in a new study revision}
\usage{
add_study_stream_corridor(dsn, output_file, lines, stream_name,
  distance, unit = "m", add_note, stream_id = NULL,
  report_purpose = "definition")
}
\arguments{
\item{dsn}{Existing study context GeoPackage.}
\item{output_file}{New GeoPackage beside dsn; never overwritten.}
\item{lines}{Selected line sf with unique nonempty character source_id values.
All source fields and native geometry are retained as evidence.}
\item{stream_name}{Explicit name for a new Stream.}
\item{distance,unit}{See \code{preview_stream_corridor()}.}
\item{add_note}{Required selection rationale.}
\item{stream_id}{Optional exact existing names-only Stream ID. Its name is retained.
Several names-only Streams require all areas together; partial assignment is refused.}
\item{report_purpose}{Context revision purpose, normally "definition".}
}
\description{Adds one Stream, or supplies the area of one existing names-only
Stream by ID. Existing spatial Streams are not edited. The saved Study Area
clips the selected lines first, then their buffered area is clipped again.
Original source lines remain in the selected_lines evidence layer; clipped_lines
retains the processed geometry and source IDs. Original/retained lengths,
buffer parameters, processing CRS/method and clipping details are recorded in
the companion GeoPackage and hash-linked context notes.}
\value{List with context, report (NULL), evidence and stream_id. Evidence is
written first and hash-linked in notes; failure can leave unreferenced evidence
but never replaces earlier context. No network acceptance, Reach creation,
topology repair or enterprise loading occurs.}
