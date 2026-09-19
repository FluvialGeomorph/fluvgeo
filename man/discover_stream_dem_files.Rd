\name{discover_stream_dem_files}
\alias{discover_stream_dem_files}
\title{Find reported source DEM files for a Stream and Survey Collection}
\usage{discover_stream_dem_files(stream, collection, max_records = 200L)}
\arguments{
\item{stream}{One valid polygon sf with stream_id. This is the acquisition AOI.}
\item{collection}{One reviewed Survey Collection sf record.}
\item{max_records}{Maximum catalog records fetched (1 to 500).}}
\value{List with stream, collection, files (sf bounding boxes), outcome,
message, retrieved_at and endpoint. File bounds are not valid-data footprints.
Only supported USGS source-directory links are resolved; no raster download,
CRS assignment, mosaicking, suitability acceptance or Event creation occurs.}
\description{Resolve supported USGS source DEM catalog entries by directory identity and Stream intersection, without downloading rasters.}
