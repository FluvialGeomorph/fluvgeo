\name{read_study_stream_segments}
\alias{read_study_stream_segments}
\alias{preview_study_reach_corridor}
\alias{add_study_reach_corridor}
\title{Define Reaches from retained Stream segments}
\usage{
read_study_stream_segments(dsn, stream_id)
preview_study_reach_corridor(dsn, stream_id, source_id)
add_study_reach_corridor(dsn, output_file, stream_id, source_id,
  reach_name, report_purpose = "definition")
}
\arguments{
\item{dsn}{Existing study context GeoPackage.}
\item{stream_id}{Exact saved spatial Stream identity.}
\item{source_id}{One or more selection_id values from the reader: COMIDs before
piece editing, distinct local piece IDs after a split. The argument name is
retained for compatibility.}
\item{output_file}{New context GeoPackage beside dsn; never overwritten.}
\item{reach_name}{Explicit unique Reach name within the parent Stream.}
\item{report_purpose}{Revision purpose: definition, terrain or staging.}
}
\description{Reads the hash-linked Stream segment evidence, previews or saves
one Reach from selected segments with inherited buffer distance, units, processing CRS and algorithm.
No live discovery, arbitrary segment splitting or independent width choice.}
\details{Missing, changed, ambiguous or unsupported Stream evidence fails closed.
Reach buffers clip to the parent Stream and pass CRS-aware containment checks.
All selected segments share one Reach identity. Disconnected parts remain;
no connecting geometry is invented. Existing saved Reach identities are not
merged or replaced. Single-segment calls remain compatible.
Adjacent buffers may overlap; they are not non-overlapping partitions. Duplicate
piece assignments within one Stream and duplicate names are refused. Saving
recomputes geometry, retains segment and area evidence, and preserves prior
records. Evidence is written first; failure may leave an unreferenced evidence
file. Only Stream evidence produced by the supported clip-first GEOS method is
accepted. Read-only browsing does not certify scientific Reach suitability.}
\value{The reader returns parent, lines, distance, unit, distance_m, method,
evidence, sha256, assigned_source_ids, reach_mappings and mapping_notes. Mappings
link current Reach identities to source IDs and their latest evidence notes.
After splitting, piece_state is TRUE and pieces and piece_evidence are supplied.
lines retains source_id as the original COMID and selection_id as the local piece
key. assigned_selection_ids and reach_mappings$selection_id are assignment keys;
assigned_source_ids remains the distinct assigned source COMIDs, not piece keys.
The authoritative assignments then come from hash-linked piece evidence rather
than historical note mappings. Reads never migrate or rewrite old evidence.
The ordering table describes downstream-to-upstream order using original
reference linework before clipping, with unresolved ordering explicitly flagged.
Preview returns the corridor preview
plus source and source_id. Saving returns revision paths, evidence and reach_id.}
