# Versioned Reach piece evidence

Implemented by fluvgeo 2026.09.17.9032 for saved-Reach splitting. The source
Stream remains unchanged. A context note links the latest state for its Stream:
`Stream pieces [stream UUID]: stream-pieces-<UUID>.gpkg; SHA256 <hash>.`
The latest explicit link is authoritative. Missing/changed linked evidence fails
closed; readers never scan orphan files or migrate a context while opening it.

## Layers

| Layer | Contract |
| --- | --- |
| pieces | Active metric line pieces for the whole Stream, including unassigned pieces |
| reach_areas | Current polygons/identities for every Reach in this Stream; checked against context geometry |
| cut | On split revisions: snapped POINT, clicked metric x/y, snap distance, cut fraction, retained side, old/new Reach IDs and method |
| cut_parent | Exact retired/refined parent line piece, retaining identity/source interval for lineage, including the first split |

`pieces` fields: piece_id (unique local UUID), source_id (original COMID,
repeatable across pieces), parent_piece_id (NA for original unsplit roots),
source_from/source_to (increasing normalized interval on the exact directed
retained metric source line), source_hash (original Stream evidence SHA256),
fg_reach_id (current assignment or NA), geometry (MULTILINESTRING in inherited
metric CRS). Intervals partition each retained source from 0 to 1, without gaps
or overlaps. Original geographic source and clipped root geometry remain in the
unchanged Stream evidence, identified jointly by checksum and COMID.

These are not intervals on a merged/simplified line or another NHDPlus edition,
and are not FG station values. Cut parent geometry and prior evidence/context
links retain intermediate pieces across repeated edits. No new COMID is invented.
The note records operation and prior evidence filename/hash. Renaming changes
only the context name, so historical labels in evidence remain historical;
identity and geometry, not names, govern agreement checks.

## Reader/API seam

`read_study_stream_segments` keeps source_id as COMID and adds selection_id:
legacy COMID before piece editing, piece_id afterward. Consumers must select
using selection_id and assigned_selection_ids, not deduplicate by source_id.
reach_mappings includes both source_id and selection_id. The existing creation
API's source_id argument name is retained, but its values must be reader-supplied
selection IDs. `assigned_source_ids` remains the distinct assigned COMIDs for
compatibility, not an available-piece inventory. piece_state/pieces/piece_evidence
identify the explicit representation. Old whole-segment calls remain compatible
for untouched contexts. Piece-enabled editing requires fluvgeo >=9032.

On first split, all existing Reach geometry/evidence in the Stream is checked
before converting whole-segment assignments. Add/Combine/Split publish new piece
evidence plus a new context revision; original files are never overwritten. This
is not a multi-file transaction: a later failure may leave unreferenced evidence.
Linked network/terrain records and target Survey Events block splits until an
explicit reconciliation workflow exists. No enterprise schema migration occurs.
