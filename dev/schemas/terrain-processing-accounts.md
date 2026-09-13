# Terrain preparation accounts — development 9018

## Implemented scope

`record_study_terrain_processing()` records one analyst-supplied, ordered account
against an existing terrain source-use association in a NEW same-folder context.
The association pins the inventoried derivative; it does not establish exact input
products, intermediate bytes, or that the described steps produced that derivative.
Source status remains CANDIDATE, RECORDED_USE or REJECTED independently.

This is the bounded archive-recovery step after retaining supporting documents.
It is **not** an executable recipe, verified processing run, normalized product
graph, or additional FGDB hierarchy. It is suitable for partial reconstruction;
unknown inputs, parameters, software versions and execution dates remain unknown.
An account cannot certify CRS transformation, units or temporal comparability.
Future deterministic execution tooling must record actual inputs and outputs
separately; it must not reinterpret these accounts as machine-observed history.
Manual recovery is optional, not mandatory duplicate bookkeeping for new FG
execution tools. No operation-name vocabulary, dispatch registry or processing
dependency schedule is implemented by this table. A report's recovery suggestions
are about historical evidence; they are not instructions to run named operations
or complete every optional cell before continuing study configuration.

## API and persisted contract

The API accepts seven plain-character step columns, in supplied order:
`operation`, `input_description`, `output_description`, `parameters`, `software`,
`software_version`, `execution_time`. Only operation is required per step. Missing
optional details must be `NA_character_`, not empty strings or invented defaults.
Descriptions can name multiple inputs/outputs but are not resolved references.
Parameters are literal text, not parsed code or a typed parameter schema. Reported
execution time preserves supplied precision as text; unknown dates are not padded.

The optional nonspatial `terrain_processing` context table has exactly 16 fields:

- `processing_id`: local append-only account ID, repeated across its steps;
- `association_id`: FK to terrain_sources, constant within the account;
- `step_number`: plain integer, unique contiguous 1..N within each account;
- the seven step columns above;
- `basis`: PROJECT_RECORD or OWNER_RECOLLECTION;
- `evidence_id`: nullable FK to a retained PROCESSING_RECORD belonging to the
  same association; a metadata snapshot cannot masquerade as a processing record;
- `qualifications`, `analyst`: nonempty support/limits/correction rationale and
  attribution; neither constitutes approval;
- `recorded_at`: exact valid UTC YYYY-MM-DDTHH:MM:SSZ, time of recording only.

Every field except step_number is plain character. All account-header fields
must agree across its steps. Empty tables, unsupported columns/types, missing
operations, conflicting headers, duplicate/gapped step numbers, invalid timestamps,
absent source claims and mismatched evidence links fail before publication.
API row order supplies declared sequence; readers/reporting use step_number.
Each account has one source-use association. Joint multi-source product lineage
and computational dependency graphs remain future work, not inferred from text.

Corrected accounts use new IDs and an explicit rationale. Earlier snapshots are
the audit trail; no automatic supersession, preferred account or execution status
is selected. Low-level writer round trips validate shape and links, not the truth
or completeness of the analyst's interpretation.

## Compatibility and failures

Presence selects `FLUVGEO_STUDY_CONTEXT_5`. Schemas 1–4 remain readable and their
writer selection is unchanged. Writer and summary append optional
`terrain_processing=NULL`; ordinary context editors preserve supplied records.
Older installed readers refuse schema 5 rather than dropping the table. A new
compatible isolated runtime is required before QGIS exposure; no existing runtime,
production ArcGIS client, Shiny deployment, manifest, network binding or FGDB
schema changes in this increment.

No original input, GeoTIFF or retained attachment is edited. Existing new-file,
same-folder publication and pinned context/manifest checks apply. Report failure
leaves the new context available for read-only retry. Move the complete folder.
Missing/changed supporting bytes remain fresh findings without erasing accounts.
An account never promotes source use or changes scientific assessment/readiness.

## Reporting

All three existing report views show declared operations, inputs/outputs and
what to recover next. Unknown optional details are explicit. Parameters, software,
reported execution dates and attribution use shared compact gt tables under an
expandable section. Retained-document integrity and rejected source associations
remain visible. Contents are escaped text, never evaluated, fetched or embedded
as executable document content. Fully populated fields still do not prove execution.
