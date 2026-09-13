# Retained terrain evidence — development 9017

## Implemented boundary

`retain_study_terrain_evidence()` copies one explicitly selected local supporting
file and appends an attributed record to a NEW same-folder Study Area context.
It supports `METADATA_SNAPSHOT` and `PROCESSING_RECORD`, linked to an existing
source-use account. It does not fetch catalogs, parse documents, execute scripts,
promote a candidate, verify historic execution, or infer source/analysis CRS.
This is preservation of evidence, not a normalized source-product or recipe model.

The original stays untouched. The copy lives under the **linked terrain
manifest's directory**, at `terrain-evidence/<sha256>.<extension>` (lowercase
alphanumeric extension up to ten characters; otherwise `bin`). Its original name
is retained separately. Identical bytes can support different claims or roles;
an existing copy is reused only if its bytes match. This reuses the folder-based
storage decision and existing safe-path/SHA-256/new-file publication mechanisms.
No artifact is added to the GeoTIFF/vector inventory merely because it is evidence.

## Persisted interface

`terrain_evidence` is an optional table appended to the writer and summary APIs.
Its exact eleven plain, nonempty character fields are:

- `evidence_id`: unique local ID, append-only;
- `association_id`: foreign key to `terrain_sources.association_id`;
- `kind`: one of the two roles above;
- `path`, `sha256`: constrained manifest-root-relative path and exact byte hash;
- `original_name`, `description`: supplied file name and readable description;
- `source_reference`: original locator/reference, with known edition/date;
- `qualifications`, `analyst`: relevance/limitations and recorder attribution;
- `retained_at`: UTC `YYYY-MM-DDTHH:MM:SSZ`, retention time, not acquisition time.

Exact claim/kind/hash duplicates and reused evidence IDs are refused. Corrected
file content uses a new evidence ID; earlier context snapshots remain the audit
trail. Supplying a file cannot rewrite the source account's immutable identity.
Existing context editors carry the table forward without interpreting its contents.

Contexts containing this table use `FLUVGEO_STUDY_CONTEXT_4`. Schemas 1–3 remain
readable and retain their existing writer selection. Earlier installed backends
refuse schema 4 rather than silently dropping evidence. No client runtime is
upgraded automatically; a compatible isolated runtime must precede QGIS exposure.

## Fresh reporting and failure behavior

The summary exposes original records in `terrain_evidence` and a fresh inspection
in `retained_evidence`, adding `integrity`: MATCH, MISSING, CHANGED or UNREADABLE.
All three existing report views display readable descriptions, current integrity
and next actions; paths, hashes, references and qualifications stay collapsible.
No document contents are embedded, opened, linked automatically or executed.
Matching bytes establish retention integrity only, not scientific truth.

Missing/changed/unreadable copies remain reviewable findings: the context and
claim are preserved, not rejected or silently repaired. Unsafe paths and malformed
tables remain structural errors. Retained-evidence findings are separate from
scientific assessment/readiness; no new compliance gate is imposed.

Move the complete folder. Paths resolve relative to the manifest, including when
it is below the context directory. Fingerprints detect changes, not authenticate
authors or make a jointly edited context/file tamper-proof. Do not modify inputs
concurrently. Publication is not a multi-file transaction: a late failure may
leave an unreferenced retained copy; a retry can reuse matching bytes. A later
report failure keeps the saved context for read-only reporting retry. No cleanup
deletes a published file, replaces an earlier copy or modifies the archive.

The caller must select only material appropriate for project access controls and
the audience. No redaction, malware scanning or classification is performed. This
API is for selected supporting records, not bulk lidar/source-product retention.
Development 9018 adds optional [ordered preparation accounts](terrain-processing-accounts.md)
linked to these records. Full source-product inventory, executable recipes and
independently verified execution remain future work. No Cole Creek provenance is inferred.
