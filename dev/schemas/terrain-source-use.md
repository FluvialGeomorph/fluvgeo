# Terrain source-use evidence — development 9016

## Purpose and evidence boundary

Implement the first bounded source-to-derivative binding in the shared backend,
under the [scientific traceability roadmap](../../../FGDB/dev/goals/scientific-traceability-roadmap.md).
This records **what an analyst says supports an association**, not complete
reproducible lineage. It works for public catalogs and noncataloged archives.
No service, AI account, point cloud, conversion or enterprise connection is required.

Three things stay separate:

| Record | Meaning | Does not establish |
| --- | --- | --- |
| Catalog candidate | A possible source worth investigating | Actual historical use |
| Attributed source-use account | Analyst-supplied evidence/recollection connecting source and derivative | Exact source bytes, preparation execution, acceptance |
| Processing evidence (future) | Exact inputs, recipe, resources and outputs | Not implemented by this account editor |

Do not turn existing survey discovery dispositions, file declarations or project
analysis-reference choices into source-use records automatically. Differences
between source and analysis CRS remain legitimate until preparation is examined.
No acquisition date is inferred from a catalog record or claim timestamp.

## API and persistence

`record_study_terrain_source()` creates a new same-folder Study Area context,
adding or explicitly revising one `terrain_sources` row. Optional report output
reuses the definition, staging or terrain view. Publication and failure recovery
reuse the existing context editor: no replacement; retain prior snapshots; a
later report failure retains the new context for read-only reporting retry.

`write_study_context(..., terrain_sources=NULL)` and
`terrain_development_summary(..., terrain_sources=NULL)` append optional arguments.
Contexts with this table use **FLUVGEO_STUDY_CONTEXT_3**, whether or not they also
contain analysis choices. Without it, existing schema-1/2 selection is unchanged.
Readers check the catalog/version combination and round-trip every field. Older
backends reject schema 3 rather than discarding records. Installations and QGIS
profiles are not upgraded by this change. No existing manifest schema changes.

The exact nonspatial table has 13 plain character columns:

- `association_id`: unique caller-supplied local claim ID, not an FGDB UUID;
- `artifact_id`, `artifact_sha256`: exact inventoried GeoTIFF ID and saved SHA-256;
- `source_catalog`, `source_record_id`: namespaced source identity, case-sensitive;
  a documented local archive namespace is valid and is not a catalog-service enum;
- `source_snapshot`: reference/locator for the consulted metadata or archive record;
- `source_description`: human-readable source/product description;
- `source_version`: nullable source edition/version; NA explicitly remains unresolved;
- `status`: `CANDIDATE`, `RECORDED_USE`, or `REJECTED`;
- `basis`: `PROJECT_RECORD` or `OWNER_RECOLLECTION`;
- `evidence`, `analyst`: nonempty support/limitations and recorder attribution;
- `recorded_at`: exact UTC text, `YYYY-MM-DDTHH:MM:SSZ`, recording time only.

All fields except source_version are nonempty and nonmissing. Unknown fields,
factors, numeric replacements, bad timestamps and duplicate IDs/associations fail.
The editor trims surrounding whitespace using established scalar-text handling;
embedded newlines, quotes, backslashes and Unicode remain literal text.

Reusing a claim ID may revise its description, state and evidence/attribution;
it cannot retarget the artifact/fingerprint, source namespace/record, consulted
snapshot or source version. For a new edition/snapshot, retain or explicitly reject
the earlier claim and record a new ID. Exact duplicate target/source/snapshot/version
tuples require explicit revision, not a second ID. Multiple sources per derivative,
source reuse across derivatives and distinct catalog listings are retained; no
cross-listing reconciliation or independent-survey count is inferred.

## Integrity and partial legacy evidence

A Study Area and linked terrain manifest are required. The writer derives the
target hash from the already inventoried GeoTIFF record. Reading/writing checks
the claim against the manifest's exact artifact ID, format and saved hash; a newly
inventoried replacement cannot inherit it. The context also pins its linked manifest.
Existing editors carry the table forward, including when a manifest revision adds
unrelated assets or metadata without changing the original target fingerprint.

The target raster need not currently be available to preserve or record a forensic
account about its known fingerprint. Fresh missing/changed-file findings remain
visible and are never cleared by a source claim. Integrity and scientific assessment
are unchanged by adding a claim. Do not mutate the intake folder during operations;
this is not a concurrent multi-file transaction.

`source_snapshot` is **retained text**, not a fetched/pinned metadata file. Source
version text does not prove exact source bytes. Remote or archive references may
be unavailable; no hidden fetch occurs. Put known qualifications in evidence.
Callers must supply references/notes suitable for the report audience, without
credentials or restricted material. Report rendering escapes supplied text and
does not interpret references as executable code or automatic hyperlinks.

## Reporting and remaining scope

All three Study Area report views show source, derivative, state and next action;
identifiers, hash, consulted record, evidence and attribution remain collapsible.
The summary exposes the original typed table for later thin QGIS/Shiny interfaces.
No new readiness gate, catalog refresh, scientific score or separate preview report.

**Still proposed:** normalized reusable source-product editions/assets,
source-product byte hashes/recoverability,
cross-catalog reconciliation and processing recipes/verified execution. The current
table is an attributed evidence binding, not a substitute for those future records
or FGDB's enterprise model. New thin client entry can reuse this API after versioned
runtime qualification. Do not automatically populate legacy Cole Creek lineage.

Verification and reproduction: [feature record](../features/terrain-source-use.md).

Development 9017 adds separately [retained supporting files](retained-terrain-evidence.md)
with integrity checks in schema-4 contexts. This pins consulted metadata or supplied
processing records, not the underlying source collection or verified execution.

Development 9018 adds [ordered preparation accounts](terrain-processing-accounts.md)
against a source-use claim. These retain supplied descriptions and unknowns, not
normalized product identities or proof that the described processing occurred.
