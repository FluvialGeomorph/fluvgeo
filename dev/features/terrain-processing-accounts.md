# Terrain preparation accounts

## User outcome

An analyst can now retain a partial account of how source terrain was prepared
for FG analysis, then see both the supplied steps and what is still unknown in
Define Study Area, Staging and Terrain Development reports. The report uses the
existing compact gt/R Markdown presentation; no preview fixture is introduced.

`record_study_terrain_processing()` belongs in the shared fluvgeo backend so
desktop and Shiny clients can use the same validation, persistence and reporting.
It records an ordered account linked to one source-use claim, optionally citing
one of that claim's retained processing documents. The
[exact schema and evidence boundary](../schemas/terrain-processing-accounts.md)
distinguish this from executed transforms and normalized product lineage.

This supplies a practical archive-recovery record, not an obligation to invent
details that were never retained. Input/output descriptions and parameters can
describe the historical preparation workflow while unknown software versions or
dates remain missing. Account recording never transforms terrain, promotes a
candidate, validates a scientific method or accepts cross-event comparability.

## Demonstration and verification

The reproducible `dev/scripts/study-terrain-processing-example.R` creates a new,
clearly synthetic fixture with a candidate source, retained note and two-step
account. It checks that existing files remain unchanged. Its described preparation
operations are NOT run; this is not recovered Cole Creek provenance.

Deterministic tests cover new-file persistence, integer step order, exact literal
text and unknowns, immutable prior snapshots, preservation through ordinary
editors, account corrections under new IDs, folder relocation, evidence role and
association constraints, malformed tables, schema/catalog disagreement, fresh
changed/missing attachment findings, escaped report text and report-failure recovery.

On 2026-09-13, the deterministic Study Area/terrain/report regression set passed
**1,118 assertions**, including **87** for this capability, with zero test failures,
warnings or skips. The separately generated example also passed its file-identity,
source-status, ordering and retained-integrity checks. The testthat built-under-R
startup notice is environmental, not a test warning. Strict reproducibleai context
validation passed with only the expected repository-owned scaffold notices.

Generated API documentation was refreshed; unrelated existing help files were
restored from a pre-generation snapshot and their hashes checked. Existing dirty
work was preserved. The source package built and the limited package check
(`--no-manual --no-vignettes --no-tests --no-examples`) completed with zero errors
or warnings and two existing NOTEs: undeclared methods use and unrelated
globals/imports. The 1,118 regression assertions ran separately; a full package
suite, examples, live-service tests or client qualification are not claimed.
Existing native-pipe/minimum-R build warnings, offline repository indexes, an
unavailable suggested fluvgeodata installation and Windows size-utility diagnostics
remain in the local logs; this increment does not change their policies.

The example report is under
`dev/outputs/terrain-development/terrain-processing-v1/example/report/define-study-area.html`.
Build, check and regression evidence are beside that example directory and remain
development outputs, not customer deliverables. No commit, archive migration or
existing client-library/profile upgrade was performed.

## Next boundary and downstream impact

The [thin QGIS interface](../../../fg-qgis-toolbox/dev/features/record-terrain-preparation.md)
is now developer-qualified in a NEW schema-5-compatible private runtime: eleven
actual provider cases agree with direct R. The returned analyst trial verifies
account persistence and unchanged prior context/assessment. CSV editing and the
form layout are acceptable, but their purpose and vocabulary were unclear.
The client now documents this as optional past-work recovery with free-text
operations, not a required processing checklist. No repeat trial is requested.
The backend implementation and existing reports are unchanged. Existing schema-4 QGIS profiles are
not upgraded and will refuse these new contexts; their previous files remain
usable. No production ArcGIS, Shiny, RegionalCurve or fluvgeodata deployment is
changed. The shared backend adds no new dependency, web request or AI runtime.

Normalized source-product identities, machine-readable multi-source dependency
graphs, typed executable recipes and independently observed execution remain
separate work. The first bounded execution tool is now
[explicit AOI terrain clipping](terrain-clipping.md): it automatically records
its actual input/AOI/output fingerprints, parameters, software and outcome.
Analysts select methods and review results, not retype observed execution into a
retrospective account. Its function-specific receipt is separate from these
accounts; it does not implement a general recipe or dependency graph. Do not
promote these attributed accounts into those future models
automatically. Keep prior context snapshots when interpreting conflicting accounts.
