# Retain supporting terrain evidence

## Outcome

An analyst can preserve the exact file consulted when identifying a terrain
source, or a supplied processing record, alongside the project. Reports show what
was retained and what needs attention when a file disappears or changes. The
source-use account remains separate: retaining a document does not establish that
its described processing occurred. See the [contract](../schemas/retained-terrain-evidence.md).

The API is deterministic, local and shared by prospective QGIS/Shiny clients.
The schema-4 backend is not automatically deployed to either. Existing qualified
QGIS libraries, production ArcGIS workflows, FGDB and fluvgeodata are untouched.

## Verification

`test_study_terrain_evidence.R` covers exact binary copies, immutable originals,
same-byte reuse, schema selection, ordinary edits, nested manifest roots, folder
relocation, missing/changed evidence, malformed records, duplicate/overwrite
refusals, all three report views, escaping and partial report failure recovery.
Fixtures are synthetic and assert no real archive lineage.

Run `dev/scripts/study-terrain-evidence-example.R` with a NEW output folder to
produce the actual Define Study Area demonstration; no responsive-preview fixture.

On 2026-09-13, the final deterministic Study Area, terrain intake/reference,
survey-opportunity and report regression run passed **1,031 assertions**, including
58 for retention, with zero failures, test warnings or skips. An initial focused
run lacked gt in its library and skipped rendering; the final run used the existing
private dependency library and exercised all three report views without skips.
The testthat/R-version startup notice is separate from test warnings.

Roxygen regenerated the affected API help. Its existing legacy hanging-indent
diagnostics were not introduced here; unrelated regenerated help and package
documentation settings were preserved from a pre-generation snapshot. Raw logs
and generated artifacts are ignored under
`dev/outputs/terrain-development/terrain-evidence-v1/`.

The demonstration completed at `report/define-study-area.html`, preserving every
original fixture hash. Both evidence roles report MATCH while the source remains
a CANDIDATE. All content is explicitly synthetic, not recovered archive provenance.

The source package built, installed in its check directory, and the limited
`R CMD check --no-manual --no-vignettes --no-tests --no-examples` completed with
zero errors/warnings and the two existing NOTEs (methods declaration and unrelated
globals/imports). The 1,031 regression assertions ran separately; a full package
suite, examples, live services and client runtime qualification are not claimed.
The existing native-pipe/minimum-R build warning, unavailable suggested
fluvgeodata, offline indexes and Windows size-utility diagnostics remain in logs.
No unrelated legacy imports or minimum-R policy were changed by this increment.

Strict reproducibleai validation passed for fluvgeo and fg-qgis-toolbox, with only
expected repository-owned scaffold notices. Only the development plan changed in
the QGIS repository during this increment. No commit, archive migration, production
deployment or existing installed-library/profile upgrade was performed.

## Next boundary

The subsequent [thin QGIS form](../../../fg-qgis-toolbox/dev/features/retain-terrain-evidence.md)
is developer-qualified in a NEW compatible private runtime. Eleven actual provider
cases agree with direct R, including fresh changed/missing-copy reports. This adds
no backend science and upgrades no existing analyst profile. Do not add document
interpretation or auto-promote claims. Development 9018 subsequently adds
[ordered, attributed preparation accounts](terrain-processing-accounts.md), not
automatic document interpretation. Normalized source-product identities, executable
recipes and independently observed execution remain later work, not claims made
by attaching a processing log or recording an analyst's account.
