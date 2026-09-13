# Record terrain source-use evidence

## Outcome

The shared backend now saves an analyst's source-use account against the exact
fingerprint of an inventoried terrain product. It keeps candidates, recorded use
and rejections distinct, supports multiple sources and preserves prior context
snapshots. All three Study Area report views share the new evidence/next-action
section with compact gt tables. See the [contract and limits](../schemas/terrain-source-use.md).

This is the next step after analysis-reference choices: recording the evidence
connecting a source and an FG terrain product, without asserting how the product
was prepared. Source catalog matching alone is not provenance. The new API makes
no web/AI calls and requires no public catalog coverage.

## Verification

Deterministic tests in `test_study_terrain_sources.R` cover schema-1/2 to schema-3
roundtrips, unchanged hierarchy/choices/assessment, shared terrain/event links,
multiple sources and reuse, explicit revisions/rejections, duplicate and retarget
refusals, strict column/timestamp handling, missing terrain, replacement fingerprints,
folder relocation, other editors, all three report views, escaped input and partial
publication recovery. The claims and terrain in this test are **synthetic**.
They establish software behavior, not recovered source lineage for Cole Creek.

Run `dev/scripts/study-terrain-source-example.R` from the workspace root with a
new output directory. It produces one clearly synthetic demonstration report and
preserves its earlier context snapshots. No responsive-preview fixture is created.

On 2026-09-13, the broader Study Area, terrain-manifest, reference-review,
survey-opportunity and report regression run passed 914 assertions with no failures
or skips. Three table-test cache-permission warnings were environmental; rerouting
the cache to the workspace resolved them. The final focused rerun passed all 92
source-use assertions (including subsequent shared-event/manifest-editor checks)
and 37 table assertions, without test warnings or skips. The startup notice that
testthat was built under R 4.6.1 is separate from these test results on R 4.6.0.
The demonstration completed with its original file hashes unchanged; its report
and final test log are under ignored `dev/outputs/terrain-development/terrain-source-use-v1/`.

Roxygen generated the new/changed help. Its installed 8.1.0 version also rewrote
unrelated legacy imports/help and reported existing hanging-indent problems;
those unrelated rewrites were excluded. Existing namespace imports and package
documentation settings were preserved, retaining only the new API export/help.

The source package built and its limited `R CMD check --no-manual --no-vignettes
--no-tests --no-examples` finished with zero errors/warnings and the two existing
NOTEs (`methods` declaration and unrelated globals/imports). No new API/documentation
mismatches were found. The focused regression suites above ran separately; a full
package/live-service suite, examples and client runtime qualification are not
claimed. Unavailable suggested fluvgeodata in the check library, offline indexes,
the Windows size utility and the existing minimum-R/native-pipe build warning are
recorded in the logs, not resolved by this increment. Check output is in the
example folder's `fluvgeo.Rcheck/00check.log`.

Strict reproducibleai validation passed for fluvgeo and fg-qgis-toolbox. Only the
QGIS development plan changed in that client repository during this increment;
its code, plugin and prior private library remained unchanged. No commit or
deployment was performed.

## Compatibility and next step

fluvgeo development 2026.09.13.9016 reads schemas 1, 2 and 3; older backends reject
new source-use contexts. The subsequent
[thin QGIS form](../../../fg-qgis-toolbox/dev/features/record-terrain-source.md)
is now developer-qualified against this version in a NEW isolated private runtime:
nine actual provider cases and direct-R context/report comparisons pass. This
adds no backend science and establishes no real archive lineage or analyst
usability endorsement. Existing analyst libraries/profiles, Shiny, ArcGIS
production and FGDB remain unchanged. Development 9017 now separately
[retains supporting metadata and processing records](retained-terrain-evidence.md)
with byte-integrity review. Exact source-product assets and verified processing
provenance remain future increments, not inferred data.
