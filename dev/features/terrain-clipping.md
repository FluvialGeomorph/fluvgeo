# Clip terrain to an explicit AOI

**Owner direction, 2026-09-14:** retain this as an experimental implementation,
not an approved toolbox/API boundary or mandatory workflow step. Its QGIS
exposure is on hold for the [design charrette](../../../FG-architecture/dev/features/toolbox-redesign-charrette.md).
Technical verification below does not establish acceptance of its placement in
the redesigned workflow. No code or outputs are removed by this clarification.

## Outcome

The first execution slice after the account-entry trial actually crops/masks a
DEM and automatically records the operation. The analyst selects the AOI and
boundary-cell rule, reviews the output, and does not retype steps into a CSV.
The short visual report states what ran, what stayed unchanged and what to do next.

The [operation and receipt contract](../schemas/terrain-clipping-run.md) defines
the limited input profile, exact-value/metadata checks, source preservation,
failure records and separation from historical accounts. This uses established
terra methods rather than selecting a new conditioning, conversion or extraction
algorithm. Unknown vertical metadata is not inferred; known compound/3D references
are refused pending preservation qualification, not silently dropped.

## Verification

Synthetic tests cover center/touched cells, independent center-point/source-cell
lookup, holes, negative/fractional elevations, source NoData, all-NoData outputs,
scale/offset decoding, undeclared units, multiple polygons, unchanged source
fingerprints, out-of-extent/different-CRS/auxiliary-file refusal, compound-CRS
refusal, failed verification receipts, report-failure recovery, safe relocation,
changed-output rejection and escaped report text. No percentages or AI services
are introduced. Existing source, Study Area and account records are not rewritten.

The example script takes an explicitly selected GeoTIFF and a new output root.
Its interior polygon is **synthetic**, not a recovered or approved Cole Creek
Reach boundary. The example processes an existing Cole Creek 2006 development
export, not the archived FileGDB. This demonstrates actual processing but does not
validate an analyst's chosen AOI or original terrain lineage.

On 2026-09-13, the selected terrain/report regression set passed **265 assertions**
with zero failures, test warnings or skips. After adding the final retained-AOI
change guard, all **59 clipping assertions** passed again. These counts overlap;
they are not 324 distinct tests. Evidence is in
`dev/outputs/terrain-development/terrain-clip-v1/`. Use the workspace-local R
cache for sandboxed rendering; the first broad run emitted styling-cache access
warnings before that environment setting was supplied.

The Cole Creek demonstration completed with terra 1.9.46 and fluvgeo development
9019: source 1,934 x 497 cells, output 1,162 x 299 on the same grid, with the
original export's SHA-256 unchanged. The short report renders and its HTML/content
checks pass. Browser visual inspection was unavailable because the in-app webview
did not attach; no visual usability or analyst acceptance is claimed. The original
export, archives, existing contexts and installed runtime remain unchanged.

Source build and limited `R CMD check --no-manual --no-vignettes --no-tests
--no-examples` completed with zero errors/warnings and two existing NOTEs
(methods declaration and unrelated globals/imports). Focused tests ran separately
above. The full suite was not run: existing Level 1–3 report tests delete named
files under HOME and include external-service work. No changes to those tests,
dependency policy or production clients are part of this increment. Existing
native-pipe/minimum-R, unavailable package-index/Suggests and Windows size-utility
diagnostics remain in the build/check evidence. Only the two new API help files
were generated; prior documentation was not rewritten.
Strict reproducibleai context validation passes in fluvgeo and fg-qgis-toolbox,
with only the existing repository-owned scaffold notices. Git whitespace checks
pass; no commit or runtime promotion was performed.

## Next boundary

First let the owner decide the capability's role in the redesigned workflow;
do not implement its proposed QGIS form yet. If later approved, compare actual-provider
outputs against direct R before asking the analyst to try it. No installed QGIS
profile or production R library is upgraded by this backend increment. Existing
ArcGIS, Shiny, RegionalCurve, fluvgeodata and FGDB interfaces remain unchanged.
Event selection/replacement and linking the receipt into the evolving Study Area
view remain explicit subsequent integration, not hidden side effects of clipping.
