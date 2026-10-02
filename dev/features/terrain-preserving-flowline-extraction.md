# Terrain-preserving synthetic stream extraction

Status: implemented and accepted for the local FG Studio workflow, 2026-10-02

## Purpose

FG terrain development needs an efficient first-cut synthetic stream derivation
from a high-resolution, hydro-modified Stream DEM. The historical product at this
step is a vector feature class named `stream_network`. It is a candidate network
for analyst review, not an accepted network Observation or a replacement for the
terrain from which it was derived.

The governing objective is to preserve existing-condition geomorphic signal while
obtaining a useful drainage path. Pools, riffles, bars, scour, secondary channels,
floodplain depressions, submerged terrain and other non-monotonic variation can
be meaningful at the working resolution. Preservation is an optimization goal,
not an absolute prohibition on filling or breaching.

## Ownership and scope

`fluvgeo` owns reusable routing, accumulation, vector derivation, scientific
validation and processing evidence. A client such as FG Studio owns selection of
an exact saved terrain edition, background-job state, interactive preview and
local publication. FGDB ownership and delivery of a governed Stream Network
Configuration or Observation remain separate.

The implemented local method uses compact Priority-Flood conditioning, compiled
strict-downslope D8 assignment, Barnes-style flat resolution, compact upstream-
cell accumulation, a hectare threshold and lossless topological consolidation.
`locate_stream_outlet()`, `extract_synthetic_stream_network()` and
`threshold_synthetic_stream_network()` are the reusable public API. This does
not authorize a governed FGDB delivery binding or establish validation across
all terrain forms.

## Terrain roles

Keep the following roles distinct:

1. **Prepared Stream DEM:** the retained measurement terrain assembled for the
   Stream and Survey Event. It remains unchanged by extraction.
2. **Hydro-modified Stream DEM:** an immutable derivative produced from explicit
   analyst cutlines through identified fine-scale flow blockages such as culverts.
   This is the normal extraction input when an applicable saved result exists.
3. **Routing representation:** a temporary or explicitly retained derivative used
   by the selected routing toolchain. It may receive the minimum practical
   automated filling or breaching needed to obtain a useful result. It is not
   measurement terrain and must not silently replace either DEM above.

An analyst's cutlines are the preferred intentional treatment of known artificial
barriers. Automated conditioning can address residual routing requirements, but
its method, parameters, affected cells and elevation changes must remain visible.
Ambiguous or fragmented output can be returned for review instead of triggering
unbounded conditioning.

## Verified repository evidence

### Legacy workflow

The ArcGIS/TauDEM workflow in `FluvialGeomorph-toolbox/tools/` provides historical
behavior, not an automatically accepted replacement specification:

- `_02_HydroDEM.py` rasterizes analyst cutlines, optionally expands them and
  assigns each zone its minimum sampled elevation.
- `_03_ContributingArea.py` removes pits, calculates D-infinity direction and
  produces TauDEM specific contributing area (`sca`).
- `_03a_ContributingAreaD8.py` fills sinks, then calculates D8 direction and
  cell-count accumulation for the watershed path.
- `_04_StreamNetwork.py` thresholds the D-infinity contributing-area raster,
  thins it and converts it to the `stream_network` polyline feature class.

The legacy paths use different accumulation meanings. A numeric threshold from
one path is therefore not portable to the other without an explicit conversion
and scientific rationale. Horizontal raster units, cell area, contributing-area
units and vertical elevation units must remain distinct.

### Current shared backend

`burn_hydro_cutlines()` now implements the owner-authorized comparable cutline
operation: touched cells, first-drawn priority on shared cells, each zone's minimum
valid elevation, no widening and preservation of the source NoData mask. It
records source/output hashes, grid and coordinate evidence, zone minima and
producer versions. This resolves the current cutline contract; it does not claim
ArcGIS pixel-for-pixel equivalence or retain the legacy widening option.

`hydroflatten_dem()` is a separate historical analysis helper that raises a DEM
to a trend-based water surface. Current workspace callers are tests. It is not
the Hydro Modify cutline operation and is not an extraction-conditioning method
under this design. Preserve its existing compatibility until a separate review
establishes any change.

`orient_lines_from_dem()` orients existing lines from endpoint elevations. It
does not derive lines, establish continuous downhill profiles or resolve a
routing method.

The installed terra 1.9.46 exposes `flowDir()` with LTD and LAD path-based,
nondispersive direction options and `flowAccumulation()` in upstream cell counts
or supplied weights. Its own documentation marks `flowDir()` experimental and
requiring result verification. D8-LTD is therefore an initial benchmark candidate,
not an accepted final method. Terra's experimental `pitfiller()` is likewise
evidence of an available candidate, not a selected conditioning policy.

## Proposed extraction sequence

The first implementation should preserve the historical product while replacing
the legacy black-box sequence with explicit stages:

1. Select and fingerprint an exact prepared or hydro-modified Stream DEM edition.
   Prefer an explicitly selected applicable Hydro DEM; never infer that the newest
   file is the analyst's intended input.
2. Create a job-owned routing representation. First test routing with no additional
   modification. Apply a bounded fill, breach or combined strategy only when the
   selected toolchain requires it or the unconditioned result is unusable.
3. Calculate direction and accumulation with explicit method and units.
4. Apply a documented default stream-initiation threshold and allow compact
   analyst adjustment. The Stream AOI is already focused on the drainage feature,
   so threshold optimization is secondary to continuity and terrain fidelity.
5. Vectorize the first-cut result as `stream_network`, retaining branching when
   produced by the selected method. Do not promote it to an accepted network
   Observation merely because processing completed.
6. Preview the candidate over the hydro terrain and cutlines. Retain conditioning
   evidence as diagnostics without requiring fill depth on the review map.

Direction/accumulation should be computed once per routing recipe where practical;
threshold previews should reuse that result instead of repeating the expensive
terrain stage.

## Conditioning evidence

For every automated routing modification, retain enough evidence to compare
success with terrain disturbance:

- method and parameters;
- routing input and output fingerprints;
- count and proportion of valid cells changed;
- positive and negative elevation-change summaries, including maximum magnitude;
- affected footprint or a regenerable change raster;
- unresolved pits, flats, NoData boundaries or iteration limits;
- runtime, producer versions and completion/failure state.

This evidence supports a preference for the least disruptive successful recipe.
It does not establish that every changed cell is scientifically acceptable.

## Threshold and output meaning

The first interface should avoid making threshold selection the central analyst
task. Supply a defensible default, expose its explicit accumulation meaning and
units, and allow a limited adjustment with a fast preview. Cell count may be
reported as cells and optionally converted using grid cell area. A weighted
accumulation must identify its weight and resulting units. Never label a TauDEM
specific-area value, a cell count or a horizontal length as generic square area.

`stream_network` is the first-cut vector derivation. A future structural contract
must decide fields, segment/node relations and acceptance linkage after the method
benchmark demonstrates what the output reliably contains. Until then, retain at
least the exact terrain edition, cutline revision when applicable, routing recipe,
threshold, accumulation semantics, hashes, software and review state alongside
the candidate.

## Spencer Creek evaluation

Use the FG Studio local Study Area for Spencer Creek, Iowa, as the first real
evaluation. Read the saved data in place and publish experiments outside the
Study's `.local-data` tree so development cannot overwrite accepted inputs or
confuse diagnostics with saved editions.

The 2019-12 Event currently has three latest saved Hydro DEM results on 1 m grids:

- Stream `988375e376b73a5ed4cec3e0fe725761`: 8 cutlines, 8,974 by 10,688 cells;
- Stream `bded05866376da1bf336bc5b28313ff8`: 12 cutlines, 5,631 by 3,764 cells; and
- Stream `ecd0c683b62aa5483f5a25664ebd5018`: 7 cutlines, 4,525 by 3,551 cells.

Their retained provenance identifies source editions, cutline revisions, SHA-256
fingerprints, NAD83(2011) / UTM zone 15N horizontal coordinates, NAVD88
international-foot elevations and terra/sf/GDAL/PROJ versions. These files are
representative performance inputs, not package fixtures and not permission to
publish experimental results into the saved Study.

Evaluate, in increasing order of intervention:

1. direction and accumulation directly from each Hydro DEM;
2. the smallest available bounded fill strategy;
3. a bounded breach strategy when supported by a viable toolchain; and
4. a combined recipe only if simpler recipes do not produce a useful network.

For each candidate, compare runtime and memory demand; network continuity through
the focused AOI; passage through cutlines; fragments, loops and artificial
detours; branching and length; visual agreement with terrain/hillshade; and all
conditioning evidence above. Use small windows for deterministic development
tests, then the three full Stream rasters for the specific integration and
performance questions.

## Accepted local target

The first usable outcome is a nonempty vector `stream_network` candidate for an
explicit Spencer Creek Hydro DEM. Acceptance requires:

- reproducible binding to the exact input edition and cutline revision;
- explicit direction, accumulation, conditioning and threshold semantics;
- completion at native 1 m resolution within practical workstation resources;
- a connected and visually reviewable first-cut network in the focused Stream AOI;
- retained residual-conditioning evidence, including native fill depth;
- preservation of all existing terrain, cutline and prior result editions; and
- no claim that the candidate is a governed or accepted Network Observation.

Method completion alone is insufficient. Comparative evidence and analyst review
determine whether the result is useful and whether its terrain disturbance is
acceptable.

The owner accepted this local target on 2026-10-02 after reviewing the Spencer
Creek candidates and the integrated FG Studio workflow. Governed FGDB delivery
and broader terrain-form validation remain separate future work.

## Initial real-terrain evidence (2026-09-30)

The first real run used the smallest saved Spencer Hydro DEM, source SHA-256
`6dd6f66db420cbb726313b55a7b0460b6f1c71502f7b5ec48d2c27614ccbaaee`.
The 4,525 by 3,551 grid contained 1,956,115 valid cells. Direct D8-LTD took 70.47
seconds and emitted `exceeded the maximum number of iterations`; it left 9,852
zero-direction cells and 9,613 pit cells in 9,340 interior pit zones. Maximum
accumulation was only 12,523 cells (0.64 percent of valid AOI cells). The initial
diagnostic threshold of one percent of valid AOI cells was therefore 19,562 and
selected no cells. Pit detection and accumulation after direction completed in
0.35 and 1.50 seconds, showing that direction/terrain treatment, not accumulation,
is the immediate bottleneck.

The two larger Spencer rasters were intentionally not run after this bounded
failure. Scaling the same direct recipe would consume more resources without
answering the next question.

This benchmark also rejects a threshold defined as a fixed fraction of all valid
AOI cells. Threshold semantics must follow the selected accumulation/routing
method and the connected drainage result rather than rectangular or masked raster
occupancy.

## Current real-terrain experiment

The first native package implementation now combines registered C++ Priority-
Flood with terra block I/O, sparse valid-cell state, conservative memory refusal
and the explicit outlet/NoData policy. On the smallest saved Spencer Hydro DEM,
the fill completed in 6.66 seconds and raised 44,353 cells. D8-LTD then exceeded
its iteration limit and left 44,400 cells without directions, including 41,304
cells in 7,447 interior pit zones. Maximum accumulation was 12,208 cells. The
100-cell threshold map is therefore diagnostic, not an acceptable derived
stream line.

Every iteration requires analyst-reviewable visual evidence: a map identifying
all pixels changed by pit filling and a map of the derived stream line over the
real Hydro DEM. Record changed-cell counts, fill depths, remaining pits, routing
warnings, maximum accumulation, threshold and runtime. Do not proceed to another
Stream or select a production method until the analyst reviews these maps.

The first outlet diagnostic uses the retained terminal NHDPlusV2 segment to query
the NLDI downstream mainstem for its immediate continuation. Intersect that next
segment with the rounded Stream cap, then select the lowest valid Hydro DEM pixel
in a small explicit boundary neighborhood around the crossing. On the smallest
Spencer Stream, COMID `14803823` crosses the cap after selected terminal COMID
`14803819`; the unique local low cell is 19.8 m from that approximate crossing.
The published line remains a location prior, not positional validation. If the
continuation is unavailable, use the lowest pixel over the whole downstream cap
only as an analyst-reviewed fallback.

Stop at this result for analyst review. Do not silently impose an epsilon or run
the larger terrains. The next method increment must explicitly resolve spill-
level flats while preserving the unchanged Hydro DEM and the separate fill-depth
evidence.

That explicit flat-resolution increment now succeeds on the same real terrain.
The Barnes-style integer mask resolved all 44,399 non-outlet flat cells without
changing the routing elevations; all 1,956,115 valid cells accumulate at the
reviewed outlet. At the deliberately small 100-cell threshold, the retained
diagnostic contains 174,483 D8 line segments. It is the first complete-drainage
`stream_network` candidate, not an accepted network: its density, channel
fidelity and vector consolidation require analyst review before threshold tuning
or another Stream is processed.

The following threshold review reused that fixed accumulation result; it did not
rerun conditioning or routing. Thresholds of 100, 500, 1,000, 2,500, 5,000 and
10,000 one-square-metre cells retained 174,483, 48,435, 34,035, 22,877, 15,877
and 10,564 D8 edges, respectively. The 10,000-cell (1 ha) candidate preserves a
continuous longitudinal network while removing 93.95 percent of the original
100-cell diagnostic edges. It is therefore the provisional simple threshold for
this tightly focused Stream AOI, subject to analyst visual review rather than a
general watershed-initiation rule.

For vector review, the 10,564 retained cell edges were losslessly consolidated
into 99 maximal lines between 50 head nodes, 49 junction nodes and the outlet.
All source edges were used exactly once, all output geometries are valid, and
the network totals 12.58 km. Consolidation changes representation only; it does
not smooth, relocate, prune or reinterpret the D8 paths. The unreviewed vertical
and lateral branches visible in the map remain evidence to assess, not features
silently removed by post-processing.

The Spencer review output is also packaged as a self-contained QGIS folder with
the unchanged Hydro DEM, the filled routing DEM, fill depth, flat-resolved flow
accumulation and the consolidated candidate GeoPackage. The two DEM roles remain
explicit: Hydro DEM is measurement terrain; routing DEM is an analysis
representation. Raster files remain external GeoTIFFs rather than being embedded
in the vector GeoPackage.

## Future validation and delivery questions

- D8-LTD behavior at natural pools, wide/flat water surfaces, local reversals,
  NoData boundaries and diagonal connections on the Spencer terrain;
- validation on terrain forms beyond the three reviewed Spencer Creek Streams;
- whether a multiscale representation is ever needed without displacing native-
  resolution measurement terrain;
- project-specific threshold guidance beyond the accepted one-hectare starting
  value for focused Stream AOIs;
- validation metrics for branches, flats and raster diagonals; and
- the governed FGDB candidate/acceptance and portable delivery binding.

## Initial performance profile (2026-10-01)

Performance work is constrained by the intended Shiny worker, not by the larger
developer workstation. Profiling therefore separates full-grid raster I/O from
valid-cell native computation and treats peak resident memory as an acceptance
metric alongside elapsed time. Larger real terrains must remain preflight-only
until their complete job fits the configured worker allowance with headroom.

The smallest reviewed Spencer case has 16,068,275 grid cells but only 1,956,115
valid corridor cells. A current instrumented conditioning run took 10.06 seconds:
5.20 seconds in preflight, including 4.42 seconds scanning the raster; 0.25 seconds
loading compact elevations; 1.83 seconds in native Priority-Flood; 0.39 seconds
writing the routing DEM; 2.17 seconds writing fill depth; and 0.13 seconds in
validation, hashing and publication. The native algorithm is not the dominant
cost at this size.

An isolated flat-resolution run took 10.38 seconds: 4.84 seconds in another
complete preflight, 0.52 seconds loading routing and direction values, 4.13
seconds in native flat resolution, 0.34 seconds writing directions and the
remainder in hashes and validation. The prior accepted run measured 1.51 seconds
for accumulation. These repeated setup costs show that the production pipeline
should retain one preflight and one compact routing state across conditioning,
direction and flat resolution instead of treating them as independent jobs.

The retained comparable run measured 72.57 seconds for `terra::flowDir()` alone,
making direction calculation the clear elapsed-time bottleneck. It also exceeded
its internal LTD iteration limit before the native flat resolver repaired the
result. The first optimization candidate is therefore a native linear pass that
assigns strict-downslope D8 directions from the already loaded compact routing
surface, followed by the existing explicit flat resolver. This candidate must be
compared against the reviewed 1-hectare network before adoption; speed alone does
not authorize a changed drainage path.

The initial small-case preflight estimated 86.2 MiB for engine and raster-block
state, plus a 256 MiB process allowance. That estimate described Priority-Flood
only; it did not account explicitly for flat-resolution arrays, accumulation or
transient R objects. The 95,914,112-cell Spencer grid contains 10,890,882 valid
cells, so its sparse native state is plausible within a constrained worker, but
the complete application and worker must be measured before running that terrain
end to end. The later memory-conservative experiment below supersedes this
initial sizing model.

Optimization order is:

1. measure process peak resident memory for each isolated stage;
2. replace iterative terra LTD direction with a scientifically reviewed native
   strict-downslope pass plus explicit flat resolution;
3. reuse preflight, compact elevation state and raster handles across stages;
4. remove avoidable row-slice allocations from valid-domain discovery; and
5. test the three real Spencer grids in increasing size only after each prior
   case meets the configured Shiny memory and elapsed-time envelope.

## Native strict-downslope performance experiment (2026-10-01)

The first optimization candidate now assigns the maximum positive eight-neighbor
slope in one compiled pass over the already loaded Priority-Flood state, using
map-unit-aware orthogonal and diagonal distances. Cells without a strictly lower
neighbor remain zero until the existing Barnes-style flat resolver assigns them.
The reviewed outlet is then restored as the only terminal cell. This keeps the
simple D8 semantics explicit and avoids both iterative LTD direction finding and
a second terrain preflight/load cycle.

On the smallest real Spencer Hydro DEM, the integrated warm route took 8.75
seconds through publication, followed by 1.69 seconds for terra accumulation.
Within the integrated route, native Priority-Flood took 0.34 seconds, strict-
downslope direction assignment 0.11 seconds and flat resolution 0.33 seconds.
The comparable earlier staged path took 113.68 seconds when its internally timed
conditioning, terra LTD direction, separate flat-resolution job and accumulation
are summed. The integrated candidate therefore reduced comparable warm elapsed
time from 113.68 to 10.44 seconds (90.8 percent) while routing all 1,956,115 valid
cells to the reviewed outlet. A cold standalone process, including package
loading, took 20.02 seconds versus 123.53 seconds for the earlier profile.

The 1-hectare candidate contains 10,483 D8 edges consolidated into 95 lines and
totals 12.495 km. The reviewed candidate contains 10,564 edges, 99 lines and
12.577 km. Exact raster-edge comparison finds 10,270 common edges, 213 native-
only edges and 294 reviewed-only edges, for a Jaccard similarity of 0.953. This
is strong agreement, but it is not automatic scientific acceptance: the changed
branches and paths remain highlighted in the review map for analyst evaluation.

This experiment supports the working hypothesis that information-dense 1 m
terrain benefits from a simple linear local direction rule after minimal global
conditioning. It does not yet establish that conclusion across larger Stream
DEMs or other terrain forms. The next performance step is worker-level peak-RSS
measurement of this integrated path before authorizing the medium terrain.

That Windows process-tree measurement found a 673.77 MiB resident baseline after
loading fluvgeo, terra and the raster handle. The isolated integrated routing job
peaked at 956.18 MiB for the process tree, while standalone accumulation peaked
at 784.11 MiB. Running routing and accumulation sequentially in the same fresh R
process peaked at 1,261.89 MiB because the worker retained allocated pages across
stages even after explicit garbage collection. The private-byte readings were
substantially larger and are retained in the machine-readable evidence, but
working set is the relevant observed resident-memory measure here.

The small terrain therefore does not yet have acceptable headroom for a nominal
one-GiB Shiny worker, despite the compact native algorithm itself adding only
about 282 MiB over the loaded-package baseline. Do not run the medium terrain
yet. The next implementation increment should reduce retained full-process
memory, distinguish reusable Shiny baseline from per-job allocations, and revise
preflight to cover flat-resolution and accumulation state rather than only the
Priority-Flood engine.

## Memory-conservative routing experiment (2026-10-01)

The accumulation stage now uses the resolved compact D8 graph directly. A
topological pass stores one Float64 accumulation value, one UInt32 indegree and
one UInt32 work-queue entry per valid corridor cell; it allocates nothing for
NoData cells outside the corridor. On the smallest real Spencer terrain it took
0.22--0.26 seconds to calculate and about 0.52--0.58 seconds to write. Blockwise
comparison with the earlier terra result found zero differing cells and zero
maximum absolute difference. The compact accumulator therefore preserves the
accepted cell-count semantics exactly on this case.

Priority-Flood queue and heap indices now use the already enforced UInt32 compact
cell domain rather than UInt64 full-grid cell numbers. This reduces worst-case
native queue capacity without changing elevations, directions or accumulation.
The final routing DEM, fill-depth DEM and direction raster have identical SHA-256
values to the prior native-D8 result; accumulation values are exactly equal.

Removing the separate terra accumulation process, retaining one compact state,
and eliminating a redundant post-run `global(range)` scan reduced the complete
process-tree peak from 1,261.89 to 966.91 MiB. Making legacy `tmap` reporting
functions load `tmap` only when invoked reduced the fluvgeo namespace baseline
from 671.31 to 630.98 MiB and reduced the complete real-terrain peak again to
899.27 MiB. Relative to the original sequential route, this is a 362.62 MiB
(28.7 percent) resident-memory reduction. The final native stages remained fast:
Priority-Flood 0.59 seconds, direction 0.15, flat resolution 0.39 and accumulation
0.26 seconds in the retained run.

Clean-worker measurements clarified the remaining architecture cost: base R
peaked at 82.16 MiB, terra at 211.21 MiB, and the first memory-slimmed fluvgeo
namespace at 630.98 MiB. Isolated dependency probes showed that the package's
broad spatial and reporting dependency graph, rather than compact hydrology
arrays, dominated startup memory. The second dependency pass therefore made
feature-specific mapping, report, raster-conversion and network-service packages
lazy: `tmap`, `leafem`, `hydrogeofetch`, `mapboxapi`, `raster`, `rmarkdown`,
`stars`, `terrainr` and `testthat` are now optional and loaded only by functions
that use them. This reduced a routing worker's fluvgeo startup peak to 260.37 MiB.

The complete production-shaped route was then repeated on the same real Spencer
Hydro DEM. It peaked at 592.53 MiB and finished in 14.89 seconds; the integrated
route itself reported 8.91 seconds after package startup. This is 306.74 MiB
(34.1 percent) below the 899.27 MiB intermediate result and 669.36 MiB (53.0
percent) below the original 1,261.89 MiB sequential pipeline. Routing, fill depth,
resolved direction and accumulation GeoTIFFs are byte-identical to the preceding
899.27 MiB run. The memory reduction therefore did not alter the scientific
result.

The route has about 175 MiB of measured headroom beneath a worker-local
75-percent allowance in a nominal one-GiB dedicated worker. Deployment cannot be
sized from the worker alone. A restored Spencer FG Studio server-side session,
with the visible geometry outputs forced in `shiny::testServer`, peaked at 901.29
MiB while the concurrent route peaked at 578.28 MiB. Their observed co-resident
peak was 1,479.57 MiB. Browser rendering is excluded because a Posit container
does not host the analyst's browser; the workstation browser connector could not
attach to localhost, so this is server-session rather than WebSocket-client
evidence.

The calibrated preflight now separates a 416 MiB fixed routing-worker allowance
from a 960 MiB active-session reserve. For the small Spencer terrain, the compact
arrays, raster blocks and observed transient allocation add 199.25 MiB, producing
a deployment estimate of 1,575.25 MiB. This intentionally blocks a two-GiB
container under the 75-percent safety policy (1,536 MiB usable) and passes a
three-GiB container (2,304 MiB usable). Dedicated-worker diagnostics may set the
foreground reserve to zero; the default application path may not.

The read-only preflight was then run on the next Spencer Hydro DEM: 21,195,084
grid cells, 2,411,766 valid corridor cells and 11.38 percent valid coverage. It
completed in 6.14 seconds and estimated a 1,584.42 MiB co-resident peak. That case
is also blocked at two GiB and passes at three GiB under the 75-percent policy.
No terrain conditioning, direction, accumulation or vector output was run.

Do not run the medium terrain until a deployment budget of at least three GiB is
explicitly established. Tiling Priority-Flood is not the preferred next move: it
would complicate global spill connectivity while targeting arrays that no longer
dominate measured memory.

## Implemented reusable API (2026-10-01)

`locate_stream_outlet()` uses the saved NHDPlusV2 chain and next-downstream NLDI
segment only to approximate the downstream cap, then selects the lowest valid
Hydro DEM boundary cell near that crossing. Where NLDI has no continuation, it
falls back to the lowest valid cap cell near the terminal reference endpoint.
`extract_synthetic_stream_network()`
owns the complete native Priority-Flood, resolved D8, upstream-cell accumulation,
hectare threshold and topological line-consolidation workflow. It writes the
analytical rasters, native fill-depth diagnostics, `stream-network.gpkg` and
provenance without overwriting the Hydro DEM. Later threshold changes use
`threshold_synthetic_stream_network()` to reuse direction and accumulation and
rebuild only the vector candidate.

On the real west Spencer Creek Hydro DEM, the automatic outlet was cell 15,798,377
at elevation 660.3914 feet, 19.80 metres from the downstream reference crossing.
At one hectare the public API produced 95 lines totaling 12,493.84 metres, with
44,352 changed cells and maximum fill 19.10022 feet. This is integration evidence
for the first analyst workflow, not validation across other terrain forms.
