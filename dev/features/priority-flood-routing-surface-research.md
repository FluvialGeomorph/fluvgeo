# Priority-Flood routing-surface research and specification

Status: first native implementation and smallest-Spencer diagnostic completed;
scientific routing method remains under review

## Purpose and boundary

This document scopes a native compiled depression-filling capability for
`fluvgeo` that can support synthetic `stream_network` extraction in FG Studio.
It responds to the poor full-surface runtime of terra 1.9.46 `pitfiller()` on the
smallest real Spencer Creek Hydro DEM. It does not replace the prepared or Hydro
DEM, select a final flowline, or authorize a public API.

The intended product is a temporary or retained **routing representation**. The
selected Hydro DEM remains measurement terrain. Every raised pixel and fill depth
must be reviewable. Analyst cutlines remain the intentional treatment of known
infrastructure barriers.

The deployment target is an R package used by a Shiny application, including
memory-constrained Linux containers. An external executable is out of scope. Any
compiled implementation must build as ordinary package source on Windows and
Linux and must be qualified in the actual FG Studio deployment profile.

## Research basis

The primary algorithm reference is Barnes, Lehman and Mulla's
[Priority-Flood paper](https://rbarnes.org/sci/2014_depressions.pdf). For
floating-point terrain, the improved algorithm uses a priority queue for the
advancing terrain boundary and a plain FIFO queue after entering a depression.
It raises depression cells to their lowest spill elevation, changes no cell that
is already high enough to drain and has `O(m log m)` time complexity, where
`m <= n`.

Three variants matter but are not interchangeable:

- Improved Priority-Flood fills to spill elevation and produces flats.
- Priority-Flood plus epsilon uses the next representable elevation to impose a
  draining gradient. Its safety checks and storage precision are consequential.
- Priority-Flood plus flow directions assigns a D8 path while implicitly
  resolving depressions; it is not a filled-terrain output and is not D8-LTD.

Barnes, Lehman and Mulla's separate
[flat-resolution algorithm](https://arxiv.org/abs/1511.04433) constructs a
convergent drainage field over flats in linear time without iterative elevation
adjustment. It is a possible later stage if D8-LTD cannot route a spill-level
surface; it is not part of the initial fill contract.

Zhou, Sun and Fu's
[one-pass Priority-Flood variant](https://doi.org/10.1016/j.cageo.2016.02.021)
uses region growing to reduce priority-queue work and reported an average 44.6%
speed improvement over the fastest compared existing variant. It is a worthwhile
optimization reference, but it is more complex than the Barnes improved
algorithm and should follow a correct baseline rather than replace it initially.

Barnes's
[parallel tiled Priority-Flood](https://arxiv.org/abs/1606.06204) is the relevant
out-of-core design. It does not independently fill and mosaic buffered tiles.
It solves and labels tiles, joins their spillovers in a global graph, solves that
graph and revisits each tile to apply globally consistent elevations. This is a
possible scale-out implementation, not a use of `terra::tile_apply()` around a
local fill function.

Existing implementations are evidence, not code sources. RichDEM and the R
PriorityFlow package are GPL-3, while `fluvgeo` currently declares CC0. The Zhou
reference repository does not establish a compatible reuse license in the
evidence reviewed here. Implement from published algorithms and project-owned
tests unless a separate license review authorizes source reuse.

The CRAN `topmodel::sinkfill()` accepts a complete matrix, enforces a supplied
minimum slope and may require repeated calls for deep sinks. PriorityFlow uses
pure-R matrices, D4 traversal and ParFlow-oriented domain/network enforcement.
Neither supplies the file-backed, D8, terrain-preserving package contract needed
here.

## Terra out-of-memory role

Terra is the raster storage and orchestration layer. Its low-level
`readStart()` / `readValues()` and `writeStart()` / `writeValues()` methods stream
row blocks without materializing a full `SpatRaster`. `blocks()` chooses bounded
chunks, and the source Hydro DEMs are already 256 by 256 tiled GeoTIFFs.

`tile_apply()` is useful for genuinely local or correctly decomposed algorithms,
but a finite overlap does not make a local depression fill globally correct.
Use it only if a later implementation provides the complete tile-label and
spillover-graph phases from the parallel algorithm. Do not accept a seam-free
mosaic as proof of hydrologic equivalence.

The proposed first engine therefore combines terra block I/O with a compact
in-memory representation of valid terrain cells. It is not a whole-raster matrix
and it must refuse execution before allocation when its conservative memory
estimate exceeds the configured job budget.

## Real Spencer input profile

Read-only block inspection of the three current 1 m Hydro DEMs found:

| Hydro revision | Grid cells | Valid cells | Valid share | Valid runs by row | Maximum runs in one row | GeoTIFF size |
|---|---:|---:|---:|---:|---:|---:|
| `f11fcede5f4c2a80d079188a633cab4c` | 16,068,275 | 1,956,115 | 12.17% | 4,701 | 3 | 5.6 MiB |
| `699ec297b78c94d82eef5ae41fb87ccc` | 21,195,084 | 2,411,766 | 11.38% | 5,701 | 2 | 7.1 MiB |
| `b7387008b2a5029abcadeff3dc0815c4` | 95,914,112 | 10,890,882 | 11.36% | 10,117 | 5 | 30.5 MiB |

All are Float32 GeoTIFFs with 256 by 256 source blocks. Their valid masks span
nearly the full row/column range but are sparse and row-wise simple. Cropping the
rectangular extent therefore saves little, while compacting only valid cells can
avoid retaining approximately 88% of the grid in the algorithm state.

## Proposed computational design

### Sparse valid-domain index

In a first terra pass, identify contiguous valid-cell runs for each raster row
and count valid cells. Retain only run start, run end and the first compact index.
The observed Spencer masks need about ten thousand runs even on the largest grid.
At most five runs occur in a row, so mapping an eight-neighbor grid coordinate to
a compact cell index is bounded and does not require a dense grid-sized index.

In a second pass, copy valid Float32 elevations into one compact C++ vector in
row-major order. Preserve the original full-grid cell number in queue nodes; row
and column follow from that number, and the row-run table maps neighbors back to
compact values.

The improved Priority-Flood then uses:

- one compact Float32 elevation vector, modified in place for the routing result;
- one byte per valid cell for closed/visited state;
- a minimum heap containing full-grid cell number and elevation;
- a FIFO queue of cell numbers inside depressions; and
- stable deterministic tie handling documented by the method version.

After filling, terra rereads the source in blocks and requests corresponding
filled values from the compact state. It writes a new Float32 routing GeoTIFF and
a Float32 fill-depth GeoTIFF using `writeStart()` / `writeValues()`. It never
constructs a full R matrix and never overwrites the source.

### Memory preflight

Use a conservative estimate rather than depending on allocation failure. For
`v` valid cells, budget at least:

- `4v` bytes for compact Float32 elevations;
- `v` bytes for visited state;
- `4v` bytes for a worst-case FIFO queue;
- a documented worst-case heap allowance, including container capacity and node
  alignment rather than only payload bytes;
- two terra row blocks represented as R doubles during input/output conversion;
  and
- fixed R, terra, GDAL and Shiny-worker headroom.

With 10.89 million valid cells, the largest Spencer grid is materially smaller
than a full 95.9-million-cell double matrix, but the priority queue remains data
dependent. The first implementation target is no more than 512 MiB incremental
peak memory on the largest Spencer input and safe refusal before 75% of the
job-specific memory budget. These are proposed engineering targets requiring
measurement, not established performance facts.

Default execution should be single-process. Additional workers multiply raster
blocks, heaps and application overhead. Posit Connect Cloud currently exposes
configurable limits from 1 to 32 GB RAM and 1 to 8 CPUs depending on plan, and
charges usage according to configured CPU and memory. The backend must not assume
more than one CPU or use parallelism merely because it is available. See the
[Connect Cloud compute settings](https://docs.posit.co/connect-cloud/user/manage/content_settings.html)
and [usage accounting](https://docs.posit.co/connect-cloud/user/usage.html).

If the sparse preflight cannot fit a supported job, stop with an actionable
estimate. A later scale-out mode should implement the published tile/spillover
graph algorithm using terra-backed tile files; naive buffered `tile_apply()` is
not an allowed fallback.

## Initial scientific contract

### Connectivity

Use eight-neighbor connectivity to align with D8 routing. Corner adjacency must
be deterministic. The routing result may raise cells only; lowering and smoothing
are outside this fill operation.

### NoData and NHDPlusV2-informed outlets

The ordinary algorithm treats the terrain boundary, including valid cells
adjoining NoData, as able to drain. That is unsuitable as the initial FG policy:
the Spencer masks are narrow corridor-like domains, so it would expose most
corridor edges as outlets and permit lateral escape rather than longitudinal
routing.

The Stream corridor was derived from retained, downstream-digitized NHDPlusV2
flowline segments. Those published lines are simplified and can be out of date
relative to the high-resolution LiDAR-derived terrain. Use them only as an
approximation of the general downstream outlet neighborhood, without treating
them as the extracted flowline or forcing the routing path to follow them. The
initial policy is `downstream_edge`:

1. Build the directed topology of the exact retained clipped source segments and
   identify its terminal downstream endpoint. Multiple terminal endpoints or an
   unresolved/cyclic topology require review rather than an inferred choice.
2. Prefer a bounded NLDI downstream-mainstem query that includes the immediately
   following NHDPlusV2 segment. Intersect that continuation with the rounded
   Stream-domain cap. Save the returned geometry and retrieval evidence with the
   Stream selection so extraction does not depend on a live service call.
3. Because the vector crossing can fall exactly between valid and NoData raster
   cells and the published line is only approximate, inspect a small explicit
   boundary neighborhood around the crossing and select its lowest valid Hydro
   DEM pixel. Terrain determines the exact outlet cell. Multiple equal minima, a
   broad low/flat boundary, multiple crossings or no crossing require review.
4. When a downstream continuation is unavailable, the offline fallback is the
   lowest valid pixel on the entire rounded downstream cap, always returned for
   analyst review rather than silently accepted.
5. Record the source selection and NLDI response fingerprints, COMIDs, crossing,
   search radius, candidate rule and selected raster cells. Search distance and
   ambiguity rules remain explicit rather than hidden constants.
6. Seed Priority-Flood only from the reviewed outlet zone. Other exterior
   valid-domain edges are closed boundaries for this operation.
7. Treat enclosed NoData holes as barriers, not drainage outlets.

This use of NHDPlusV2 is a weak location prior at the downstream boundary, not a
positional reference or circular validation of the new synthetic stream. The
derived line must be evaluated against terrain independently. If the bounded
search cannot identify a plausible unambiguous boundary zone, require analyst
outlet placement; never enlarge the search silently to force a match.

### Fill amount and stopping

Priority-Flood is not an iterative partial-fill model. A depression is raised to
its spill elevation or remains unresolved. `maximum_fill_depth` and
`maximum_changed_cells` should initially be acceptance gates: compute the
candidate, retain diagnostics and refuse automatic acceptance when a gate is
exceeded. Silently clipping fill depth would forfeit the drainage guarantee.

### Flats

The initial candidate is fill-to-spill without epsilon, followed by
`terra::flowDir(lambda = 0.5, deviation_type = "ltd")`. This directly tests the
owner's established manual workflow. If D8-LTD cannot route the resulting flats,
stop and review maps. Do not silently add epsilon or replace D8-LTD.

If a later epsilon representation is considered, it must be Float64: successive
`nextafter` increments can collapse when written to Float32, while a long flat
path can accumulate a material Float32 increment. The epsilon footprint and total
added elevation require separate evidence from spill filling.

## Proposed evidence and review loop

Each candidate iteration on the smallest real Spencer Hydro DEM must produce:

- the source fingerprint and unchanged-source verification;
- the routing and fill-depth GeoTIFFs;
- a map of every raised pixel, colored by fill depth;
- counts and proportions of raised cells;
- maximum, quantiles and total volume of fill;
- unresolved pits after D8-LTD;
- flow-direction warnings and maximum accumulation;
- the exact low stream-initiation threshold in cells;
- a vector or centerline map of the derived stream over the Hydro DEM;
- runtime and peak-memory measurements by stage; and
- the outlet, NoData, connectivity, precision and tie-breaking policies.

The first initiation threshold should be deliberately small and is subordinate
to confirming successful direction and accumulation. Do not tune it as a proxy
for fixing disconnected routing. Stop after every iteration for analyst review;
do not automatically process another Stream.

## Deployment and package implications

`fluvgeo` currently has no `src/` directory or compiled-code registration. A
future implementation must add a minimal registered native interface and pass
package checks on Windows and Linux. Rcpp is convenient but not required; a base
R `.Call` interface avoids adding a new package dependency and deserves preference
for this small, performance-critical boundary. Any stateful external pointer must
have deterministic cleanup on errors and interruption.

Connect Cloud reconstructs R deployments from a manifest and supported R/package
versions. Package-contained C/C++ is a normal package build concern, but actual
FG Studio deployment must verify compilation, temporary-file permissions, memory
limits and cancellation. See the official
[Connect Cloud R deployment documentation](https://docs.posit.co/connect-cloud/user/platform/r.html).

## Blockwise preflight implementation evidence

Development 2026-09-30 adds an internal, non-public
`.fg_routing_surface_preflight()` increment. It uses terra row-block reads to
discover every valid-cell run, checks source identity before and after scanning,
and estimates future engine memory before any compact elevation, visited, FIFO or
heap allocation. It writes no terrain product and returns
`processing_authorized = FALSE`.

With a 1,024 MiB job budget, 75 percent safety allowance, 16 MiB input-block
target and 256 MiB fixed process headroom, all three real Spencer Hydro DEMs pass:

| Hydro revision | Scan time (s) | Estimated engine (MiB) | Estimate with fixed headroom (MiB) | Safe allowance (MiB) |
|---|---:|---:|---:|---:|
| `f11fcede5f4c2a80d079188a633cab4c` | 4.06 | 86.2 | 342.2 | 768 |
| `699ec297b78c94d82eef5ae41fb87ccc` | 4.78 | 98.8 | 354.8 | 768 |
| `b7387008b2a5029abcadeff3dc0815c4` | 13.05 | 333.4 | 589.4 | 768 |

These are conservative model estimates, not measured fill peaks. They include a
full-capacity FIFO, reserved heap nodes and two raster blocks. The largest
estimated engine remains below the proposed 512 MiB incremental target. Focused
tests cover exact run indexing across blocks, source preservation and refusal
when the safe allowance is too small.

The first downstream-cap diagnostic used the smallest real Spencer Hydro DEM and
its three retained NHDPlusV2 source segments. The terminal selected segment is
COMID `14803819`; its endpoint lies about one 152.4 m corridor-buffer radius
inside the rounded cap, so nearest-boundary snapping is underdetermined. A
read-only five-kilometre NLDI downstream-mainstem query returned the immediate
continuation, COMID `14803823`, which crosses the rounded cap 152.1 m from the
retained endpoint. Within a 25 m boundary neighborhood of that crossing, 65 valid
boundary cells were present. The lowest was unique at 660.3914 ft and lay 19.8 m
from the vector crossing, where the low LiDAR channel reaches the eastern cap.
This qualifies the crossing-plus-local-terrain rule on one Stream; the outlet
remains a diagnostic candidate until analyst review.

The installed `hydrogeofetch::navigate_nldi()` currently passes an older `origin`
argument shape to the installed `dataRetrieval::findNLDI()` and collapses the
resulting error to `NULL`. The successful diagnostic called the current
`dataRetrieval` interface directly. Resolve that dependency compatibility in the
existing drainage service seam before making downstream-continuation retrieval a
supported backend behavior; do not interpret `NULL` as absence of a continuation.

## Recommended implementation sequence after review

1. Add a bounded, retained NLDI downstream-continuation lookup through the
   drainage service seam and qualify cap intersection plus local lowest-cell
   selection; stop when service evidence, topology, crossing or terrain is
   ambiguous.
2. Expose the job memory budget through FG Studio background-job configuration;
   retain the 1,024 MiB/75-percent values as development defaults until the
   deployment profile supplies an explicit limit.
3. Implement the sparse improved Barnes Priority-Flood in registered C++ without
   copying external source code.
4. Write routing and fill-depth outputs through terra blocks and verify exact
   source/mask/grid preservation.
5. Run one iteration on the smallest Spencer DEM, then D8-LTD, accumulation and
   the two required review maps.
6. Stop for analyst review. Flat resolution, Zhou optimization, scale-out tiling,
   larger Streams and the stable public API remain separate decisions.

## First compiled real-terrain finding

Development 2026-09-30 implemented the sparse improved Barnes Priority-Flood as
registered package C++ with terra block input/output. The first run used only the
smallest saved Spencer Creek Hydro DEM and the reviewed NLDI-informed outlet
cell. It visited all 1,956,115 valid cells in 6.66 seconds, raised 44,353 cells
(2.267 percent), and produced a maximum fill depth of 19.1002 ft. The source
GeoTIFF fingerprint was unchanged. Routing and fill-depth GeoTIFFs were written
as new job-owned outputs.

This successful fill did **not** establish a successful routing surface. Terra
D8-LTD took 72.57 seconds, reported that it exceeded its maximum iteration
count, assigned direction zero to 44,400 cells and left 41,304 cells in 7,447
interior pit zones. Maximum accumulation was only 12,208 cells. At the deliberately
small 100-cell threshold, 142,041 D8 cell-to-cell segments exist, but they are a
diagnostic visualization of a disconnected result and are not `stream_network`.

The result confirms the anticipated distinction between depression filling and
flat resolution: fill-to-spill creates level surfaces that D8-LTD does not fully
route on this real LiDAR terrain. No epsilon gradient or second conditioning pass
was applied. The next scientific decision is whether to add the Barnes flat-
resolution field, a separately evidenced Float64 epsilon representation, or
direct Priority-Flood D8 directions. Do not process the larger Spencer rasters or
publish a candidate network until that choice and the paired maps are reviewed.

The next bounded iteration implemented the Barnes, Lehman and Mulla integer flat
mask over the same sparse valid-cell state. It retains terra's already assigned
D8-LTD directions outside flats, treats the reviewed outlet as the sole terminal,
and assigns ordinary D8 codes only to formerly direction-zero cells. The mask is
an integer direction aid; it is not added to the routing elevations.

On the same smallest Spencer result, the native flat phase identified 6,967
drainable flat regions and resolved all 44,399 non-outlet direction-zero cells in
0.26 seconds (6.14 seconds including block input/output). The largest mask value
was 398. Terra accumulation then completed in 1.51 seconds. Exactly one direction-
zero cell remains—the reviewed outlet—and its accumulation is 1,956,115 cells,
equal to the complete valid domain. `pitfinder()` correspondingly reports only
that terminal cell. A deliberately small 100-cell initiation threshold produces
174,483 vector D8 segments in a diagnostic `stream_network` GeoPackage. This
establishes complete drainage, but threshold selection, cell-segment
consolidation and geomorphic review remain before accepting the candidate.

## Decisions still required

- What local search radius around an NLDI cap crossing is acceptable before
  requiring analyst outlet placement, and should it scale with raster resolution?
- What explicit job-memory limit will FG Studio receive from each deployment
  profile?
- What fill-depth or changed-area finding requires refusal versus review?
- If D8-LTD cannot route spill-level flats, should FG evaluate a separate flat
  direction field, Float64 epsilon terrain, or direct Priority-Flood D8 routing?
