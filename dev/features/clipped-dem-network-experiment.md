# Clipped Sinsinawa: logical network through DEM preparation

- Date: 2026-09-05
- Status: original experiment passed; production follow-up passed (below);
  not an accepted observation
- Reproduce from `fluvgeo`: `Rscript --vanilla dev/scripts/clipped-dem-network-experiment.R`
  using the executable resolved by the workspace workstation instructions.
- Extends [the network interoperability experiment](network-processing-experiment.md).

## Result

The retained mainstem, clipped to the surviving DEM's usable footprint, passes
through sfnetworks consolidation, the existing DEM direction method,
`prepare_stream_network_from_features()`, and Hydroloom connection rebuilding.
All 28 original assertions passed. That experiment changed neither source data
nor production code; the subsequent implementation is recorded below.

| Stage | Pieces / logical links | Length (m) | Direction assessment |
| --- | ---: | ---: | --- |
| Original retained mainstem | 99 | 4759.190 | Original DEM pairing remains incomplete |
| Clipped to rectangular DEM extent | 55 | 3146.165 | 46 supported, 6 NoData, 3 equal |
| Clipped to usable DEM footprint | 51 | 3059.164 | 48 supported, 3 equal |
| Consolidated logical mainstem | 1 | 3059.164 | Supported; upstream coordinate order established |

The logical line initially samples 654.7000 at its northern end and 630.8399
at its southern end, in native raster vertical units. Preparation reverses it
into the required downstream-to-upstream coordinate order. The original three
equal-elevation pieces (`arcid` 1126, 1268, 1278) are still represented in the
source membership; none was discarded to achieve the result. They no longer
need independent direction decisions.

Preparation produces one direction operation and one remaining issue:
`SEGMENT_REVIEW_REQUIRED`. The result remains `REVIEW_REQUIRED`, with unset node
UUIDs and unresolved segment role. Hydroloom correctly represents the separate
downstream-oriented computational copy as a single-link network with one
computational outlet. That outlet is a clipping boundary, not an assertion about
the stream's true mouth.

## Coverage detail discovered during the experiment

Matching bounding rectangles is insufficient. The northern and southern
rectangle-clipped endpoints both occupy NoData cells. Consolidating first does
not fix that; production preparation correctly rejects the result.

The positive fixture instead intersects the linework with a polygon of finite
DEM cells. Exact intersections on the valid/NoData cell boundary still select
NoData under containing-cell sampling. A fixture-only inward margin of
0.000001 m (one millionth of the 1 m cell dimension) places the endpoints
unambiguously inside valid cells. It removes only about 0.00000242 m of line.
A tenfold margin gives identical endpoint elevations and orientation. No
elevations are interpolated or filled; production extraction is unchanged.
This is a disclosed experimental boundary treatment, not a general adopted
clipping tolerance or repair policy. Any footprint gaps would remain gaps;
this particular usable subset is one connected chain.

## Provenance and limits

- Inputs are sibling `fluvgeodata/inst/extdata/testing_data.gdb` layer
  `stream_network` and `dem_1m.tif`.
- The user confirmed this is an edited/pruned mainstem. Clipping makes a useful
  experimental stand-in for segmented extraction output, not a genuinely raw
  tributary network or proof of its original DEM derivation.
- All 51 retained source features map to the prepared segment in an experimental
  membership side table. The other 48 original features lie outside the retained
  usable subset and are excluded, not silently repaired.
- The production source table currently contains one row for its one input
  logical line. It does not yet incorporate the 51 historical source rows.
- Constructor UUIDs and observation year are explicitly test scaffolding;
  they are not enterprise identities or a verified historical terrain date.
  Coverage is marked `PARTIAL_CONFIGURATION`; no accepted state is fabricated.
- Endpoint elevation contrast supports this method's orientation, not a
  monotonic longitudinal profile or independent verification of actual flow.
- Checks cover negative coverage rejection, all three equal cases retained,
  geometric/length preservation, margin sensitivity, preparation output,
  many-source membership, Hydroloom outlet/sorting, high-to-low computational
  geometry, repeat-run orientation, and unchanged source/DEM data.
- Environment: sfnetworks 0.6.6, hydroloom 1.2.1, sf 1.1.2, terra 1.9.46,
  tidygraph 1.3.1, igraph 2.3.3, and the current local fluvgeo source. Existing
  R locale and testthat build-version startup warnings occurred.

## Scope to close the production preparation task

The demonstrated sequence is sufficient for this mainstem's geometric
consolidation and endpoint-based direction. The remaining work is integration:

1. Add logical-link construction with explicit protected boundaries to the
   preparation workflow, before DEM orientation; retain clip/gap boundaries.
2. Carry original-feature membership and clipping/consolidation operations into
   the existing source and operation relations, rather than treating the
   consolidated line as the only source.
3. Map network endpoint relationships to governed node IDs and assign segment
   roles from appropriate evidence. `MAINSTEM` is known for this fixture from
   the user; it is not a universal inference for arbitrary networks.
4. Reassess the resulting node, role, direction, and topology findings through
   an acceptance validator, retaining partial-coverage qualification and human
   review where required. Do not merely remove `SEGMENT_REVIEW_REQUIRED`.

Package integration and acceptance are proposed next work, not implemented by
this standalone experiment. Original DEM recovery and rivnet remain deferred.
## Production integration follow-up (2026-09-05)

The user accepted the experiment as sufficient evidence to proceed. The script
now also passes the 51 raw clipped pieces directly into production preparation
with `consolidate = TRUE`. Verified: one logical link, all 51 original source
relationships, unchanged geometric result, and two ordered operations
(`CONSOLIDATE_SEGMENTS`, `REVERSE_DIRECTION`). Only SEGMENT_REVIEW_REQUIRED
remains. All 38 checks passed (28 original plus 10 integration checks).

This replaces the need for a lineage sidecar in that production path; the
original experiment and its limitations below remain historical evidence.
Clipping/inset policy is still experimental, not part of production preparation.

## Connectivity integration follow-up (2026-09-05)

With `consolidate = TRUE, connect = TRUE`, the 51 raw clipped pieces now produce
one oriented candidate link, two endpoint node UUIDs, one observed-outlet
connection row, and three ordered operations: consolidation, direction reversal,
and node assignment. All 51 source relationships and the prior geometric result
are preserved. All 47 checks pass (38 preceding checks plus 9 connectivity checks).
Only role and observation-acceptance review remain in the segment finding.
The downstream boundary remains the edge of this clipped observation, not a
claim about the physical mouth of the Sinsinawa.
