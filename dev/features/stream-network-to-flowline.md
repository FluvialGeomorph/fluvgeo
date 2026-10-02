# Reviewed Flowline derivation from a synthetic Stream Network

- Status: proposed for owner review
- Updated: 2026-10-02
- Workflow position: after accepted local `stream_network` extraction and before
  Flowline Points
- Governing domain draft:
  [FGDB Flowline feature contract](../../../FGDB/dev/schemas/flowline-feature-contract.md)

## Outcome

Turn one exact saved terrain-derived `stream_network` candidate into an
analyst-reviewed local Flowline candidate for every applicable Reach under the
selected event setting. A Flowline is the likely reference flow path through one
Reach. It is not asserted to be a wetted path, channel centerline, or surveyed
thalweg.

The first acceptance target is the Spencer Creek Study: three reviewed Stream
Network candidates and the eleven current Reaches. The analyst must be able to
review the selected channel, compare raw and smoothed geometry against the Hydro
DEM, save the result, close the app, and reopen the same candidate.

## Functional gap

The legacy `_05a_Flowline.py` did not select the analyzed path. Before running
it, an analyst manually removed tributaries from `stream_network` and populated
`ReachName`. The tool then dissolved by `ReachName` and applied Esri PAEK
smoothing. Current `fluvgeo::flowline()` solves a different partial problem: it
accepts one already drawn line and uses DEM endpoint elevations to orient it. It
does not select a path from a branched network, bind that path to current Reach
identities, or replace PAEK.

The new synthetic networks make the missing decision explicit. Each current
Spencer candidate is a one-outlet directed tree with 29–31 maximal lines and
15–16 heads. Each saved Reach polygon intersects several of those lines.
Clipping the network to a Reach polygon therefore preserves tributaries rather
than producing a Flowline. Selecting the largest-accumulation branch is also not
sufficient: a named Stream can join a larger channel, and the intended project
path is a scientific scope decision rather than a generic mainstem statistic.

## Proposed derivation

### 1. Bind exact inputs

Use one immutable saved Stream Network revision, its Hydro DEM fingerprint, the
current Stream identity, exact local event setting, any explicitly linked Survey
Event identities, and the current Reach source-piece evidence. A changed
threshold, network hash, Reach assignment, Hydro edition, or context revision
makes an earlier Flowline candidate stale; it is never silently transferred to
the changed inputs.

The current Spencer 2019-12 setting has no governed Reach-owned Survey Event IDs
in `event_links`. The local feature must therefore bind each candidate to its
Reach ID, local event-setting ID, date/precision, and input revisions without
inventing `survey_event_id`. A later governed-delivery step must reconcile or
create the owning Survey Events before publication under the FGDB Flowline
contract.

Validate that the candidate network is an acyclic directed graph with exact
endpoint connections and one observed outlet. Its geometries currently run from
upstream to downstream and carry `upstream_cell` and `downstream_cell`; preserve
that routing evidence during selection.

### 2. Recommend one Stream-level path

Every network head defines one unique route to the observed outlet. Rank those
head-to-outlet routes against the retained NHDPlusV2 Stream chain and current
Reach source pieces over their full length. Reference hydrography supplies only
approximate branch and extent evidence: it may be simplified or out of date and
must never replace the terrain-derived coordinates.

The first implementation should compare candidate routes using explicit
proximity/coverage evidence across the complete retained reference chain, with
deterministic tie-breaking and a reported separation from the next candidate.
Accumulation can support the ranking but cannot independently define the answer.
If the reference chain is incomplete, a unique route is not supported, or the
best alternatives are materially ambiguous, return review-required rather than
inventing a mainstem.

### 3. Make analyst selection efficient

Present the recommended path over the Hydro DEM with the complete raw network
muted behind it. Highlight candidate heads and junction alternatives. Selecting
another head is sufficient to replace the whole route because the directed tree
already defines its unique downstream path; the analyst should not have to click
dozens of individual one-metre grid segments.

The map must distinguish:

- raw Stream Network;
- recommended or analyst-selected raw path;
- retained NHDPlusV2 reference evidence; and
- the smoothed Flowline preview.

The analyst confirms the selected route and smoothing preview before saving.
Reference alignment is decision support, not automatic acceptance.

### 4. Assemble and orient the raw path

Concatenate the selected Stream Network lines without simplifying their
coordinates. Retain ordered `stream_line_id` lineage. Reverse the assembled line
once so canonical Flowline coordinates begin downstream and end upstream. D8
topology is the primary direction evidence; Hydro DEM endpoint elevations are a
corroborating diagnostic and must report ambiguity rather than overturning known
routing direction.

### 5. Smooth once at Stream scale

Smooth the continuous Stream-level path before dividing it into Reaches. Smoothing
each Reach independently could create boundary kinks or gaps. Preserve the raw
path as evidence and record the algorithm, parameter, unit, package, version,
maximum displacement, length change, and validation result.

PAEK is not available in the portable R workflow, and the existing
`smoothr` methods are not presumed equivalent. The first real-data comparison
should show the owner:

- the unsmoothed D8 path;
- Gaussian kernel smoothing with explicit map-unit bandwidths in the historical
  2–5 unit range; and
- a small bounded Chaikin comparison.

The selected open method must preserve the two Stream endpoints, remain simple,
stay within the accepted channel/Reach corridor, and use an explicit maximum
deviation rule. Exact vertex reproduction of PAEK is not the acceptance goal.
The default method and tolerance remain an owner review decision after the
Spencer maps are available.

### 6. Divide the reviewed path into Reach Flowlines

Use the ordered retained Stream source pieces and their current Reach assignments
to identify Reach transition points. Project each shared reference boundary onto
the raw selected path in downstream-to-upstream order, then transfer the ordered
boundaries to the smoothed Stream path. Split once at each shared boundary so
adjacent Reach Flowlines have exactly the same endpoint.

Do not use overlapping Reach polygon edges as split locations. Reach polygons
are analysis corridors and are allowed to overlap. Refuse noncontiguous Reach
assignments, reversed boundary order, ambiguous projections, gaps, or a Reach
with no nonempty path. Every applicable Reach under the selected event setting
receives exactly one continuous, single-part, downstream-to-upstream candidate.

## Backend and application boundary

`fluvgeo` should own graph validation, route enumeration/ranking evidence, raw
path assembly, smoothing, Reach-boundary projection/splitting, geometry checks,
and portable provenance. The functions must accept ordinary `sf` inputs and have
no Shiny session state.

FG Studio should own exact saved-revision selection, map interaction, analyst
choice, immutable local publication, reopening, working/failure feedback, and
stale-input rejection. This is a new Flowline step after Hydro Modify rather
than another terrain calculation inside stream extraction.

## Local candidate representation

The implementation schema should keep local candidates distinct from governed
FGDB acceptance. One immutable revision needs at least:

- a GeoPackage raw Stream path;
- one GeoPackage Flowline-candidate row per applicable Reach and local event
  setting, with an optional governed Survey Event link only when it exists;
- ordered Stream Network source-segment relationships;
- ordered Reach-boundary evidence;
- method, parameter, displacement, length, direction, software, and reviewer
  provenance; and
- hashes for the exact Stream Network, Hydro DEM, context revision, and Reach
  source-piece evidence.

Changing only the smoothing choice should reuse the reviewed raw path. Changing
the selected head should rebuild the raw path, Reach split, and smoothed preview,
but must not repeat terrain conditioning, direction, accumulation, or thresholding.

## Validation and acceptance evidence

Backend checks must cover:

1. directed acyclic topology, one observed outlet, and unique head-to-outlet paths;
2. deterministic recommendation evidence and explicit ambiguity;
3. lossless ordered source-line membership in the selected raw path;
4. one simple, nonempty, single-part line per applicable Reach/event-setting pair;
5. downstream-to-upstream coordinate order;
6. exact shared endpoints between adjacent Reach Flowlines;
7. Hydro DEM coverage and containment within the reviewed corridor;
8. bounded smoothing displacement and recorded length change;
9. immutable save/reopen and stale-input refusal; and
10. no change to the saved Hydro DEM or Stream Network artifacts.

Whole-app acceptance uses all three Spencer Streams and all eleven current
Reaches. Review maps must make incorrect branch selection, boundary placement,
over-smoothing, and channel departure visible. Small fixtures can verify graph
and geometry invariants, but they do not replace the real-terrain review.

## Deferred from this increment

- Flowline Points, stationing, calibration, or elevation profiles;
- field-surveyed thalweg-to-Flowline production;
- a general-purpose vertex editor;
- governed Dataset Edition acceptance or enterprise/FileGDB loading;
- ArcGIS/QGIS client migration; and
- declaring open smoothing scientifically equivalent to legacy PAEK.
