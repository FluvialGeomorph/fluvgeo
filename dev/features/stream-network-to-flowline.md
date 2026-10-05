# Automatic Flowline derivation from a synthetic Stream Network

- Status: automatic path selection, bounded default smoothing and FG Studio
  review implemented; Reach division and candidate persistence pending
- Updated: 2026-10-05
- Workflow position: after accepted local `stream_network` extraction and before
  Flowline Points
- Governing domain draft:
  [FGDB Flowline feature contract](../../../FGDB/dev/schemas/flowline-feature-contract.md)

## Outcome

Turn one exact saved terrain-derived `stream_network` candidate into an
automatically selected local Flowline candidate for every applicable Reach under
the selected event setting. Stream definition already records the analyst's
intent to analyze the identified Stream. Flowline derivation must use that intent
and the retained NHDPlusV2 evidence to select the Stream's mainstem without a
second manual branch-selection task. A Flowline is the likely reference flow path
through one Reach. It is not asserted to be a wetted path, channel centerline, or
surveyed thalweg.

The first acceptance target is the Spencer Creek Study: three saved Stream
Network candidates and the eleven current Reaches. The app must automatically
select the intended Stream path, let the analyst visually verify raw and smoothed
geometry against the Hydro DEM, save the result, close the app, and reopen the
same candidate. Visual verification does not require the analyst to select or
confirm individual network segments.

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
than producing a Flowline. The longest end-to-end connected route is the useful
mainstem baseline, but length or maximum accumulation alone can select an
adjacent tributary when a named Stream joins a larger or longer channel. The
chosen NHDPlusV2 Stream chain must therefore constrain and disambiguate the
terrain-derived route without supplying its output coordinates.

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

### 2. Select one Stream-level path automatically

Every network head defines one unique route to the observed outlet. Rank those
head-to-outlet routes against the retained NHDPlusV2 Stream chain and current
Reach source pieces over their full length. Treat the selected reference chain
as evidence of the Stream the analyst already defined, not as a replacement
geometry. It may be simplified or out of date relative to LiDAR terrain.

The selection rule is a **reference-constrained longest path**:

1. enumerate each complete terrain-derived head-to-outlet route;
2. use full-route proximity, coverage, endpoint and ordered Reach-source evidence
   to identify the routes consistent with the selected NHDPlusV2 Stream;
3. select the longest connected route among those consistent candidates; and
4. apply deterministic reference-agreement and stable source-cell tie-breakers.

Record every component of the selection score, the selected route's separation
from the runner-up and the ordered source-segment membership. Accumulation can
support the evidence but cannot independently define the answer. No arbitrary
user tuning control or branch-selection interaction belongs in this step. If the
topology is invalid or the retained evidence cannot support one defensible route,
fail with an actionable explanation and direct the analyst back to the owning
Stream definition; do not silently choose a branch or introduce a manual override
that bypasses the recorded Stream intent.

### 3. Make automatic selection reviewable

Present the selected path over the Hydro DEM with the complete raw network muted
behind it. The map is verification evidence, not an editing surface. It must make
an obviously incorrect branch visible, but it must not require the analyst to
click candidate heads, individual segments or a confirmation control before the
derived path can proceed.

The map must distinguish:

- raw Stream Network;
- automatically selected raw path;
- retained NHDPlusV2 reference evidence; and
- the smoothed Flowline preview.

The analyst can inspect the selected route and smoothing preview before saving.
If the route contradicts the intended Stream, the corrective action belongs in
Stream definition or in the reusable deterministic selection rule, not in a
one-off branch choice that cannot be reproduced.

### 4. Assemble and orient the raw path

Concatenate the selected Stream Network lines without simplifying their
coordinates. Retain ordered `stream_line_id` lineage. Reverse the assembled line
once so canonical Flowline coordinates begin downstream and end upstream. D8
topology is the primary direction evidence; Hydro DEM endpoint elevations are a
corroborating diagnostic and must report ambiguity rather than overturning known
routing direction. Supply this single assembled line to the existing
`fluvgeo::flowline()` preparation contract rather than creating a parallel
Flowline representation.

### 5. Smooth once at Stream scale

Smooth the continuous Stream-level path before dividing it into Reaches. Smoothing
each Reach independently could create boundary kinks or gaps. Preserve the raw
path as evidence and record the algorithm, parameter, unit, package, version,
maximum displacement, length change, and validation result.

PAEK is not available in the portable R workflow. The legacy toolbox records a
default PAEK tolerance of 2 map units and describes 2–5 as acceptable. The
portable default therefore applies `smoothr` Gaussian kernel regression with a
2-map-unit bandwidth and no unnecessary densification. This is a behavioral
replacement, not a claim of vertex equivalence with PAEK. FG Studio computes the
four integer candidates in the historical 2–5 map-unit range once, presents 2 as
the conservative default, and lets the analyst switch the displayed Flowline to
a more aggressive candidate without repeating path selection or terrain work.
The raw selected path and every candidate remain in memory as provenance.

`smooth_flowline()` preserves both endpoints, requires a simple valid output,
and rejects a result whose Hausdorff displacement exceeds the bandwidth. On the
three Spencer paths the 2 m default moved the line by at most 0.54–0.57 m and
shortened it by about 5.9–6.2 percent. The 5 m candidates moved the line by at
most 1.05–1.25 m and shortened it by about 9.4–10.6 percent. Each tested result
remained valid and simple; individual candidates completed in 0.14–0.48 seconds.

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

`fluvgeo` should own graph validation, deterministic route selection evidence,
raw path assembly, smoothing, Reach-boundary projection/splitting, geometry
checks, and portable provenance. The functions must accept ordinary `sf` inputs
and have no Shiny session state.

Implement network selection as a separate reusable preprocessor rather than
changing the meaning of `fluvgeo::flowline(flowline, reach_name, dem)`. The
existing function accepts one arbitrary user-drawn line, gives it a Reach name
and orients it from DEM endpoints. `{ohwm2}` depends on that three-argument
behavior. The new preprocessor returns one assembled, downstream-to-upstream `sf`
line plus structured selection evidence; FG Studio passes that line into the
existing `flowline()` workflow. Any later optional extension to `flowline()` must
retain its current signature defaults, return shape, arbitrary-line support and
`{ohwm2}` tests.

FG Studio should own exact saved-revision selection, review-only map display,
immutable local publication, reopening, working/failure feedback, and stale-input
rejection. This is a new Flowline step after Hydro Modify rather than another
terrain calculation inside stream extraction.

## Local candidate representation

The implementation schema should keep local candidates distinct from governed
FGDB acceptance. One immutable revision needs at least:

- a GeoPackage raw Stream path;
- one GeoPackage Flowline-candidate row per applicable Reach and local event
  setting, with an optional governed Survey Event link only when it exists;
- ordered Stream Network source-segment relationships;
- ordered Reach-boundary evidence;
- selection scores, selected-route margin, method, parameter, displacement,
  length, direction and software provenance; and
- hashes for the exact Stream Network, Hydro DEM, context revision, and Reach
  source-piece evidence.

Changing only the smoothing choice should reuse the selected raw path. Changing
the Stream definition, reference evidence or network revision must rebuild the
automatic selection, Reach split and smoothed preview, but must not repeat terrain
conditioning, direction, accumulation or thresholding when the saved network
itself is unchanged.

## Validation and acceptance evidence

Backend checks must cover:

1. directed acyclic topology, one observed outlet, and unique head-to-outlet paths;
2. deterministic reference-constrained longest-path selection, score evidence,
   tie-breaking and explicit unsupported/ambiguous failure;
3. lossless ordered source-line membership in the selected raw path;
4. one simple, nonempty, single-part line per applicable Reach/event-setting pair;
5. downstream-to-upstream coordinate order;
6. exact shared endpoints between adjacent Reach Flowlines;
7. Hydro DEM coverage and containment within the reviewed corridor;
8. bounded smoothing displacement and recorded length change;
9. immutable save/reopen and stale-input refusal;
10. no change to the saved Hydro DEM or Stream Network artifacts; and
11. the existing arbitrary drawn-line `flowline()` behavior and `{ohwm2}` call
    remain compatible.

Whole-app acceptance uses all three Spencer Streams and all eleven current
Reaches. Each result must be repeatable without segment-selection input. Review
maps must make incorrect branch selection, boundary placement, over-smoothing,
and channel departure visible. Small fixtures can verify graph and geometry
invariants, but they do not replace the real-terrain review.

### Implemented selection and smoothing evidence

`select_stream_mainstem()` now validates the directed one-outlet tree, enumerates
complete paths, calculates full-route discrete Hausdorff distance to the saved
reference, selects the longest route among equivalent best matches, preserves
ordered source segments and returns canonical downstream-to-upstream linework.
`smooth_flowline()` then produces bounded candidates at 2, 3, 4 and 5 map units.
FG Studio presents 2 as the conservative default and permits an immediate switch
among those candidates over the viewport-stretched Hydro DEM, muted source
network and retained NHDPlusV2 reference. The raw selected path and all four
candidates are retained as transient provenance. This result is review-only; it
does not claim that the chosen candidate is persisted or that Reach splitting or
immutable-candidate requirements are complete.

On the saved Spencer candidates, the selector evaluated 15 mainstem, 16 east
tributary and 15 west tributary routes in 1.38, 0.44 and 0.31 seconds. The selected
route was also the longest complete path in every case. Mainstem and east had
447.8 m and 475.9 m separation from the next reference match. West had equal
best reference distance for two routes, so the specified longest-route rule
selected the longer one. The selected raw lengths are 23,421.76 m, 9,477.89 m
and 7,833.08 m respectively. These are current real-data review results, not
general qualification across terrain forms.

## Deferred from this increment

- Flowline Points, stationing, calibration, or elevation profiles;
- field-surveyed thalweg-to-Flowline production;
- a general-purpose vertex editor;
- governed Dataset Edition acceptance or enterprise/FileGDB loading;
- ArcGIS/QGIS client migration; and
- declaring open smoothing scientifically equivalent to legacy PAEK.
