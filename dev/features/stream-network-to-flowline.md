# Automatic Flowline derivation from a synthetic Stream Network

- Status: local Flowline creation implemented: automatic path selection,
  selectable bounded smoothing, deterministic Reach division, compatible
  `flowline()` preparation and immutable FG Studio save/reopen
- Updated: 2026-10-06
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

## Implemented derivation

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
- retained NHDPlusV2 reference evidence; and
- the selected smoothed Flowline candidate.

The automatically selected raw path remains retained as provenance and as the
stable source for every smoothing candidate; it need not be a separate competing
map overlay when the complete raw network is already visible.

The analyst can inspect the selected route, Reach divisions and smoothing preview,
then save the chosen candidate. If the route contradicts the intended Stream, the corrective action belongs in
Stream definition or in the reusable deterministic selection rule, not in a
one-off branch choice that cannot be reproduced.

### 4. Assemble and orient the raw path

Concatenate the selected Stream Network lines without simplifying their
coordinates. Retain ordered `stream_line_id` lineage. Reverse the assembled line
once so canonical Flowline coordinates begin downstream and end upstream. D8
topology is the primary direction evidence; Hydro DEM endpoint elevations are a
corroborating diagnostic and must report ambiguity rather than overturning known
routing direction. Retain this single assembled Stream-scale line as the common
source for smoothing and Reach division. After division, supply each Reach-owned
line to the existing `fluvgeo::flowline()` preparation contract with its Reach
name and applicable Hydro DEM rather than creating a parallel Flowline
representation.

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
The raw selected path and every candidate remain in session memory for review.
The application persists the raw path and only the chosen smoothed candidate.

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

`fluvgeo` owns graph validation, deterministic route selection evidence,
raw path assembly, smoothing, Reach-boundary projection/splitting, geometry
checks, and portable provenance. The functions must accept ordinary `sf` inputs
and have no Shiny session state.

Implement network selection as a separate reusable preprocessor rather than
changing the meaning of `fluvgeo::flowline(flowline, reach_name, dem)`. The
existing function accepts one arbitrary user-drawn line, gives it a Reach name
and orients it from DEM endpoints. `{ohwm2}` depends on that three-argument
behavior. The preprocessor currently returns one assembled,
downstream-to-upstream Stream-scale `sf` line plus structured selection evidence.
FG Studio passes the chosen smoothed candidate, transformed retained reference,
current mappings and Reach inventory to `derive_reach_flowlines()`. The backend
splits at ordered Reach boundaries and passes each line through `flowline()` with
its Reach name and `direction="preserve"`. This optional direction argument retains
the historical DEM-oriented default, return shape, arbitrary-line support and
`{ohwm2}` behavior.

FG Studio owns exact saved-revision selection, review map display,
immutable local publication, reopening, working/failure feedback, and stale-input
rejection. This is a new Flowline step after Hydro Modify rather than another
terrain calculation inside stream extraction.

## Local candidate representation

The implemented FG Studio schema keeps local candidates distinct from governed
FGDB acceptance. One immutable revision contains `flowlines.gpkg`, `result.rds`
and `provenance.json`. The GeoPackage preserves the selected raw path, chosen
smoothed Stream path, one Flowline per applicable Reach, selected source segments
and shared boundaries when present. Feature attributes retain the backend's
selection, smoothing, length, displacement and direction evidence. The index and
JSON record the chosen bandwidth, Reach IDs, time, GeoPackage hash and hashes for
the exact Study context, local event setting, Hydro output, Stream Network,
retained reference and Reach mapping. A completion marker prevents partial
directories from being reopened.

Alternative route scores and the three unchosen smoothing candidates remain
transient review results; the local store does not claim a complete governed
method execution record. It also does not invent `flowline_id`, Reach-owned
`survey_event_id`, Dataset Edition or acceptance rows. Those identities and
additional software/review provenance belong to the later FGDB publication
contract.

Changing only the smoothing choice should reuse the selected raw path. Changing
the Stream definition, reference evidence or network revision must rebuild the
automatic selection, Reach split and smoothed preview, but must not repeat terrain
conditioning, direction, accumulation or thresholding when the saved network
itself is unchanged.

## Validation and acceptance evidence

Implemented backend and local-store checks cover:

1. directed acyclic topology, one observed outlet, and unique head-to-outlet paths;
2. deterministic reference-constrained longest-path selection, score evidence,
   tie-breaking and explicit unsupported/ambiguous failure;
3. lossless ordered source-line membership in the selected raw path;
4. one simple, nonempty, single-part line per applicable Reach/event-setting pair;
5. downstream-to-upstream coordinate order;
6. exact shared endpoints between adjacent Reach Flowlines;
7. matching projected CRS for Reach preparation;
8. bounded smoothing displacement and recorded length change;
9. immutable save/reopen, file-integrity checks and stale-input refusal;
10. no change to the saved Hydro DEM or Stream Network artifacts; and
11. the existing arbitrary drawn-line `flowline()` behavior and `{ohwm2}` call
    remain compatible.

Hydro DEM coverage, channel-corridor containment, governed identity, complete
software/method provenance and analyst acceptance remain FGDB qualification
requirements. The local candidate does not yet assert that those publication
checks have passed.

Whole-app acceptance uses all three Spencer Streams and all eleven current
Reaches. Each result must be repeatable without segment-selection input. Review
maps must make incorrect branch selection, boundary placement, over-smoothing,
and channel departure visible. Small fixtures can verify graph and geometry
invariants, but they do not replace the real-terrain review.

### Implemented selection, division and persistence evidence

`select_stream_mainstem()` now validates the directed one-outlet tree, enumerates
complete paths, calculates full-route discrete Hausdorff distance to the saved
reference, selects the longest route among equivalent best matches, preserves
ordered source segments and returns canonical downstream-to-upstream linework.
`smooth_flowline()` then produces bounded candidates at 2, 3, 4 and 5 map units.
FG Studio presents 2 as the conservative default and permits an immediate switch
among those candidates over the viewport-stretched Hydro DEM, muted source
network and retained NHDPlusV2 reference. `derive_reach_flowlines()` divides each
preview candidate from ordered retained Reach-source transitions, refusing
missing, duplicated, noncontiguous, gapped, reversed or ambiguous evidence.
`study_flowline_store()` publishes the chosen raw path, smoothed Stream path,
Reach lines, shared boundaries and selected network segments in one immutable
GeoPackage with RDS/JSON provenance and exact-input fingerprints.

On the saved Spencer candidates, the selector evaluated 15 mainstem, 16 east
tributary and 15 west tributary routes in 1.38, 0.44 and 0.31 seconds. The selected
route was also the longest complete path in every case. Mainstem and east had
447.8 m and 475.9 m separation from the next reference match. West had equal
best reference distance for two routes, so the specified longest-route rule
selected the longer one. The selected raw lengths are 23,421.76 m, 9,477.89 m
and 7,833.08 m respectively. These are current real-data review results, not
general qualification across terrain forms.

## Completed local gate before Flowline Points

The local Flowline product now:

1. projects ordered retained Reach-source transitions onto the chosen smoothed
   Stream path and splits it into exactly one candidate per applicable Reach;
2. prepares each Reach line through the compatible `fluvgeo::flowline()` contract,
   retaining downstream-to-upstream topology as primary direction evidence;
3. saves an immutable local revision containing the raw path, chosen smoothing
   parameter, Reach candidates, source-segment/boundary evidence and exact input
   fingerprints;
4. reopens the same revision after an app restart and refuses it as current when
   its Stream definition, Hydro DEM, network, Reach assignment or event setting
   has changed; and
5. presents the saved Reach candidates in the normal Flowline review.

On current Spencer inputs this yields five mainstem, three east-tributary and
three west-tributary Flowlines, with exact shared endpoints. Focused tests cover
division failure modes, historical `flowline()` compatibility, local immutable
save/reopen and stale mapping refusal.

Enterprise/FileGDB acceptance can remain deferred. Flowline Points may consume
the completed local candidates, but must not depend on transient session geometry.

## Deferred from this increment

- Flowline Points, stationing, calibration, or elevation profiles;
- field-surveyed thalweg-to-Flowline production;
- a general-purpose vertex editor;
- governed Dataset Edition acceptance or enterprise/FileGDB loading;
- ArcGIS/QGIS client migration; and
- declaring open smoothing scientifically equivalent to legacy PAEK.
