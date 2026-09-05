# ADR-0001: Adopt sfnetworks and hydroloom for network processing

- Status: accepted by the user
- Date: 2026-09-05

## Context

Raw extracted or retained stream features are not necessarily logical network
links. Short pieces can occupy one DEM cell, making endpoint direction
indeterminate even when their continuous parent link has measurable relief.
The user accepted the mainstem, protected-boundary, branched/divergent-network,
and clipped-DEM experiments as sufficient evidence to proceed.

## Decision

Use **sfnetworks** (with tidygraph/igraph) for spatial network structure and
topological transformations, and **hydroloom** for hydrologic connectivity,
ordering, navigation, and applicable hydrologic calculations. Keep thin,
reusable adapters in fluvgeo. Do not duplicate these algorithms in FGDB.

FGDB retains governed identities, observation/configuration semantics, source
lineage, review state, and persistence. Positional graph indices are not UUIDs.
Use sf geometry as the interchange layer; adapt representations explicitly.
Hydroloom relationship tables can repeat feature IDs at diversions and are
not interchangeable with unique spatial feature tables. Reverse a copy when
downstream-oriented calculations consume FGDB's downstream-to-upstream storage
geometry. HY_Features alignment is conceptual; comprehensive OGC conformance
has not been established.

The first production slice is explicit logical-link construction before DEM
orientation, not a short-length cutoff. Join only degree-two exact-endpoint
continuations with matching declared Stream/Reach identity. Preserve protected
endpoints and suspicious geometry. The compatible preparation default remains
raw segmentation; callers opt in with `consolidate = TRUE`.

## Consequences

- sfnetworks, tidygraph, and igraph become explicit runtime dependencies now.
  Hydroloom adoption is decided; its production direction/connectivity adapter
  is a subsequent slice, not implemented by this change.
- Every normalized source-part relationship survives consolidation. Merged
  links receive new candidate UUIDs; multi-source operations reference the link
  rather than arbitrarily selecting one source.
- This does not infer undeclared boundaries, snap gaps, resolve crossings,
  establish downhill validity throughout a link, or accept a network. Cyclic
  edges are conservatively left unmerged, including legitimate diversions.
- Original DEM archive recovery, automated clipping policy, and DEM extraction
  tools (including rivnet) remain deferred. The clipped raster experiment is
  a controlled fixture, not evidence of original derivation provenance.

Evidence: [interoperability experiment](../features/network-processing-experiment.md)
and [clipped-DEM experiment](../features/clipped-dem-network-experiment.md).
