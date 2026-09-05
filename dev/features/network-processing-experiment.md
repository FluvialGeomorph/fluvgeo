# Spatial and hydrologic network interoperability experiment

- Date: 2026-09-05
- Status: experiment passed; production integration not implemented
- Scope: `sfnetworks` cleaning, source lineage, `hydroloom` connectivity, and
  the FGDB coordinate-order convention. DEM derivation remains deferred.
- Reproduce from `fluvgeo`: `Rscript --vanilla dev/scripts/network-interop-experiment.R`
  (resolve the workstation executable using `.agents/workstation.md`).

## Verified results

| Input | Source pieces | Logical links | Retained source records | Inter-link connections |
| --- | ---: | ---: | ---: | ---: |
| Sinsinawa pruned mainstem | 99 | 1 | 99 | 0 |
| Same mainstem, one protected intermediate node | 99 | 2 | 99 | 1 |
| New Hope, including branches and diversions | 746 | 643 | 746 | 728 |

All three results have one outlet in their computational orientation. Total
length differences are below 5e-12 m; geometric coverage is equal. The script
passes 59 checks, including original source coordinate sequences, complete
one-time source membership, CRS, valid geometry, connected components,
junction/terminal degree signatures, boundary classification, repeat-run edge
counts, downstream navigation coverage, and conversion to/from upstream storage
coordinate order. No snapping or gap repair is performed.

For New Hope, all 728 rebuilt inter-link connections agree both with the
cleaned spatial graph and with the fixture's original hydrologic connections
mapped through source membership. Secondary paths were not discarded. The
returned connection table has 729 rows including its outlet record, but only
643 unique link IDs. This is expected for a non-dendritic relationship table.

## Inputs and evidence limits

- Sinsinawa comes directly from sibling `fluvgeodata/inst/extdata/testing_data.gdb`,
  layer `stream_network`. The user confirmed it was pruned to retain the
  mainstem. Its endpoint geometry is one chain, despite inconsistent legacy
  node identifiers and mixed coordinate order.
- New Hope is the real `extdata/new_hope.gpkg` example distributed with
  `hydroloom` 1.2.1. Its source relationships supply independent connection
  evidence; it is not a FluvialGeomorph-produced tributary fixture.
- The protected Sinsinawa node is deliberately selected at the end of source
  `arcid` 1126 to exercise the mechanism. It is not an asserted Reach boundary.
- The mainstem is oriented toward an arbitrary terminal for the adapter test.
  Neither actual downstream direction nor resolution of equal DEM elevations
  is claimed. New Hope uses its distributed downstream-oriented geometry.
- Versions: sfnetworks 0.6.6, hydroloom 1.2.1, sf 1.1.2, tidygraph 1.3.1,
  igraph 2.3.3. Runtime was about six seconds locally; this is not a scaling
  benchmark. R printed pre-existing locale startup warnings.

## Integration lessons

1. `sfnetworks::to_spatial_smooth()` consolidates incidental segmentation while
   preserving connectivity. This operation concatenates geometry, not
   cartographic smoothing. Use `protect` for explicit node boundaries and
   `require_equal` for selected feature classifications. New Hope preserves
   StreamRiver/Connector/ArtificialPath transitions in this experiment.
2. Preserve membership explicitly. `store_original_data = TRUE` retains the
   source records and geometries in `.orig_data`; the experiment extracts a
   source-to-logical-link table. Attribute aggregation must also be explicit:
   the default ignores merged attributes. Here the equal boundary class is
   retained with `summarise_attributes`; obsolete source graph indices are not.
3. Feature identities and graph positions are distinct. The `from`/`to` values
   in sfnetworks are node positions. This experiment generates temporary link
   and node labels, not persistent scientific UUIDs. Production must maintain
   an explicit mapping and an identity policy for merged/split outputs.
4. After cleaning, reconstruct relationships. The experiment builds a spatial
   `hy_node` view and uses `make_attribute_topology(..., min_distance = 0)` to
   recover exact connections. Keep the feature table separate from the
   potentially repeated-ID connection table. Both sorting and downstream
   navigation work on the rebuilt relationships.
5. Reverse a computational/storage copy explicitly when moving between
   downstream-oriented computation and FGDB's upstream coordinate order.
   Do not assume undirected graph indices describe geometry coordinate order.

## Proposed next implementation

Follow-up: [the clipped-DEM experiment](clipped-dem-network-experiment.md) now
extends the Sinsinawa case through actual DEM orientation and preparation.
It resolves direction for one logical link built from 51 clipped pieces;
production node/role and original-source-lineage integration remain outstanding.

Use these packages behind a reusable `fluvgeo` logical-network operation, with
source membership and protected-boundary inputs. Separate geometric
consolidation from scientific flow-direction assignment and persistence.
Translate existing FGDB identities/provenance rather than introducing another
independent network engine. No package imports, exported functions, accepted
schema, or client behavior changed in this experiment.

Still untested: noisy gaps/crossings, true multipart normalization during this
round trip, persistent UUID assignment, post-cleaning stream-order/levelpath
recalculation, a FluvialGeomorph-produced branched fixture, large-network
performance, and a scientifically anchored mainstem direction method. Those
are not prerequisites for the interoperability conclusion, but are not solved
by these results.

## Package references

- [sfnetworks spatial morphers](https://luukvdmeer.github.io/sfnetworks/reference/spatial_morphers.html)
- [Hydroloom network representations](https://doi-usgs.github.io/hydroloom/articles/hydroloom.html)
- [Hydroloom non-dendritic networks](https://doi-usgs.github.io/hydroloom/articles/non-dendritic.html)

Installed package behavior and the executable experiment, not website examples,
are the evidence for the numeric results above.
