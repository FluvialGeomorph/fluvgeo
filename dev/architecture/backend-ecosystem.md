# Backend ecosystem

## Purpose

`fluvgeo` is the shared R backend for repeatable fluvial geomorphic analysis
from remotely sensed terrain data, especially lidar. It centralizes reusable
scientific calculations, spatial data handling, analysis conventions, and
reporting components so client applications do not duplicate core methods.

Reports are scientific deliverables that explain study definition, data
development and results, not merely client diagnostics. The accepted
[reporting intent](../goals/reporting-intent.md) extends that record above Reach
level and separates the thorough desktop artifact from selective Shiny interaction.
The third reporting job is forensic reconstruction of archived analyses. The
accepted [GeoPackage storage direction](../../../FGDB/dev/decisions/adr-0024-geopackage-local-standard-and-archive-reconstruction.md)
applies to local project spatial data and related tables; legacy File GDB remains
a source-read boundary while migration profiles are qualified.
The later [folder/GeoTIFF decision](../decisions/ADR-0002-folder-deliverables-and-geotiff-terrain.md)
clarifies that GeoPackages carry vectors/tables, not the migration's analytical
raster payloads. Reach–Survey–Event folders keep external GeoTIFFs and their
metadata/identity links together; qualified Esri terrain exchange is not assumed.

## Clients and collaborators

Changes may affect:

- `FluvialGeomorph-toolbox`, which supports ArcGIS Pro desktop workflows;
- `fg-qgis-toolbox`, the separate developing open-source desktop client;
- `ohwm2`, which provides an interactive Shiny application;
- `RegionalCurve`, which supplies regional hydraulic geometry operations; and
- `fluvgeodata`, which supplies test and example data used by this package.

Treat the package API and its documented data structures as contracts shared
with these repositories. A change initiated for one client should remain
appropriately general before it is added to `fluvgeo`.

## Responsibility boundary

Classify work before changing code:

1. **Upstream geospatial input generation** produces source features or rasters
   that `fluvgeo` consumes. Some of this work remains in ArcGIS tooling.
2. **Core analysis** belongs in `fluvgeo` when it is reusable scientific,
   spatial, validation, or reporting logic.
3. **Client orchestration and presentation** belongs in the consuming desktop
   or web application when it concerns workflow state, navigation, interaction,
   or client-specific presentation.

Do not duplicate core calculations in a client to avoid defining an explicit
interface. Do not move client-specific orchestration into the backend merely
because the client imports this package.

## Architecture transition

The accepted [parallel QGIS migration decision](../../../fg-qgis-toolbox/dev/decisions/ADR-0001-parallel-open-source-migration.md)
makes fg-qgis-toolbox the desktop development/test path while the ArcGIS Pro
toolbox continues in production. Tool divisions may change; scientific methods
remain reusable in fluvgeo for QGIS and Shiny. Separate repositories do not by
themselves isolate shared R libraries: identify development runtimes and backend
versions, and promote changes to production clients only after relevant review.
QGIS-specific interaction must not become a prerequisite for Shiny capabilities.

The ecosystem began as a hybrid ESRI and R architecture. Open-source
geospatial capabilities now implement some operations previously performed
upstream, but coverage remains partial. Therefore:

- preserve compatibility with currently supported ArcGIS-derived inputs;
- state required input artifacts and coordinate/spatial assumptions explicitly;
- distinguish capabilities implemented in R from those still supplied upstream;
- prefer open-source implementations when they satisfy scientific and
  operational requirements;
- evaluate replacements for scientific validity, portability, licensing,
  reproducibility, performance, and maintenance cost.

The package must not be described as fully standalone until repository evidence
supports that claim.

## Scientific and compatibility invariants

- Methods and derived metrics must be scientifically defensible, repeatable,
  documented, and testable.
- Input provenance, units, coordinate reference systems, and structural
  assumptions must remain explicit.
- User-visible behavior and documentation must agree with implementation.
- Exported API changes require downstream impact assessment.
- Breaking changes require a deliberate decision, migration guidance, tests,
  documentation, and release notes.
- Prefer the smallest change that keeps the shared backend clear and reusable.
- Remote enrichment services must expose explicit failure behavior. A client
  that does not require an enrichment value may opt out while retaining the
  documented output field as typed missing data.
- Use `hydrogeofetch` as the supported R access package for USGS hydrologic
  geospatial services and Geoconnex reference features. Keep NLDI processing
  operations distinct from Geoconnex reference-feature discovery in APIs,
  provenance, and failure handling.

## Related context

Network processing uses sfnetworks/tidygraph/igraph for spatial topology and
hydroloom for non-dendritic connectivity and hydrologic ordering. fluvgeo owns reusable
methods; FGDB owns persistence and governed relational meaning. See
[ADR-0001](../decisions/ADR-0001-network-processing-libraries.md).

- Current implementation architecture: `dev/architecture/design.md`
- Structural contracts: `dev/schemas/project-schemas.md`
- Change procedure: `dev/workflows/backend-change-assessment.md`
- Package procedure: `dev/workflows/r-package-development.md`
