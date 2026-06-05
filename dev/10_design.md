# Current design and architecture

This document records the current stable architecture, operating assumptions, and design boundaries for `fluvgeo`.

## Repository structure
- Package code: `R/`
- Documentation: `man/`
- Tests: `tests/`
- Vignettes: `vignettes/`
- Data: `data/`
- Development instructions: `dev/instructions/`
- Development plan: `dev/05_plan.md`
- Schemas: `dev/40_schemas.md`
- Decisions: `dev/decisions/`
- Sessions: `dev/sessions/`

## Open questions
- [ ] Add unresolved design questions here

## Architecture history and current transition state
`fluvgeo` was originally developed as part of a hybrid ESRI + R architecture for fluvial geomorphology analysis.

### Original architecture
- Proprietary ArcGIS/ESRI Python tooling in `FluvialGeomorph-toolbox` was used for the early geospatial processing stages.
- `fluvgeo` handled later-stage analysis, reporting, and data science functionality in R.
- This division reflected the available tooling at the time and the project's dependence on ESRI capabilities for key geospatial operations.

### Current direction
As the open-source geospatial ecosystem has matured, some geospatial processing capabilities have been reimplemented in `fluvgeo` using open-source tools.

The package is therefore in a transition state:
- it is not yet fully standalone for all workflows
- some workflows still depend on proprietary upstream geospatial input generation
- some geospatial functionality now exists inside `fluvgeo`, but coverage is partial
- the long-term direction is to prefer open-source implementations where they are viable and scientifically appropriate

The package has stable current behavior, but its geospatial input boundary is still evolving as open-source implementations replace proprietary steps where feasible.

### Design implications
This transition affects how the package should evolve:
- preserve compatibility with downstream consumers that still depend on upstream proprietary geospatial inputs
- avoid assuming that all geospatial feature derivation is already implemented inside the R package
- prefer open-source solutions when they meet technical and scientific requirements
- treat the package as a shared backend that may support multiple front ends and evolving geospatial input strategies
- keep analysis logic reproducible, testable, and documented so that future replacement of proprietary steps remains feasible

### Dependency boundary
`fluvgeo` remains dependent on externally produced geospatial inputs for some workflows. That means:
- the package should be designed with clear expectations about required input artifacts
- future work should explicitly identify which geospatial derivation steps are internal to `fluvgeo` and which remain upstream
- architectural changes should be evaluated for their effect on portability, adoption, and licensing burden

### Strategic principle
When multiple approaches are acceptable, prefer open-source solutions over proprietary ones if they:
- satisfy the scientific and operational requirements
- reduce licensing constraints
- improve portability and adoption
- preserve reproducibility and maintenance viability
