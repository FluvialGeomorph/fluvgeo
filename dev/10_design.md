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



## Unit architecture and display-unit API

`fluvgeo` treats unit handling as a layered architectural concern with three distinct systems:

### 1. Geospatial data unit system
This layer represents the units, datums, and coordinate reference properties of incoming geospatial data. The package must tolerate heterogeneous spatial inputs across space and time, including mixed coordinate systems and mixed vertical datums. `fluvgeo` does not assume that all source data can or should be reduced to one standard coordinate system.

### 2. Analysis unit system
This layer represents the units required by the scientific formulas implemented in the package. Fluvial geomorphology methods are derived from literature spanning many regions and time periods, and those methods are often defined in native unit conventions. Analysis functions should implement formulas in the units and functional form required by the source method, converting inputs only as needed to evaluate the formula correctly. Analysis code is responsible for scientific correctness, not display formatting.

### 3. Display unit system
This layer represents the user-facing unit system used in plots, tables, captions, legends, reports, and composed figure layouts. Display units must be selectable and may differ from both the geospatial input units and the native analysis units. Output functions should derive all rendered unit labels and display conversions from a single display-unit specification.

### Separation principle
These three systems are intentionally independent.

- Geospatial input assumptions must not leak into display formatting.
- Display preferences must not alter the scientific definition of analysis functions.
- Analysis functions must not be responsible for presentation-layer decisions.

### Open-source spatial ecosystem alignment
`fluvgeo` is built on the open-source geospatial stack, including `sf`, `terra`, and `stars`, and should embrace that ecosystem’s mature unit infrastructure, including the `units` package, for forward compatibility and reproducibility.

This means:

- package-facing functions should continue to expose a simple `unit_system` contract;
- internal implementations should prefer ecosystem-native unit metadata and conversions where practical;
- explicit unit objects from `units` should be used where they improve clarity, interoperability, or future compatibility;
- ad hoc hard-coded conversion logic should be avoided when a maintained ecosystem mechanism exists.

### Public display-unit API
The public display-unit interface should use a single parameter:

- `unit_system`: one of `"USCS"` or `"SI"`

This parameter controls how user-facing output is rendered, including:
- axis labels
- legend labels
- figure captions
- report narrative text
- table headings
- composed figure labels and annotations
- other display-only unit strings

The default display system should preserve current behavior unless explicitly changed by the caller.

### Internal display specification
Implementation should resolve `unit_system` into an internal display specification object or list that centralizes:
- length unit names and abbreviations
- area unit names and abbreviations
- elevation unit names and abbreviations
- unit-bearing label templates
- conversion factors used only for display
- ecosystem-aligned unit metadata
- any other text fragments needed for plots, composed figures, and reports

Output functions should use this shared display specification rather than hard-coded unit strings.

### Analysis boundary
Analysis functions may perform local conversions required by formulas, but those conversions must remain internal and testable. If a reference method requires native units or a specific functional form, the implementation should preserve that method’s scientific meaning and only convert at clearly defined boundaries.

### Migration from legacy `profile_units`
Existing functions that currently accept `profile_units` are part of the legacy display interface and should be migrated to `unit_system` as the primary contract. During transition, `profile_units` may be mapped internally to the new display specification where needed, but new code should prefer `unit_system`.

### Implementation implications
- Unit-aware output functions should accept an explicit display-unit parameter.
- Unit labels, axis titles, captions, and legend text should be generated from shared helpers rather than hard-coded strings.
- Reports, plots, and figure-composition helpers should use the display unit system consistently across all figures, tables, and narrative text.
- The same report should render correctly in both USCS and SI modes without changing the underlying analysis results.


### Unit metadata and rendering layer

In addition to the `units` package as the semantic backend, `fluvgeo` should maintain a lightweight structured metadata layer for display-oriented unit handling. This layer is not a replacement for `units`; it is a package-specific rendering contract that sits above `units` and standardizes how unit-bearing quantities are described and displayed across plots, reports, tables, and maps.

#### Purpose
The metadata layer exists to answer display questions that `units` does not fully solve on its own:

- What kind of quantity is this?
- What is the canonical base unit symbol?
- Does the quantity use a power, compound denominator, or ratio form?
- How should the quantity be rendered in plain text, plotmath, unicode, and prose?
- Which render style should be used in each output context?

#### Relationship to `units`
- `units` remains the authoritative semantic unit backend.
- `fluvgeo` metadata defines the rendering and formatting contract.
- The metadata layer may refer to `units` objects or unit strings, but it must not replace `units` for dimensional correctness or conversion logic.

#### Metadata responsibilities
The metadata layer should support, at minimum:

- quantity classification
- base unit symbol selection
- exponent handling
- compound unit description
- “per” relationships
- label templates for:
  - plain text
  - plotmath
  - unicode
  - prose

#### Rendering targets
The metadata layer should support multiple rendering targets because different output systems have different needs:

- **plain text**: for file outputs, logs, and simple labels
- **plotmath**: for `ggplot2` axis titles and annotations
- **unicode**: for human-readable report text and table labels
- **prose**: for narrative report language

#### Design principle
The rendering layer should be derived from a single structured specification so that unit naming is consistent across all outputs. A quantity should never have to be re-described independently in multiple plotting or reporting functions.

#### Example conceptual structure
A display-oriented quantity specification may include:

- quantity kind, such as distance, area, elevation, or slope
- system, such as `USCS` or `SI`
- semantic unit reference, preferably compatible with `units`
- renderable forms:
  - `plain`
  - `plotmath`
  - `unicode`
  - `prose`

#### Implementation implication
Output functions should request unit renderings from helper functions rather than constructing labels manually. For example, a plot function should ask the helper layer for the correct x-axis label or unit symbol instead of hard-coding `ft^2`, `m^2`, or prose phrases in the plotting code.

#### Forward compatibility
This layer should be designed so additional render targets can be added later without changing the output-function contract. For example, if a later workflow needs LaTeX, markdown, or HTML-safe unit rendering, that should be added as a new render target rather than by reworking every plotting function.



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
