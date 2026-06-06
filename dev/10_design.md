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


***


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


***


### Unit metadata and rendering layer

In addition to the `units` package as the semantic backend, `fluvgeo` should maintain a lightweight structured metadata layer for display-oriented unit handling. This layer is not a replacement for `units`; it is a package-specific rendering contract that sits above `units` and standardizes how unit-bearing quantities are described and displayed across plots, reports, tables, maps, static documents, and interactive applications.

#### Purpose
The metadata layer exists to answer display questions that `units` does not fully solve on its own:

- What kind of quantity is this?
- What is the canonical base unit symbol?
- Does the quantity use a power, compound denominator, or ratio form?
- How should the quantity be rendered in plain text, plotmath, unicode, LaTeX, MathJax, and prose?
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
  - LaTeX
  - MathJax

#### Rendering targets
The metadata layer should support multiple rendering targets because different output systems have different needs:

- **plain text**: for file outputs, logs, and simple labels
- **plotmath**: for `ggplot2` axis titles and annotations
- **unicode**: for human-readable report text and table labels
- **prose**: for narrative report language
- **latex**: for static document generation
- **MathJax**: for interactive HTML, Shiny apps, and browser-rendered formula display

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
  - `latex`
  - `mathjax`

#### Implementation implication
Output functions should request unit renderings from helper functions rather than constructing labels manually. For example, a plot function should ask the helper layer for the correct x-axis label, unit symbol, or rendered formula instead of hard-coding `ft^2`, `m^2`, `\mathrm{ft}^{2}`, or prose phrases in the plotting code.

#### Forward compatibility
This layer should be designed so additional render targets can be added later without changing the output-function contract. For example, if a later workflow needs markdown-safe rendering, HTML-safe rendering, or a specialized renderer for a new plotting framework, that should be added as a new render target rather than by reworking every plotting function.


***


## Ecosystem review checklist: `units` + tidyverse + geospatial workflows

Before defining the computation interface ADR, review established patterns for using `units` in scientific and geospatial workflows built on the tidyverse ecosystem.

### Review goals
- Identify common ways that `units` objects are introduced, preserved, transformed, and dropped in pipelines.
- Determine where unit conversion is typically performed: at input boundaries, inside verbs, or at output boundaries.
- Understand how `sf`, `terra`, and `stars` interact with unit-bearing values.
- Capture patterns that support robust, readable, and maintainable analysis code.
- Avoid inventing a computation contract that conflicts with common ecosystem practice.

### Questions to answer
1. **Public function boundaries**
   - Do well-designed packages accept `units` objects directly?
   - Do they also accept bare numeric values with explicit unit metadata?
   - Which pattern is most common for public-facing scientific functions?

2. **Pipeline behavior**
   - How do `units` objects behave in `dplyr::mutate()`, `summarise()`, `across()`, and grouped operations?
   - Are unit-bearing columns preserved through common transformations?
   - What operations tend to fail or coerce unexpectedly?

3. **Conversion placement**
   - Where are conversions performed in practice?
   - Are values normalized to method-native units before calculation?
   - Are outputs converted to display units only at the end?
   - Are there cases where calculations are intentionally performed in a canonical system throughout?

4. **Spatial object integration**
   - How do `sf` objects retain and propagate units on geometry-derived values?
   - How do `terra` and `stars` manage unit metadata, if at all?
   - When working across `sf`/`terra`/`stars`, what is the safest point to attach or preserve units?

5. **Summary and aggregation**
   - How do unit-bearing values behave in grouped summaries?
   - What happens to units during `mean()`, `sum()`, `min()`, `max()`, and derived metrics?
   - Are there package conventions for preserving units through aggregation?

6. **Representation and display**
   - When do packages render units directly versus using a display layer?
   - What is the balance between `units` objects, plain strings, and human-readable labels?
   - How do plotting packages like `ggplot2` handle unit-bearing aesthetics and labels?

7. **Validation and coercion**
   - How strictly do packages validate unit compatibility?
   - Do they error early on incompatible units?
   - Do they silently convert compatible units?
   - How do they handle missing or unknown units?

8. **Implementation ergonomics**
   - Which approach is easiest to maintain in a pipeline-heavy package?
   - Which patterns minimize repeated conversion logic?
   - Which patterns are most likely to remain compatible with future geospatial tooling?

### Research sources to inspect
- `units` package documentation and vignettes
- `sf` documentation and examples involving `units`
- `terra` documentation and examples involving measurement values
- `stars` documentation and examples involving metadata and derived quantities
- tidyverse examples from scientific or spatial packages using `units`
- package code that uses `units` in `dplyr` pipelines

### Deliverable
Summarize:
- the dominant ecosystem pattern
- recommended boundary behavior
- recommended internal representation
- likely risks or friction points
- implications for a future computation interface ADR

### Decision rule
Do not finalize the computation interface ADR until this review has identified the most practical and ecosystem-aligned usage pattern.


***


## Computation interface design questions

Before continuing further with display-layer refactoring, `fluvgeo` should define the interface for how values and units are represented in computations. This is a separate concern from display formatting, but it will strongly influence how the display layer and analysis layer interact.

### Core questions
1. **What is the canonical computational representation?**
   - Should internal functions primarily operate on bare numeric values with separate unit metadata?
   - Should functions accept `units` objects directly?
   - Should both be supported, with one canonical internal form?

2. **Where do unit conversions happen?**
   - At the function boundary?
   - Inside analysis helpers?
   - Only in dedicated conversion helpers?
   - Should output functions ever perform computation-layer conversions, or only display conversions?

3. **What is the public API contract for value + unit inputs?**
   - Should public-facing functions require explicit unit metadata?
   - Should they infer units from data structures when possible?
   - Should they validate units aggressively or allow permissive coercion?

4. **What should internal helper functions accept and return?**
   - Bare numeric vectors?
   - `units` objects?
   - Tibbles/data frames with unit-bearing columns?
   - Structured objects that carry both values and unit metadata?

5. **How should formulas from the literature be encoded?**
   - Should each scientific method declare its required input and output units?
   - Should computations be normalized into method-native units before evaluation?
   - Should conversion helpers be method-specific or shared?

6. **How should interoperability with `sf`, `terra`, `stars`, and `units` be handled?**
   - Should geometry and raster/vector inputs retain their native units where possible?
   - Should spatial metadata be propagated through computations?
   - Should unit conversion rely on ecosystem-native methods where available?

7. **How should tests be structured for computation semantics?**
   - Should tests assert unit preservation?
   - Should they assert conversion accuracy?
   - Should they assert equivalence between bare numeric and `units`-aware paths?

### Design goals
- preserve scientific correctness
- make unit handling explicit and testable
- reduce hidden assumptions in analysis functions
- support both `units` objects and display-layer unit rendering without conflating them
- avoid duplicating conversion logic across output functions and analysis functions
- remain compatible with the broader geospatial ecosystem

### Proposed next design step
Define a computation interface that distinguishes between:
- **semantic unit handling**: represented with `units`
- **analysis input normalization**: conversion into method-required units
- **display rendering**: handled by the display metadata layer

This interface should be documented before expanding the plotting and report refactors further.


***


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


***

