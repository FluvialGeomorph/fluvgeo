# ADR-0001: Separate geospatial, analysis, and display unit systems

## Status
Proposed

## Context
`fluvgeo` supports fluvial geomorphology workflows that consume geospatial data from heterogeneous sources across many coordinate systems, datums, and time periods. The scientific formulas implemented by the package come from literature that uses a wide variety of native unit conventions. At the same time, reports, plots, composed figure layouts, static documents, and interactive applications must support user-selectable display units for broad adoption across regions and audiences.

Historically, the package has allowed display assumptions, analysis assumptions, and data assumptions to become intermingled. This creates maintenance risk and makes it difficult to support flexible reporting without introducing inconsistent unit handling.

`fluvgeo` is built on the open-source geospatial ecosystem, including `sf`, `terra`, and `stars`, and should align with the ecosystem’s mature unit-handling infrastructure, including the `units` package, for forward compatibility and reproducibility. The package should prefer ecosystem-native unit metadata and conversions where practical.

Many output contexts require different unit renderings. For example, `ggplot2` can benefit from plotmath expressions, static documents may prefer LaTeX, interactive HTML or Shiny applications may prefer MathJax, and prose-heavy reports may prefer plain text or Unicode. The architecture must support these render targets without duplicating unit semantics.

## Decision
`fluvgeo` will maintain a strict separation between three unit systems:

1. Geospatial data unit system
2. Analysis unit system
3. Display unit system

Geospatial data units describe incoming spatial inputs and are preserved as part of the package’s data boundary.

Analysis units describe the units required by scientific formulas and internal calculations. Analysis functions may convert inputs into formula-native units as needed, but those conversions are internal to the scientific implementation.

Display units describe the user-facing representation of results in plots, tables, captions, legends, composed figure layouts, static documents, and interactive applications. Display units must be explicitly selectable and must be derived from a shared display-unit specification.

Package-facing code should expose a simple `unit_system` contract while using ecosystem-native `units` objects and conversions where they improve clarity, interoperability, and long-term compatibility.

In addition to `units`, `fluvgeo` will maintain a lightweight structured metadata layer for rendering unit-bearing quantities. This layer is not a competing unit system; it is a display-oriented contract that sits above `units` and standardizes how units are rendered for different output targets, including plain text, plotmath, Unicode, prose, LaTeX, and MathJax.

## Consequences
### Positive
- clearer architectural boundaries
- improved maintainability
- more reliable testing
- consistent report, plot, and figure-composition output
- easier support for both USCS and SI display conventions
- better alignment with the broader open-source spatial ecosystem
- support for multiple render targets without rewriting unit semantics
- better compatibility with static documents and interactive applications

### Negative
- significant refactoring of output functions, figure-composition helpers, and report templates
- additional coordination with downstream consumers
- some transitional complexity while legacy unit assumptions are removed
- a larger dependency on consistent use of ecosystem-native unit semantics
- additional design work to keep rendering targets consistent across contexts

## Alternatives considered
### 1. Keep unit handling implicit and ad hoc
Rejected because it perpetuates coupling and inconsistent output formatting.

### 2. Standardize all data and analysis to one global unit system
Rejected because it is not compatible with the package’s need to support heterogeneous geospatial inputs and literature-derived formulas.

### 3. Separate units by layer and align with ecosystem-native unit handling
Accepted because it preserves scientific fidelity, supports flexible output, and provides a durable architecture for future maintenance.

### 4. Use `units` alone for both semantics and rendering
Rejected because `units` is strong for semantic consistency and conversion, but not sufficient by itself to define a package-wide rendering contract across plotmath, Unicode, LaTeX, MathJax, prose, and HTML-oriented outputs.

## Notes
This ADR should be read together with the current design documentation and the implementation plan for refactoring output functions, figure-composition helpers, and report templates.
