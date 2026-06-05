# ADR-0001: Separate geospatial, analysis, and display unit systems

## Status
Proposed

## Context
`fluvgeo` supports fluvial geomorphology workflows that consume geospatial data from heterogeneous sources across many coordinate systems, datums, and time periods. The scientific formulas implemented by the package come from literature that uses a wide variety of native unit conventions. At the same time, reports and plots must support user-selectable display units for broad adoption across regions and audiences.

Historically, the package has allowed display assumptions, analysis assumptions, and data assumptions to become intermingled. This creates maintenance risk and makes it difficult to support flexible reporting without introducing inconsistent unit handling.

## Decision
`fluvgeo` will maintain a strict separation between three unit systems:

1. Geospatial data unit system
2. Analysis unit system
3. Display unit system

Geospatial data units describe incoming spatial inputs and are preserved as part of the package’s data boundary.

Analysis units describe the units required by scientific formulas and internal calculations. Analysis functions may convert inputs into formula-native units as needed, but those conversions are internal to the scientific implementation.

Display units describe the user-facing representation of results in plots, tables, captions, legends, and reports. Display units must be explicitly selectable and must be derived from a shared display-unit specification.

## Consequences
### Positive
- clearer architectural boundaries
- improved maintainability
- more reliable testing
- consistent report and plot output
- easier support for both USCS and SI display conventions

### Negative
- significant refactoring of output functions and report templates
- additional coordination with downstream consumers
- some transitional complexity while legacy unit assumptions are removed

## Alternatives considered
### 1. Keep unit handling implicit and ad hoc
Rejected because it perpetuates coupling and inconsistent output formatting.

### 2. Standardize all data and analysis to one global unit system
Rejected because it is not compatible with the package’s need to support heterogeneous geospatial inputs and literature-derived formulas.

### 3. Separate units by layer
Accepted because it preserves scientific fidelity, supports flexible output, and provides a durable architecture for future maintenance.

## Notes
This ADR should be read together with the current design documentation and the implementation plan for refactoring output functions and report templates.
