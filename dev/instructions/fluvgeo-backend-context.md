# fluvgeo-backend-context — Overlay Module (Shared Fluvial Geomorphology Backend Context)

## Purpose
`fluvgeo` is a shared R package backend for fluvial geomorphology analysis. It exists to rapidly derive fluvial geomorphic metrics and analysis from remotely sensed terrain data, especially LiDAR, using standardized and repeatable methods with a transparent audit trail.

This package supports both:
- the `FluvialGeomorph/FluvialGeomorph-toolbox` ArcGIS Pro script toolbox used by USACE GIS analysts in desktop workflows, and
- interactive R Shiny web applications such as `FluvialGeomorph/ohwm2`.

The primary purpose of the package is to define and maintain a common framework for deriving fluvial geomorphic metrics that can serve both desktop and web application front ends without duplicating core analysis logic.

## Scope
Use this overlay when working on any `fluvgeo` task involving:
- development
- troubleshooting
- deployment
- packaging
- documentation
- testing
- release preparation
- maintenance of shared analysis logic used by downstream applications

## Core context
`fluvgeo` is designed around the needs of fluvial geomorphology analysis workflows that benefit from remote sensing rather than manual field survey alone. This approach is especially valuable because it can:
- analyze arbitrarily large areas at high spatial resolution
- provide standardized representation of fluvial feature geometry
- standardize fluvial metric calculations
- support repeatable methods across resurveys through time
- produce standardized reporting outputs
- preserve explicit method definitions and a transparent audit trail for review and scientific validation

## Ecosystem role
`fluvgeo` is the common analysis layer in a broader application ecosystem.

It should be treated as:
- the authoritative backend for shared fluvial geomorphology methods
- a dependency used by multiple downstream client applications
- a package that must remain compatible with both desktop and web-based workflows
- a stable home for reusable calculations, data handling, and analysis conventions

When making changes, consider downstream impacts on:
- ArcGIS Pro toolbox workflows
- Shiny application workflows
- package users who rely on reproducible analysis outputs
- release and packaging processes

## Architecture transition context
`fluvgeo` originated in a hybrid ESRI + R architecture.

- Early geospatial feature derivation was implemented in proprietary ArcGIS/ESRI Python tooling in `FluvialGeomorph-toolbox`.
- `fluvgeo` provided later-stage analysis, reporting, and data science functionality in R.
- Over time, some geospatial functionality has been reimplemented with open-source tools inside `fluvgeo`.
- The package remains partially dependent on proprietary upstream geospatial input generation, so it is not fully standalone for all workflows.
- This transition is ongoing, and future development should account for partial coverage of geospatial functionality inside the R package.
- Treat the current architecture as a mixed-state system: some workflows still rely on upstream ArcGIS-derived inputs, while others are now supported directly in R.

## Preference for open source
When multiple viable approaches exist, prefer open-source solutions over proprietary ones when they satisfy the scientific, operational, and maintenance requirements, especially when they:
- preserve or improve scientific validity
- reduce licensing burden
- improve portability and adoption
- support reproducibility and maintainability
- fit the package's long-term direction toward a more open and portable ecosystem

## Design and maintenance principles
When advising on `fluvgeo`, prefer guidance that:
- preserves a common framework across all client applications
- keeps core analysis logic centralized in the package
- favors reproducibility, consistency, and traceability
- maintains clear and testable interfaces
- minimizes unnecessary coupling to any single front end
- protects API stability unless a breaking change is clearly justified

## Troubleshooting priorities
When diagnosing problems, prioritize:
- package behavior in downstream consuming applications
- consistency of metric calculations and derived outputs
- data structure and input assumptions
- environment and dependency issues
- documentation and examples matching actual package behavior
- packaging, installation, and release failures

When troubleshooting or designing changes, first determine whether the issue belongs to:
- upstream geospatial input generation,
- core R analysis,
- or a downstream consuming application.

## Documentation and release expectations
For user-facing changes, development sessions should consider:
- whether exported functions need documentation updates
- whether examples remain fast, reliable, and reproducible
- whether tests should be added or updated to lock in intended behavior
- whether NEWS/release notes need a concise entry
- whether downstream client usage or compatibility is affected

## Assistant behavior
In future sessions, use this overlay to:
- interpret `fluvgeo` changes in the context of shared backend support
- evaluate how decisions affect both desktop and web clients
- emphasize scientifically defensible, repeatable, and reviewable methods
- avoid assuming the package serves only one interface or deployment model
- recommend the smallest change that preserves clarity, compatibility, and maintainability

## Relationship to other instruction modules
This overlay is intended to compose with the repository's base chat instructions and other instruction modules.
For documentation and governance matters, follow `dev/instructions/development-governance.md` and the repository's established artifact precedence.
If there is any conflict, follow the most restrictive safety and permission constraints first, then the more specific repository guidance.
