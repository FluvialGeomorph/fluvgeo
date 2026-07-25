# Schemas

Last updated: 2026-07-24

## Purpose
This document records important structural contracts used by the repository, including data objects, files, tables, configuration structures, and other interfaces whose shape must remain explicit.

## How to use
- Add schemas for any durable data structures that other code depends on.
- Record required fields, types, constraints, and invariants where relevant.
- Update this file when new structured artifacts are introduced or existing ones change.

## Cross-section watershed contract

`cross_section()` always returns a numeric `Watershed_Area_SqMile` field.
Its `watershed` argument defines how that field is populated:

- `"required"` is the default. The remote watershed lookup must return one
  finite, positive drainage-basin area or processing stops.
- `"optional"` attempts the lookup. A service or response failure emits a
  warning and returns `NA_real_` for the affected cross section.
- `"skip"` performs no watershed request and returns `NA_real_`.

The geometry, reach position, sequence, and stationing fields remain available
in every mode. Consumers must not substitute a fabricated drainage area when
the field is missing. Operations that scientifically require drainage area
must retain strict validation; consumers that require only DEM-derived geometry
may continue without it.

## Current status

The cross-section watershed contract above is the current supplemental
project-wide schema. Other function-level contracts remain in generated package
documentation and their tests.

Add an explicit schema here when a data object, spatial layer, file, table, or
cross-repository interface has a durable shape that is not adequately governed
by one function's documentation. Do not use illustrative placeholder fields as
if they were implemented contracts.
