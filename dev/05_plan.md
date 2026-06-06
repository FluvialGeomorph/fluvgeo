# Project Plan

Last updated: 2026-06-05

## Purpose
This file is the canonical ordered task list for active development work.

## How to use
- Keep tasks small and concrete.
- Record definitions of done where helpful.
- Update this file when design discussions create follow-up work.
- When resuming work, read this file and `dev/10_design.md`.

## Now
- [ ] Add immediate next task

## Upcoming
- [ ] Add next milestone or task group


## Unit system refactor plan

### Objective
Introduce a first-class display-unit architecture in `fluvgeo` that separates:
1. geospatial data unit system,
2. analysis unit system, and
3. display unit system.

### Phase 1: Architecture definition
- Confirm the public `unit_system` API.
- Add or finalize the unit architecture section in `dev/10_design.md`.
- Record the architectural decision in `dev/decisions/adr-0001-unit-system-separation.md`.

### Definition of done for Phase 1
- [x] The three-layer unit architecture is documented in `dev/10_design.md`.
- [x] The decision is captured in an ADR.
- [x] The public display-unit parameter is defined as `unit_system` with supported values `"USCS"` and `"SI"`.
- [x] The migration relationship between `profile_units` and `unit_system` is documented.

### Phase 2: Shared display-unit helper layer
- Create an internal helper module for display-unit resolution and formatting.
- Define the canonical USCS and SI display specifications.
- Provide helper functions for axis labels, legend labels, captions, and conversions.

### Phase 3: Report-dependent output functions
- Refactor `xs_profile_plot`.
- Refactor `compare_long_profile`.
- Refactor `compare_xs_long_profile`.
- Refactor `xs_metrics_plot_L1`.
- Refactor `xs_metrics_plot_L2`.
- Refactor `xs_metrics_plot_L3`.

### Phase 4: Remaining output functions
- Refactor `xs_metric_plot`.
- Refactor `fig_xs_profiles_L1`.
- Refactor `fig_xs_profiles_L2`.
- Refactor `reach_rhg_graph`
- Refactor `map_reach_overview`.
- Refactor `map_reach_metric`.

### Phase 5: Active report templates
- Update `inst/reports/level_1_report_b.Rmd`.
- Update `inst/reports/level_2_report_b.Rmd`.
- Update `inst/reports/level_3_report.Rmd`.
- Update `inst/reports/estimate_bankful_report.Rmd`.

### Phase 6: Report wrapper functions
- Update `R/level_1_report_b.R`.
- Update `R/level_2_report_b.R`.
- Update `R/level_3_report.R`.
- Update `R/estimate_bankful_report.R`.

### Phase 7: Testing and documentation
- Add unit tests for both USCS and SI display modes.
- Verify report rendering and plot labels.
- Update roxygen, README examples, and release notes as needed.

### Definition of done
- Display unit selection is explicit and consistent across all reports and plots.
- Analysis logic remains independent of display formatting.
- Geospatial input assumptions remain separate from display and analysis concerns.
- Existing report workflows render correctly under both supported display systems.
