# Development scripts

Store maintained automation supporting development workflows here. Scripts should document inputs, outputs, dependencies, and safe execution expectations.

- `development.R`: interactive context validation, documentation, loading,
  testing, dependency reconciliation, and package checks.
- `package-bootstrap.R`: historical record of one-time package scaffolding.
- `check-stream-corridor.R`: focused offline Stream buffering, publication and
  polygon-combination tests. Uses temporary fixtures, no active projects/services.

Run these selectively; neither file is intended to be sourced from top to
bottom as an automated pipeline.
