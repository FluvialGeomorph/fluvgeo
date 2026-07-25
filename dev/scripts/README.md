# Development scripts

Store maintained automation supporting development workflows here. Scripts should document inputs, outputs, dependencies, and safe execution expectations.

- `development.R`: interactive context validation, documentation, loading,
  testing, dependency reconciliation, and package checks.
- `package-bootstrap.R`: historical record of one-time package scaffolding.

Run these selectively; neither file is intended to be sourced from top to
bottom as an automated pipeline.
