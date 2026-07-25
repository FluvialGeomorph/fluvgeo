# Backend change assessment

## Purpose

Use this workflow for implementation, troubleshooting, documentation, release,
or maintenance work that may affect `fluvgeo` methods or downstream consumers.

## 1. Establish ownership

Determine whether the issue belongs to:

- upstream geospatial input generation;
- reusable `fluvgeo` analysis or validation; or
- downstream application orchestration and presentation.

Inspect the relevant client contract before moving responsibility across this
boundary. For multi-repository changes, define the interface and compatibility
order before editing either side.

## 2. Inspect evidence

Review the smallest authoritative set of:

- affected functions and generated documentation;
- input and output schemas;
- existing tests and representative package data;
- downstream call sites where compatibility is material;
- dependency versions and platform assumptions;
- relevant accepted decisions and current architecture.

Do not infer scientific or spatial requirements when repository evidence,
reference methods, or a reproducible example can establish them.

## 3. Define the change contract

State:

- intended behavior and scientific rationale;
- inputs, outputs, units, coordinate reference systems, and invariants;
- API and data compatibility;
- error and missing-data behavior;
- upstream and downstream effects;
- the smallest verification that would demonstrate correctness.

Update `dev/schemas/` when a maintained structural contract changes. Record a
decision when the choice has durable tradeoffs or changes an ownership boundary.

## 4. Implement and verify

- Keep reusable domain logic in ordinary package functions.
- Add focused deterministic tests for the changed contract.
- Use package data or `fluvgeodata` fixtures with documented provenance.
- Guard unavoidable network or platform-specific behavior appropriately.
- Regenerate roxygen documentation when exported behavior changes.
- Update examples, README or articles, and `NEWS.md` when user-visible behavior
  changes.
- Run focused tests, the full suite, and `R CMD check` in a restored compatible
  environment.

## 5. Review downstream impact

Before completion, assess applicable effects on:

- `FluvialGeomorph-toolbox`;
- `ohwm2`;
- `RegionalCurve`;
- `fluvgeodata`;
- package installation and release workflows.

Keep each repository independently reviewable. Do not combine unrelated client
refactors with a backend fix.
