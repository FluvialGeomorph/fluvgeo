# Agent instructions

## Identity and scope

`fluvgeo` is the current repository. Treat its code, tests, configuration, and maintained documentation as the authoritative evidence for repository-local behavior.

## Always-applicable rules

- Inspect repository and Git evidence before making consequential changes.
- Keep this file concise; route detailed knowledge into maintained artifacts under `dev/`.
- Preserve unrelated user changes and keep work within the requested repository scope.
- Distinguish verified evidence, reasonable inference, and unknowns.
- Open-source functions that replace `FluvialGeomorph-toolbox` ArcPy tools must
  preserve the legacy derived feature-class contract: entity/layer name and all
  established field names, capitalization, types, units, and meanings. New
  fields, validation and provenance may be additive. Breaking that contract
  requires an explicit reviewed migration decision and downstream plan.

## Conditional context routes

- Goals, scope, or success criteria: `dev/goals/`
- Architecture, dependencies, or ownership boundaries: `dev/architecture/`
- Consequential and durable choices: `dev/decisions/`
- Governance or artifact lifecycle: `dev/governance/`
- Repeatable development procedures: `dev/workflows/`
- Exact structural contracts: `dev/schemas/`
- Cohesive user-visible capabilities: `dev/features/`
- Resumable unfinished work: `dev/checkpoints/current/`
- R package API, implementation, documentation, or tests: `DESCRIPTION`, `NAMESPACE`, `R/`, `man/`, `tests/testthat/`, and `dev/workflows/r-package-development.md`
- Scientific backend responsibilities, ecosystem dependencies, or ownership boundaries: `dev/architecture/backend-ecosystem.md`
- Backend implementation, troubleshooting, compatibility, or release impact: `dev/workflows/backend-change-assessment.md`
- Current package architecture and the ESRI-to-open-source transition: `dev/architecture/design.md`
- Data, file, spatial, or interface contracts: `dev/schemas/project-schemas.md`
- Legacy ArcPy replacement or derived-output changes: read
  `dev/workflows/legacy-derived-feature-compatibility.md`, the applicable FGDB
  compatibility profile, original ArcPy producer, Technical Manual data
  dictionary, representative legacy data, downstream call sites, and relevant
  `check_*` validators before editing.

Full session transcripts are not normal context sources. Use maintained durable artifacts and concise checkpoints.

## Completion governance

Before declaring meaningful work complete, determine whether it changed goals, architecture, a durable decision, a schema or interface, a repeatable workflow, feature behavior, or resumable state. Update the applicable durable artifact. Create a checkpoint only when useful unfinished state remains.

## Verification and information governance

Run checks proportionate to the change, inspect Git status and diff, and confirm the change boundary. Never record secrets, credentials, PII, restricted data, or unnecessarily large logs in agentic-context artifacts.
