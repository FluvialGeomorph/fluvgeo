# Agentic-context migration 0.1

- Applied: 2026-07-24
- Profiles: base, r-package
- Policy: replacements were created and validated before tracked legacy sources were removed.

## Adapted artifacts

- `dev/05_plan.md` -> `dev/goals/project-plan.md`
- `dev/10_design.md` -> `dev/architecture/design.md`
- `dev/40_schemas.md` -> `dev/schemas/project-schemas.md`
- Backend ecosystem and architecture guidance from
  `dev/instructions/fluvgeo-backend-context.md` was distilled into
  `dev/architecture/backend-ecosystem.md`.
- Backend troubleshooting, compatibility, scientific validation, and release
  guidance was distilled into
  `dev/workflows/backend-change-assessment.md`.
- `dev/02_dev.R` -> `dev/scripts/development.R`
- `create_package_steps.R` -> `dev/scripts/package-bootstrap.R`

## Removed artifacts

- `dev/instructions/chat-manual.md`: Legacy instruction copy is replaced by AGENTS.md and routed durable artifacts.
- `dev/instructions/CHAT_INSTRUCTIONS.md`: Legacy instruction copy is replaced by AGENTS.md and routed durable artifacts.
- `dev/instructions/development-governance.md`: Legacy instruction copy is replaced by AGENTS.md and routed durable artifacts.
- `dev/instructions/fluvgeo-backend-context.md`: Legacy instruction copy is replaced by AGENTS.md and routed durable artifacts.
- `dev/instructions/goals.md`: Legacy instruction copy is replaced by AGENTS.md and routed durable artifacts.
- `dev/instructions/r-package.md`: Legacy instruction copy is replaced by AGENTS.md and routed durable artifacts.

## Verification

- Replacement files copied: 3
- Superseded files removed: 9
- The standard structure passed validation before removal.
- Git history remains the archive for superseded content.
- The package-specific backend guidance was reviewed and routed into maintained
  artifacts before its legacy instruction copy was removed.
- The existing `dev/decisions/README.md` was semantically compatible with the
  standard and was updated to the standard index before the final plan.
- The local test suite could not start because the active R library lacks many
  required spatial and package imports. This is an environment limitation, not
  a passing or failing test result.
