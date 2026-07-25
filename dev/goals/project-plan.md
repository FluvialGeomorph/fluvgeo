# Project Plan

Last updated: 2026-07-24

## Purpose
This file is the canonical ordered task list for active development work.

## How to use
- Keep tasks small and concrete.
- Record definitions of done where helpful.
- Update this file when design discussions create follow-up work.
- When resuming work, read this file and `dev/architecture/design.md`.

## Current state

Agentic-context standard 0.1 has been migrated on
`feat/agentic-context-migration`. No functional package-development objective
was recorded before the migration, and this structural branch does not invent
one.

## Next planning action

- [ ] Select and define the next functional package objective.
- [ ] Restore or select a compatible R dependency environment before relying on
  local full-suite results.
- [ ] Consider adding an R CMD check workflow as a separate, focused change;
  current GitHub Actions publish pkgdown but do not run package checks.
