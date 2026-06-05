# Development Governance

## Purpose
This module governs how AI-assisted development sessions maintain durable project documentation in repositories that use a `dev/` folder for development-state artifacts.

The goal is to ensure that important outcomes from a chat session are promoted into concise, structured repository documents rather than being left only in a session transcript.

This module is about:
- documentation governance
- artifact roles
- document precedence
- session-to-document promotion
- required chat behavior when durable project state changes

This module is not about:
- domain-specific implementation details
- general software engineering beyond documentation governance
- user-facing package documentation except where governance boundaries must be clarified

## When to use
Use this module when:
- the repository is developed through iterative AI-assisted chat sessions
- the repository uses a `dev/` directory to store development-state documentation
- the team wants durable records of plans, architecture, decisions, schemas, and reusable chat instructions
- the session may produce requirements, architecture, workflow, schema, or decision changes that should guide future work

This module is especially appropriate for:
- R packages
- `golem`-based Shiny apps developed as R packages
- other package-style repositories using `reproducibleai` conventions

## Assumed repository structure
This module assumes the repository contains the following internal development-governance artifacts:

- `dev/05_plan.md`
- `dev/10_design.md`
- `dev/40_schemas.md`
- `dev/decisions/`
- `dev/instructions/`
- `dev/sessions/`

It also assumes that user-facing package documentation belongs elsewhere, such as:
- `README`
- roxygen/man documentation
- vignettes/articles
- pkgdown site pages

## Artifact roles

### `dev/05_plan.md`
Canonical active work plan.

Use for:
- milestones
- ordered tasks
- definitions of done
- near-term progress and next steps

### `dev/10_design.md`
Stable current-state design and architecture document.

Use for:
- current package or system structure
- invariants
- capability boundaries
- document map
- stable operating assumptions
- current design truths that future work should inherit

### `dev/40_schemas.md`
Canonical schema and structural contract document.

Use for:
- file schemas
- structured object contracts
- required fields and types
- interface/data invariants
- durable data structure assumptions that other code depends on

Schema documentation is required, not optional. In data-science-oriented and AI-assisted repositories, implicit schemas are a common source of fragile and buggy behavior.

### `dev/decisions/`
Decision records.

Use for:
- meaningful design choices
- rationale
- alternatives considered
- supersession history

### `dev/instructions/`
Reusable chat/developer instruction modules and recipes.

Use for:
- modular workflow instructions
- reusable chat behavior conventions
- recipe-oriented guidance for future sessions

### `dev/sessions/`
Archived session transcripts.

Use for:
- transparency
- historical derivation trail
- auditability

Do not treat session files as the canonical source of final design state.

## Document precedence
When development-governance artifacts disagree, apply this precedence:

1. the latest accepted decision record in `dev/decisions/` for the decision itself
2. `dev/10_design.md` for current architecture and stable design state
3. `dev/40_schemas.md` for exact structural contracts
4. `dev/05_plan.md` for intended next work
5. `dev/instructions/` for chat workflow behavior
6. `dev/sessions/` for historical context only

If ambiguity remains, the chat should call out the ambiguity explicitly and recommend which artifact should be updated to resolve it.

## Trigger conditions
During the session, monitor for changes that should be promoted into durable documentation.

### Update `dev/05_plan.md` when:
- new implementation work is identified
- milestones are refined
- task ordering changes materially
- definitions of done are clarified
- a design discussion creates follow-up work

### Update `dev/10_design.md` when:
- architecture is defined or changed
- stable capability boundaries are clarified
- package responsibilities are clarified
- invariants or operating assumptions are established
- document structure or governance roles are revised

### Update `dev/40_schemas.md` when:
- exact file, object, table, or data contracts are defined
- required fields or structures become part of the design
- outputs, datasets, or scaffolded artifacts require a precise schema reference
- a module introduces or changes a maintained structured interface

### Create or update `dev/decisions/` when:
- a meaningful design choice is made
- alternatives were considered
- a convention is intentionally adopted
- an earlier design decision is superseded, narrowed, or replaced

### Update `dev/instructions/` when:
- reusable chat behavior is defined
- a new module or recipe is being designed
- guidance should be preserved for future sessions

## Required chat behavior
When a trigger condition is met, the chat must:

1. recognize that a documentation update is warranted
2. identify the correct artifact or artifacts to update
3. provide concise, paste-ready content for the user
4. prefer distilled final-state wording over transcript-like prose
5. remind the user when important governance updates remain pending

Do not merely say that something “should be documented later” when the session can already draft the correct update.

## Completion rule
A meaningful development task is not fully complete if the session has introduced or changed:
- architecture
- durable requirements
- design decisions
- structural contracts
- reusable workflow guidance

without also drafting the corresponding `dev/` updates for the user.

This does not require every reply to propose documentation edits. It requires the session to promote important durable changes before wrapping up.

## Anti-patterns
Avoid the following:

### 1. Treating session transcripts as the canonical source of truth
Session files are archival context, not final-state documentation.

### 2. Leaving meaningful decisions undocumented
If a design choice materially affects future work, capture it in the appropriate artifact.

### 3. Leaving schemas implicit
If other code depends on the structure of an object, file, dataset, or interface, record that structure explicitly in `dev/40_schemas.md`.

### 4. Mixing artifact roles
Do not use:
- plan files for stable architecture
- design docs for transient task tracking
- schema docs for rationale that belongs in decisions
- ADRs for task tracking
- session files as design documents
- user-facing articles as internal decision logs

### 5. Giving vague documentation advice
Do not say only “you should document this.”
Provide paste-ready additions or replacements.

### 6. Over-documenting trivial changes
Do not create design updates or ADRs for every minor edit. Use judgment and focus on durable project state.

### 7. Duplicating the same material across all artifacts
Each document should have a distinct role. Summarize where appropriate and avoid copy-pasting large repeated sections across plan, design, and decision records.

## Relationship to user-facing package documentation
For R packages, internal development-governance artifacts in `dev/` are distinct from user-facing package documentation.

Use pkgdown/vignettes/articles for:
- user guidance
- package workflows
- examples
- published usage documentation

Use `dev/` artifacts for:
- internal development state
- architecture
- design decisions
- schema contracts
- AI-assisted workflow continuity

Do not default to putting internal architecture decisions into public-facing docs unless those decisions are also useful to package users.

## Relationship to module handlers
This module remains a static reviewed instruction artifact.

In `reproducibleai`, instruction modules may have corresponding handler functions that install the canonical instruction text and perform supporting repository configuration.

For `development-governance`, the handler is expected to:
- install this instruction file into `dev/instructions/`
- scaffold the required `dev/` governance structure
- preserve existing repository content by default unless overwrite is explicitly requested

The handler supports this module’s use, but does not replace the chat session’s responsibility to draft substantive project-specific updates.

## Output expectations
When proposing governance updates, the chat should:
- clearly identify which artifact(s) need updating
- briefly explain why
- provide paste-ready content
- separate updates by artifact
- prefer concise, maintainable wording
- avoid leaving important decisions undocumented at the end of the session
