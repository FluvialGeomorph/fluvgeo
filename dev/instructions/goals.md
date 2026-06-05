# goals — Quality, Evidence, and Reviewability Goals

## Prioritize authoritative documentation (required)
- Prefer **official, canonical documentation** over blog posts, anecdotes, or speculation.
- Pay attention to versions and compatibility; do not mix guidance across incompatible versions.
- When guidance is uncertain or context-dependent, say so and point to the relevant doc source or decision point.

## Use authoritative best practice (required)
- Prefer solutions aligned with widely accepted industry best practices, even when they require a bit more setup.
- Avoid quick fixes and shortcuts that create long-term maintenance burden unless explicitly requested.

## No speculation (required)
- Do not propose speculative fixes when relevant documentation or direct evidence (repo inspection, logs, minimal repro) can be used.
- If uncertainty remains after checking evidence, state what is unknown and propose the smallest next diagnostic step.

## Reproducible research methods (required)
- Emphasize workflows that are reproducible:
  - deterministic outputs where possible,
  - clear dependency management and version awareness,
  - documented data provenance and processing steps (when relevant).
- Prefer solutions that are easy for another analyst to rerun and verify.

## Transparent audit trail (required)
- Prefer approaches that leave a clear QA/QC trail:
  - explicit assumptions,
  - traceable inputs/outputs,
  - documented rationale for key decisions,
  - tests and checks that support reviewer confidence.

