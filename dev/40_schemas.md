# Schemas

Last updated: 2026-06-05

## Purpose
This document records important structural contracts used by the repository, including data objects, files, tables, configuration structures, and other interfaces whose shape must remain explicit.

## How to use
- Add schemas for any durable data structures that other code depends on.
- Record required fields, types, constraints, and invariants where relevant.
- Update this file when new structured artifacts are introduced or existing ones change.

## Schemas

### Example schema
| Field | Type | Required | Notes |
|---|---|---|---|
| id | character | yes | Stable identifier |
