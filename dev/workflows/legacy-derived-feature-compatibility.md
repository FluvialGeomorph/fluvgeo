# Legacy-derived feature compatibility

## Purpose

Use this workflow when a `fluvgeo` function replaces or materially overlaps a
legacy `FluvialGeomorph-toolbox` ArcPy producer. The new function may improve the
algorithm, validation, automation, defaults, portability, and provenance, but
its derived output remains compatible with historical projects and consumers.

FGDB and implementation evidence interact iteratively. Accepted FGDB invariants
govern the backend, but FG Studio may demonstrate previously unresolved portable
GeoPackage or interoperability requirements. Route that evidence back into the
owning FGDB contract; do not treat a draft physical schema as final or silently
promote a client-local representation.

## Evidence and authority

Before changing code, jointly inspect:

1. the ArcPy producer and parameters;
2. `FG-Tech-Manual/data_dictionary.csv`;
3. representative legacy feature classes and package fixtures;
4. current downstream readers, reports, and clients;
5. the applicable FGDB compatibility profile and canonical domain contract; and
6. the existing constructor, `check_*` validator, help, and tests.

Do not resolve conflicting evidence silently. Preserve the broadest verified
non-conflicting contract and route a material semantic or unit conflict for
review.

## Producer contract

- Preserve the legacy entity/layer name.
- Preserve every established field name exactly, including capitalization.
- Preserve field type, unit, null behavior, and scientific meaning.
- Preserve the geometry family and documented direction/measure conventions.
- Treat additions as additive. Never substitute a clearer new field for a
  legacy field.
- Do not fabricate Esri-managed IDs or use them as scientific identity. A
  target driver may create its own `OBJECTID`, geometry, and length fields.
- Keep the open-source method and legacy output schema as separate concepts:
  schema compatibility does not claim numerical equivalence between algorithms.

## Defensive checks

`check_*` functions are part of the enforcement boundary, but field-presence
checks alone are insufficient. The applicable validator should check geometry,
required exact names and types, finite/nonmissing values, units or explicit
measure relationships, direction/order, and any cross-field invariants that can
be established without inventing provenance.

When current R clients rely on behavior that differs from the ArcPy contract,
preserve the existing default and add an explicit replacement profile. FG Studio
must request that profile. Test both paths and document the distinction.

## Completion

Update the FGDB compatibility profile when evidence changes. Add deterministic
contract tests using representative data plus negative cases for renamed,
removed, mistyped, and unit-changed fields. Assess all downstream repositories
named in `backend-change-assessment.md`; do not declare an ArcPy tool replaced
while a known output-contract gap remains.
