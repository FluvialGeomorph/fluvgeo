# Optional watershed enrichment for cross sections

## Capability

Cross-section geometry and stationing can be processed independently of the
remote NLDI watershed service when a client does not require drainage area.

`cross_section()` supports three modes:

- `required`: preserve strict historical behavior;
- `optional`: attempt enrichment and warn with typed missing data on failure;
- `skip`: avoid the remote request and return typed missing data.

The default remains `required`, preserving compatibility for scientific
workflows that require watershed area. `ohwm2` uses `skip` because its Results
geometry, water-surface volumes, profile plots, and Manning discharge
calculation do not use watershed area.

## Failure behavior

An invalid response includes a missing drainage-basin object, an empty basin,
or a non-finite/non-positive area. Required mode stops with a concise error.
Optional mode warns and returns `NA_real_`. Skip mode neither calls the service
nor warns.

See `dev/schemas/project-schemas.md` for the output contract.
