# Reporting intent: a reviewable record of the complete study

Status: user-established intent, 2026-09-06, clarified 2026-09-10. The design targets below are not a
claim of implemented functionality. Current behavior is recorded in the
[Terrain Development feature](../features/terrain-development-report.md).

## Purpose

fluvgeo reports should explain what a study encompasses, how its data were
developed, why consequential choices were made, and what evidence supports its
results. They should let another analyst or customer understand and review the
study without relying on the originating expert's memory or an active GIS
session. This is a scientific deliverable, not just a compliance log.

That purpose begins with Study Area definition and terrain development, before
Level 1. It includes selected Streams, Reach definitions, Survey Events and
their terrain/network evidence, as well as the subsequent Reach-level results.
FGDB has made the missing parent-level record apparent; the need exists whether
or not a study uses FGDB.

The user identifies reliance on a small number of highly trained GIS analysts
as a historical barrier to broader adoption. The intended response is to make
early data development understandable and repeatable through tooling and visual
feedback, not to add paperwork or automate away scientific judgment.

## What existing reports establish

The following shipped R Markdown templates were reviewed in full:

| Templates | Existing strengths to carry forward |
| --- | --- |
| [Level 1](../../inst/reports/level_1_report.Rmd) and [variant B](../../inst/reports/level_1_report_b.Rmd) | Reach overview, longitudinal comparisons, cross-section metrics and individual profiles. |
| [Level 2](../../inst/reports/level_2_report.Rmd) and [variant B](../../inst/reports/level_2_report_b.Rmd) | Spatial and graphical interpretation of dimensions, regional comparisons and detailed cross-section evidence. |
| [Level 3](../../inst/reports/level_3_report.Rmd) | Reach-scale maps, longitudinal and cross-section evidence, dimensionless metrics and regional comparisons. |
| [Bankfull estimation](../../inst/reports/estimate_bankfull_report.Rmd) | An explicit analytical purpose, candidate-elevation comparisons, goodness-of-fit evidence and supporting profiles. |

Verified: the Level 1–3 templates' “Study Area Overview” sections call
`map_reach_overview()` and caption a “Reach Overview Map.” Their supplied inputs
already include derived Reach-level analysis products. These templates do not
record the parent Study Area/Stream development process merely by using that
heading. This finding concerns the reviewed templates, not all historical
project documentation.

Their useful pattern is visual orientation followed by comparisons and detailed
evidence. Terrain Development should extend that pattern upstream: explain the
study and its choices with maps and figures, not replace it with validation
tables. The current Terrain Development prototype supplies an initial inventory
and overview, not the complete record described here.

## Distinct reports sharing one study record

The user clarified on 2026-09-10 that project reconstruction and structural
specification belong primarily in a **Staging Report**, rather than overloading
the Terrain Development Report. This refines the earlier "one evolving report"
presentation, not the underlying study model or the three reporting jobs below.

- **Staging Report:** what the project contains, how Study Area/Streams/Reaches/
  Survey Events are defined, which artifacts support them, and which structural
  interpretations or migration inputs the analyst must resolve. It should also
  describe a correctly specified project, not only its failures.
- **Terrain Development Report:** terrain sources, derivation and conditioning,
  grid/coverage evidence, reference systems, scientific limitations and intended
  uses. It reuses the study definition rather than becoming another hierarchy
  editor or requiring duplicate analyst input.

Both consume shared fluvgeo context and assessments. Their review outcomes remain
separate from conversion fidelity and enterprise acceptance. The first
[Staging Report](../features/study-staging-report.md) now provides a focused
presentation; existing Terrain report callers remain compatible while further
content separation can proceed incrementally.

### Three continuing reporting jobs

1. **Help build and configure the study.** Show intended scope alongside available
   evidence, candidate definitions, consequences and the next decisions. Keep
   drafts and unknowns visible so a partially defined study can be discussed.
2. **Describe the study once defined.** Preserve its structure, selected evidence,
   development methods, decision rationale and qualifications as a durable
   snapshot. A study with no outstanding findings still needs this description.
3. **Reconstruct archived analyses for FGDB preparation.** Guide an analyst who
   did not create the project through surviving artifacts and missing structural
   intent. Keep observed facts, proposed relationships, confirmed interpretations,
   rejected alternatives and unknowns distinct. Preserve the evidence and
   reasoning; never convert a plausible filename-based guess into governed
   hierarchy without explicit reconciliation. This job was added by the user on
   2026-09-06 and shares the same report components, not a separate scientific model.

Use reusable context and focused report variants, rather than require a separate
document for every operation. Later Reach reports should be traceable to this
parent context without repeating the entire Study Area narrative.

New and reconstructed projects target standardized local GeoPackage storage and
folder organization under [FGDB ADR-0024](../../../FGDB/dev/decisions/adr-0024-geopackage-local-standard-and-archive-reconstruction.md).
Keep source archives unchanged. A successful storage conversion does not itself
recover missing history or establish readiness to load FGDB.

## Visual desktop deliverable: design targets

Organize the report around questions a GIS analyst and a non-GIS customer can
both ask. Each visual should explain its meaning, limitations and relevant
choices; use clear names, legends, units and consistent identifiers.

| Reader's question | Intended visual evidence |
| --- | --- |
| What are we studying, and why this extent? | Study Area locator and detailed AOI map, purpose and scope rationale; distinguish selected Streams from the wider available network. |
| How is the study organized? | Study Area → Stream → Reach → Survey Event overview paired with labelled maps and explanations of Reach boundaries. Show Collection context when relevant. |
| What exists for each place and time? | Reach-by-Survey-Event inventory/coverage matrix and comparable coverage maps; distinguish planned, retained, assessed and usable evidence. |
| How was the terrain and network developed? | Source-to-output lineage, elevation/hillshade views, valid-cell coverage and relevant before/after processing comparisons. |
| Can periods be compared meaningfully? | Side-by-side extents and terrain properties, stated horizontal/vertical references, methods and explicit temporal/Reach-identity qualifications. |
| What was decided, and what remains to do? | Spatially located findings and concise decision explanations, linked to evidence, affected entities and the next action. |

The hierarchy view must not incorrectly nest every network under a Reach:
Study-Area-owned network Configurations and their Observations need a separate
relationship view showing actual Stream/Reach assignments and supported event
associations. Names, proximity or matching years alone do not establish those
relationships.

Stream boundaries and naming are analyst-selected study-design choices, not
universally fixed hydrologic units. In the NWO_Papillion example, the user confirms
that selected HUC12 polygons and names define Stream areas and their dissolved
union defines the Study Area. This useful project convention is not a requirement
for other studies. Reports must state the chosen delineation/naming basis and its
authority; standardized future storage must make the hierarchy explicit without
requiring an analyst to reconstruct it from filenames or geographic proximity.

Thorough means complete and interpretable, not an unfiltered data dump. Give the
desktop reader an intelligible overview, substantive visual sections and a
technical appendix for identifiers, detailed checks and provenance. Optional
interactive exploration should not be the only place essential evidence exists
in the saved deliverable.

## Shiny: the same study, less interaction burden

Shiny should always offer a Study Area view and access to the durable report.
Expose detailed configuration requests selectively when human input is needed.
Automate bookkeeping and unambiguous checks; retain human decisions about study
scope, meaningful segmentation, ambiguous identities and scientific acceptance.
Do not turn all report sections into mandatory wizard steps.

Both presentations should consume the same reusable study description and
assessment in fluvgeo. Shiny owns interaction; FGDB owns governed persistence.
The report is the primary human-facing review artifact, not the sole validation
authority. Saving/loading must still enforce the applicable data contracts.

## Durability and honest feedback

The target saved report should identify its generation time, study/data editions,
source references, methods, decisions and qualifications sufficiently to explain
what was reviewed. Preserve essential figures and descriptions with the report;
do not depend on a temporary preview server. Reference retained source data
rather than imply that an HTML report replaces them. Subsequent edits should
produce a distinguishable snapshot, not silently rewrite the previous record.

Separate observed evidence, analyst decisions, proposals and unknowns. Distinguish
missing, not assessed and not applicable. Assess coverage against its intended
use: a Reach-scale DEM need not cover the whole Study Area. Optional AOIs are not
missing required identities, and unspecified project rules are not analyst errors.
Show what each finding prevents at the relevant stage, rather than a single
undifferentiated compliance score. Network acceptance alone does not establish
terrain quality, cross-time comparability, Level 1 readiness or full FGDB compliance.

## Design feedback and success criteria

This report is also a working aid for FGDB design. Concrete maps, relationship
views and multi-period examples should expose missing concepts and confusing
associations before they become persistence contracts. Unresolved design questions
belong in that discussion, not disguised as requirements already enforced by code.

Success means another analyst can reconstruct the study's organization and
development rationale, and a non-GIS customer can understand its scope, evidence
and limitations. Users should make fewer repetitive decisions without losing
control over consequential scientific choices.

The first visual Study Area structure/event-grid view and reconstruction ledger
are now implemented, with a limited shared assessment; valid-cell coverage and
complete migration validation remain future work. A larger,
analyst-supplied fixture is needed to verify multi-Stream/multi-Reach behavior;
its absence need not stop every presentation improvement. Exact provenance fields,
report edition storage and event/network associations remain design work, not
new schema contracts established by this intent document.
