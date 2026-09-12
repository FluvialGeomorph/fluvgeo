# Reporting intent: a reviewable record of the complete study

Status: user-established intent, 2026-09-06, clarified 2026-09-10. The design targets below are not a
claim of implemented functionality. This is the maintained requirements home for
the new-project/legacy-project distinction. Current behavior is recorded in the
[Terrain Development](../features/terrain-development-report.md) and
[Staging Report](../features/study-staging-report.md) features.

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
evidence. Study Area and terrain reporting should extend that pattern upstream: explain the
study and its choices with maps and figures, not replace it with validation
tables. The current Terrain Development prototype supplies an initial inventory
and overview, not the complete record described here.

## Two entry workflows, one Study Area configuration

**User-established clarification, 2026-09-10:** new projects are progressively
designed as customer requirements develop; legacy projects are forensically
reconstructed from limited surviving artifacts. The difference is how the study
definition is established, not two different scientific models. This supersedes
the earlier wording that made "staging" the general home for structural work.

| View / workflow | Primary question | Appropriate evidence and feedback |
| --- | --- | --- |
| New project: **Define Study Area** | What do we intend to study, and which scope choices remain open? | Customer objectives, candidate AOIs, selected Streams, proposed Reach segmentation, intended observations and decision rationale. Undecided choices are normal design work, not archive defects. |
| Legacy project: **Staging Report** | What did this project represent, and what must be reconstructed for migration? | Surviving artifacts, source selection, competing interpretations, analyst-confirmed hierarchy, acquisition evidence and missing migration inputs. Preserve uncertainty and the untouched archive. |
| Either origin: **Study Area Report** | What is the resulting study definition and why? | A durable, customer-readable description of the shared configuration, its boundaries, relationships, decisions and qualifications, including outstanding limitations. |
| Either origin: **Terrain Development Report** | What terrain was used, how was it developed, and what limitations affect its use? | Terrain sources, derivation/conditioning, grid/coverage evidence, reference systems and scientific suitability. Reuse the Study Area definition rather than defining it again. |

These are purpose-specific views of one shared record, not four required documents
at each step, a fixed wizard sequence or separate schemas. A draft can be reviewed
before complete configuration; a defined study still deserves a description even
when its action queue is empty. The Study Area Report is not a claim that every
analysis product exists or that FGDB has accepted the project. The view names are
working user-facing requirements, not new API names or persisted status codes.

### New-project design requirements

- Start with customer purpose and the decisions the study must support. Allow
  an initial scope, candidate boundaries and alternatives to evolve without
  demanding legacy sources, completed terrain or a final survey schedule.
- Progressively specify Study Area extent, selected Streams, Reach segmentation
  and intended temporal coverage. Record what is decided, what remains open,
  whose input is needed and why each consequential choice was made.
- Keep proposed observations distinct from actual Survey Events. An intended
  year or planned campaign is not evidence of an acquisition. The existing
  dated Survey Event contract remains strict; do not insert fabricated dates
  or weaken accepted records to accommodate planning. A structured planning
  representation and its promotion rules still need design.
- Revisions should make affected relationships and evidence visible for review;
  do not silently reparent entities or reuse identities after substantive
  merge/split changes. Full draft editing and revision provenance are development
  targets, not capabilities supplied by the current name/note editor.
- New open-source projects target the GPKG folder standard directly. No FileGDB
  staging, archive-copy provenance or legacy-conversion step is required.

### Legacy reconstruction requirements

- Begin with the untouched archive and analyst-selected clean copies. The
  migrating analyst may not be the original maker; filenames, timestamps,
  geometry overlap and repeated labels are evidence, not authoritative meaning.
- Preserve verified source facts, inferred interpretations, proposed definitions,
  analyst confirmations and unknowns separately. An unconfirmed interpretation
  must not silently populate a governed hierarchy or acquisition record.
- Reconstruct explicit parent-level context and event/source associations in
  FileGDB staging, then qualify conversion to the GPKG folder standard. The
  [legacy staging draft](../../../FGDB/dev/schemas/legacy-project-staging-contract.md)
  supplies the source-side proposal, not universal new-project prerequisites.
- Unknown acquisition dates remain analyst inputs. A missing date may block
  conversion-ready status under that draft while still allowing inspection,
  reconstruction discussion and reviewable reporting to proceed.

### Shared configuration and acceptance boundaries

Both workflows describe the same Collection / Study Area / Stream / Reach /
Survey Event relationships. Share identity, explicit parentage, AOI meaning,
evidence references, analyst decisions and descriptions across reports and
clients; do not ask the user to maintain separate copies. A Study Area
configuration is not the narrower Stream Network Configuration entity.

Mixed projects may reconstruct old content while planning new observations;
retain the evidence basis for each input or decision rather than imposing a
permanent new-versus-legacy identity on the whole project. Switching report
purpose must not rewrite data, generate new identities or change acceptance.

For either workflow, distinguish an open design choice, an unknown historical
fact, missing required data, unassessed evidence and an actual contradiction.
Show the affected operation and next useful action. Block only work that needs
the unresolved information; do not make a report an all-or-nothing gate to useful
design progress. Structural definition, terrain suitability, conversion fidelity
and FGDB load acceptance are different outcomes, not one compliance score.

### Implemented versus still to build

**Verified implementation:** shared partial report context, explicit supplied
hierarchy and dated events, an interpretation ledger, bounded saved-context
name/note revision, and the first legacy inventory/Staging Report. Existing
Terrain report callers retain their earlier combined view for compatibility.

**First prospective slice (2026-09-10):** `start_study_context()` creates a named
new-study draft and optional scope notes without acquired data; the new
`define_study_area_report()` offers a small prospective view of the shared
context. This is not the complete progressive design experience. The new QGIS
starter wrapper passed actual-provider qualification on 2026-09-11; its
[analyst trial](../../../fg-qgis-toolbox/dev/workflows/qgis-desktop-trial.md)
remains separate from that technical evidence.

**Requirements not yet implemented:** structured customer requirements/alternatives/planned observations, general
hierarchy editing and a dedicated neutral Study Area Report. The existing saved
context is a limited development binding, not the complete shared configuration
model. See [its schema and limits](../schemas/study-context.md). Full staging
conformance, terrain qualification and enterprise loading remain separate work.

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

These report views are also working aids for FGDB design. Concrete maps, relationship
views and multi-period examples should expose missing concepts and confusing
associations before they become persistence contracts. Unresolved design questions
belong in that discussion, not disguised as requirements already enforced by code.

Success means another analyst can reconstruct the study's organization and
development rationale, and a non-GIS customer can understand its scope, evidence
and limitations. Users should make fewer repetitive decisions without losing
control over consequential scientific choices.

Future tools should demonstrate both entry cases before claiming general Study
Area configuration support: a new study with changing scope and no acquired
terrain, and a legacy project with incomplete provenance and unresolved dates.
Both must produce a useful partial review without inventing accepted records;
once explicitly configured, both must yield the same neutral study description.
Legacy-specific prompts must not appear merely because a new project has no
archive, and new-project planning must not erase uncertainty in historical data.

The first visual Study Area structure/event-grid view and reconstruction ledger
are now implemented, with a limited shared assessment; valid-cell coverage and
complete migration validation remain future work. A larger,
analyst-supplied fixture is needed to verify multi-Stream/multi-Reach behavior;
its absence need not stop every presentation improvement. Exact provenance fields,
report edition storage and event/network associations remain design work, not
new schema contracts established by this intent document.

**Historic Reach-area clarification (2026-09-11):** polygons were not required by
the historic workflow. Reports must not recast their absence as failed historical
compliance or a universal barrier to analysis. A selected `dem_hydro` extent can
provide a documented reconstruction candidate. Distinguish its rectangular grid
envelope from valid-cell coverage and deliberate delineation; keep the chosen
survey/raster and suitability across periods explicit. New-study area design
does not require legacy terrain or staging.
