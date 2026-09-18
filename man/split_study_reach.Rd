\name{preview_study_reach_split}
\alias{preview_study_reach_split}
\alias{split_study_reach}
\title{Split a saved Reach at a snapped map location}
\usage{
preview_study_reach_split(dsn, reach_id, point, keep_side = "downstream",
  snap_distance = 200)
split_study_reach(dsn, output_file, reach_id, point, new_reach_name,
  keep_side = "downstream", snap_distance = 200, report_purpose = "definition")
}
\arguments{
\item{dsn}{Existing study context GeoPackage.}
\item{output_file}{New context GeoPackage beside dsn. Never overwritten.}
\item{reach_id}{Exact saved Reach identity.}
\item{point}{One CRS-defined sf/sfc POINT, normally a WGS84 map click.}
\item{keep_side}{downstream or upstream; portion retaining the original identity and name.}
\item{snap_distance}{Maximum click-to-line distance in metres, default 200, at most 2000.}
\item{new_reach_name}{Nonempty new name, unique in the parent Stream.}
\item{report_purpose}{definition, terrain or staging.}
}
\description{Projects the click into the inherited Stream processing CRS, snaps
to the selected Reach, splits its line pieces and regenerates both buffered
extents with the inherited width and parent clipping. Source COMIDs remain
separate from local piece identities. Previous revisions and evidence survive.}
\details{Requires one unbranched connected directed chain. Rejects whole-Reach
endpoints, distant clicks, self-crossings, unresolved evidence and dependencies
requiring reconciliation (Survey Events on the target, linked network/terrain
manifest). No child records are arbitrarily reassigned. A 1 cm endpoint snap
threshold prevents numerical remnants; this is not a minimum scientific Reach
length. Source-relative intervals refer to the exact retained metric source
line, not stationing or a different edition of NHDPlus. Cut and parent-line
evidence preserve lineage through repeated edits. Evidence-first publication can
leave orphan evidence after failure. No file or earlier revision is overwritten.}
\value{Preview: source, pieces, areas, cut, lengths_m, reach_id, new_reach_id,
stream_id, reach_name, keep_side and snap_distance_m. Preview IDs are provisional;
save recomputes and returns context/report paths, evidence, retained/new IDs.}
