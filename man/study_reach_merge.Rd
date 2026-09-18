\name{preview_study_reach_merge}
\alias{preview_study_reach_merge}
\alias{merge_study_reaches}
\title{Combine saved Reaches under one retained identity}
\usage{
preview_study_reach_merge(dsn, reach_ids, retain_reach_id)
merge_study_reaches(dsn, output_file, reach_ids, retain_reach_id,
  reach_name, report_purpose = "definition")
}
\arguments{
\item{dsn}{Existing study context GeoPackage.}
\item{output_file}{New context GeoPackage beside dsn; never overwritten.}
\item{reach_ids}{Two or more distinct saved Reach identities from one Stream.}
\item{retain_reach_id}{One selected identity to retain.}
\item{reach_name}{Combined Reach name, unique in its parent Stream.}
\item{report_purpose}{Revision purpose: definition, terrain or staging.}
}
\description{Verifies retained source evidence and combines its inherited-buffer
geometry under one selected Reach identity. Other selected identities leave the
current inventory but remain in prior revisions. No source files are changed.}
\details{Survey Events retain their IDs, dates and attributes; only retired parent
Reach IDs are reassigned. Linked network and terrain-manifest references require
separate reconciliation and block this operation. Missing, changed or stale
Reach evidence also blocks merging. Evidence-first publication may leave an
unreferenced evidence file on failure. Disconnected parts and overlaps are not
repaired. Neither a complete network nor scientific acceptance is inferred.}
\value{Preview returns the combined corridor, source mapping, retained and retired
IDs, retained name and reassigned event count. Save returns revision paths,
evidence and the retained reach_id.}
