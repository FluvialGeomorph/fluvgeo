\name{rename_study_feature}
\alias{rename_study_feature}
\title{Rename a saved Stream or Reach}
\usage{rename_study_feature(dsn, output_file, level, feature_id, name,
  report_purpose = "definition")}
\arguments{
\item{dsn}{Existing study context GeoPackage.}
\item{output_file}{New GeoPackage beside dsn. Never overwritten.}
\item{level}{stream or reach.}
\item{feature_id}{Exact saved identity.}
\item{name}{Nonempty replacement name, unique within its parent ignoring case.}
\item{report_purpose}{definition, terrain or staging.}
}
\description{Changes only the display name in a new revision, retaining geometry,
identities, relationships, source evidence and earlier revisions. No stationing
or automatic numbering is performed. Unchanged names are not a new revision.}
\value{Revision context and report paths.}
