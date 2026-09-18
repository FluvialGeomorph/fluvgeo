\name{order_drainage_flowlines}
\alias{order_drainage_flowlines}
\title{Order reference flowlines for upstream or downstream browsing}
\usage{order_drainage_flowlines(lines, comid = NULL, direction = "upstream",
  id_column = "nhdplus_comid")}
\arguments{
\item{lines}{CRS-defined sf LINESTRING reference features digitized downstream.}
\item{comid}{Identifier of the navigation origin, present in lines. NULL orders
the whole directed network topologically in the requested direction.}
\item{direction}{"upstream" or "downstream", away from the origin.}
\item{id_column}{Column containing unique source identifiers.}
}
\description{Uses sfnetworks endpoint connectivity and igraph depth-first
traversal to keep branches together. The origin segment leads the list;
source IDs break branch ties deterministically, without inferring a mainstem.
Intended for NLDI/NHDPlus reference linework, not arbitrary extracted networks.
With NULL origin, upstream means downstream-to-upstream across the whole graph;
all downstream dependencies precede upstream edges. Separate branches/components
have deterministic ordering, not an inferred stationing route or mainstem.
No snapping, reversal, repair, geometric intersection at crossings, network
completeness or scientific acceptance is inferred. Source geometry is unchanged.}
\value{Data frame in browsing order with source_row (input row), source_id,
navigation_order and order_status. Ordered features come first. Unreachable
features (including opposite-direction candidates from earlier queries) follow
in ID order with status unreached. Cycles, missing origins/identifiers and
unsupported geometry return all rows as unresolved, never a fabricated order.}
