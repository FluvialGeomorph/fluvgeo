.fg_report_terrain_sources <- function(x, inventory = NULL, retained = NULL, processing = NULL) {
  if (is.null(x)) return(invisible(NULL))
  state <- c(CANDIDATE = "Candidate; use not established",
    RECORDED_USE = "Recorded use; attributed account", REJECTED = "Rejected association")
  action <- c(CANDIDATE = "Seek project records or attributed recollection of actual use; overlap alone is insufficient.",
    RECORDED_USE = "Resolve source edition, exact inputs and preparation evidence before relying on lineage or comparability.",
    REJECTED = "Retain the rejection and its rationale; do not reuse this association automatically.")
  label <- x$artifact_id
  if (!is.null(inventory)) {
    i <- match(x$artifact_id, inventory$artifacts$artifact_id)
    label <- inventory$artifacts$path[i]
  }
  cat('<h3>Terrain source-use evidence</h3><p>These are attributed accounts, not verified processing history. Catalog matches do not establish use, independent surveys or exact source bytes. No event dates, terrain references or scientific findings were changed.</p>')
  .fg_report_table(data.frame(Terrain = label, Source = x$source_description,
    Status = unname(state[x$status]), `Next action` = unname(action[x$status]), check.names = FALSE))
  cat('<details><summary>Source identities, evidence and attribution</summary>')
  for (i in seq_len(nrow(x))) {
    .fg_report_table(data.frame(Item = c("Claim / target artifact", "Target SHA-256", "Source namespace / record",
      "Source edition", "Consulted metadata / archive record", "Basis", "Evidence and qualifications", "Recorder / recorded UTC"),
      Detail = c(paste(x$association_id[i], x$artifact_id[i], sep = " / "), x$artifact_sha256[i],
        paste(x$source_catalog[i], x$source_record_id[i], sep = " / "),
        if (is.na(x$source_version[i])) "Unresolved; exact source edition not established" else x$source_version[i],
        x$source_snapshot[i], if (x$basis[i] == "PROJECT_RECORD") "Supplied project record" else "Owner recollection; seek corroboration",
        x$evidence[i], paste(x$analyst[i], x$recorded_at[i], sep = " / "))))
  }
  cat('<p>Source references are retained text, not fetched or fingerprinted source assets. The target hash identifies the inventoried derivative, not a source product. Retain prior context snapshots when revising these accounts.</p></details>')
  if (!is.null(processing)) .fg_report_terrain_processing(processing, x, retained)
  if (!is.null(retained)) {
    states <- c(MATCH = "Retained bytes match", MISSING = "Retained file missing",
      CHANGED = "Retained file changed", UNREADABLE = "Retained file unreadable")
    actions <- c(MATCH = "Review the record's relevance and limitations; a checksum does not verify its account.",
      MISSING = "Restore the retained file from a trusted copy; do not substitute another edition.",
      CHANGED = "Recover the original bytes or retain the new evidence separately; do not overwrite history.",
      UNREADABLE = "Check file access, then repeat this review.")
    cat('<h3>Retained supporting records</h3><p>These selected files preserve consulted metadata or supplied processing documentation. They do not prove processing execution, source-product identity or scientific comparability. No attachment is opened or executed by this report.</p>')
    .fg_report_table(data.frame(Claim = retained$association_id, Record = retained$description,
      Kind = ifelse(retained$kind == "METADATA_SNAPSHOT", "Metadata snapshot", "Processing record"),
      Integrity = unname(states[retained$integrity]),
      `Next action` = unname(actions[retained$integrity]), check.names = FALSE))
    cat('<details><summary>Retained file identities, references and qualifications</summary>')
    for (i in seq_len(nrow(retained))) .fg_report_table(data.frame(
      Item = c("Evidence / claim", "Original filename", "Retained path (relative to terrain manifest)",
        "Retained SHA-256", "Original source reference", "Qualifications", "Recorder / retained UTC"),
      Detail = c(paste(retained$evidence_id[i], retained$association_id[i], sep = " / "),
        retained$original_name[i], retained$path[i], retained$sha256[i], retained$source_reference[i],
        retained$qualifications[i], paste(retained$analyst[i], retained$retained_at[i], sep = " / "))))
    cat('</details>')
  }
}

.fg_report_terrain_processing <- function(x, sources, retained) {
  known <- function(v) ifelse(is.na(v), "Unknown; not recovered", v)
  cat('<h3>Recorded terrain preparation</h3><p>These are analyst-supplied accounts. Step order, inputs, parameters and outputs are not independently verified. Blank archive details stay unknown; no operation is executed and no source claim is promoted.</p>')
  for (id in unique(x$processing_id)) {
    y <- x[x$processing_id == id, , drop = FALSE]
    y <- y[order(y$step_number), , drop = FALSE]
    source <- sources[match(y$association_id[1], sources$association_id), , drop = FALSE]
    missing <- .fg_terrain_processing_step_fields()
    missing <- missing[vapply(y[missing], anyNA, logical(1))]
    action <- if (source$status == "REJECTED")
      "Source association rejected: retain this account as history; do not rely on it as current lineage." else
      if (length(missing)) paste0("Recover where feasible: ",
        paste(gsub("_", " ", missing), collapse = ", "),
        ". Leave unsupported details unknown; assess source use and comparability separately.") else
      "Review support for this account and assess source use and comparability separately; populated fields do not verify execution."
    integrity <- "No retained processing document linked"
    if (!is.na(y$evidence_id[1])) {
      i <- match(y$evidence_id[1], retained$evidence_id)
      integrity <- paste("Retained document:", if (is.na(i)) "not inspected" else retained$integrity[i])
      if (!is.na(i) && retained$integrity[i] != "MATCH")
        action <- paste("Resolve the retained document's integrity finding before relying on it.", action)
    }
    .fg_report_table(data.frame(Item = c("Account / source claim", "Inventoried target", "What is known", "What to do next"),
      Detail = c(paste(id, y$association_id[1], sep = " / "), source$artifact_id,
        paste(nrow(y), "declared step(s);", if (y$basis[1] == "PROJECT_RECORD") "project-record account." else "owner recollection.", integrity), action)))
    .fg_report_table(data.frame(Step = y$step_number, Operation = y$operation,
      Inputs = known(y$input_description), Outputs = known(y$output_description)))
    cat('<details><summary>Parameters, software and account qualifications</summary>')
    .fg_report_table(data.frame(Step = y$step_number, Parameters = known(y$parameters),
      Software = known(y$software), Version = known(y$software_version),
      `Reported execution time` = known(y$execution_time), check.names = FALSE))
    .fg_report_table(data.frame(Item = c("Qualifications / support", "Linked evidence ID", "Recorder / recorded UTC"),
      Detail = c(y$qualifications[1], known(y$evidence_id[1]), paste(y$analyst[1], y$recorded_at[1], sep = " / "))))
    cat('</details>')
  }
}
