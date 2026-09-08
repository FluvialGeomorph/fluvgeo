# Presentation grouping only: never change the underlying findings or decisions.
.fg_terrain_review_actions <- function(assessment) {
  actions <- data.frame(action_id = integer(), code = character(), stage = character(),
    status = character(), requires_input = logical(), finding_count = integer(),
    entity_count = integer(), next_action = character())
  members <- data.frame(action_id = integer(), assessment_row = integer())
  if (is.null(assessment) || !nrow(assessment))
    return(list(review_actions = actions, review_action_members = members))

  # Completed evidence belongs in the record, not in the pending-work queue.
  pending <- which(assessment$requires_input |
    !assessment$status %in% c("VERIFIED", "CONFIRMED", "REJECTED"))
  keys <- c("code", "stage", "status", "requires_input", "next_action")
  groups <- unique(assessment[pending, keys, drop = FALSE])
  for (i in seq_len(nrow(groups))) {
    same <- vapply(pending, function(j)
      all(vapply(keys, function(k) identical(assessment[[k]][j], groups[[k]][i]), logical(1))),
      logical(1))
    rows <- pending[same]
    actions <- rbind(actions, data.frame(action_id = i, groups[i, c("code", "stage", "status", "requires_input")],
      finding_count = length(rows), entity_count = length(unique(assessment$entity_id[rows])),
      next_action = groups$next_action[i], row.names = NULL))
    members <- rbind(members, data.frame(action_id = i, assessment_row = rows))
  }
  # File blockers must remain visible even when they do not request human input.
  priority <- ifelse(actions$status == "BLOCKED", 1L,
    ifelse(actions$requires_input, 2L, 3L))
  actions <- actions[order(priority, actions$action_id), , drop = FALSE]
  rownames(actions) <- NULL
  list(review_actions = actions, review_action_members = members)
}
