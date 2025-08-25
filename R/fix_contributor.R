#' Format contributors into a citation string
#'
#' Format a list of `utils::person` objects into a compact string, merging roles
#' per person and normalizing names. Contributors without explicit roles are
#' assigned `"ctb"`. If `NULL` or `":unas"` is supplied, returns `":unas"`.
#'
#' @param contributors A vector or list of `person` objects, or `NULL`, or the
#'   character string `":unas"`.
#'
#' @return A single character string, e.g. `"{Jane Doe [dtm, ctb]} and
#'  {John Smith [ctb]}"`.
#' @keywords internal
fix_contributor <- function(contributors = NULL) {
  if (is.null(contributors) ||
      (is.character(contributors)
       && length(contributors) == 1
       && contributors == ":unas")) {
    return(":unas")
  }

  # Normalize into list of persons
  if (inherits(contributors, "person")) {
    contributors_list <- as.list(contributors)
  } else if (is.list(contributors) && all(vapply(contributors, inherits, "person", FUN.VALUE = logical(1)))) {
    contributors_list <- contributors
  } else {
    return(":unas")
  }

  # Group by identity (given|family)
  identity_keys <- vapply(contributors_list, function(p) {
    paste0(p$given %||% "", "|", p$family %||% "")
  }, character(1))

  merged <- vapply(unique(identity_keys), function(key) {
    matches <- contributors_list[identity_keys == key]

    # Collect unique roles (default to ctb)
    roles <- unique(unlist(lapply(matches, function(p) {
      if (is.null(p$role) || length(p$role) == 0 || all(!nzchar(p$role))) {
        "ctb"
      } else {
        p$role
      }
    })))

    base <- matches[[1]]
    full_name <- clean_person_name(base)
    paste0("{", full_name, " [", paste(roles, collapse = ", "), "]}")
  }, character(1))

  paste(merged, collapse = " and ")
}

#' Remove role suffixes from formatted person names
#'
#' @param p A `person` object.
#'
#' @return Character string without role annotations, e.g. `"Jane Doe"`.
#' @keywords internal
clean_person_name <- function(p) {
  name <- format(p)
  sub("\\s*\\[[^]]+\\]$", "", name)
}

#' Map R person roles to schema.org-style roles
#'
#' @param role A character vector of roles (e.g. `"cre"`, `"ctb"`).
#'
#' @return A character vector with schema.org-style roles.
#' @keywords internal
map_role_to_schema <- function(role) {
  role_map <- c(
    cre = "creator",
    aut = "author",
    ctb = "contributor",
    dtm = "editor",
    fnd = "funder",
    cph = "copyrightHolder",
    own = "copyrightHolder",
    pbl = "publisher"
  )
  ifelse(role %in% names(role_map), role_map[role], "contributor")
}
