#' Crosswalk bibliographic metadata to schema.org (dataspice-ready)
#'
#' @param x A `dublincore` or `datacite` metadata record
#' @param schema One of "schema.org" (default), "dataspice"
#' @return A named list that matches schema.org/Dataset JSON-LD
#' @keywords import
# ---- helpers ---------------------------------------------------------------

# Map R/CRAN-style roles to a compact schema-ish vocabulary


# Format utils::person to plain name, stripping trailing "[role]" that format() adds
clean_person_name <- function(p) {
  nm <- format(p)
  sub("\\s*\\[[^]]+\\]$", "", nm)
}

# Safe extractor for structured subject and relation
.extract_subject_term <- function(x) {
  subj <- attr(x, "subject", exact = TRUE)
  if (!is.null(subj) && inherits(subj, "subject") && length(subj$term)) {
    subj$term
  } else if (!is.null(x$subject) && length(x$subject)) {
    as.character(x$subject)
  } else {
    NA_character_
  }
}

.extract_related_identifier <- function(x) {
  rel <- attr(x, "relation", exact = TRUE)
  if (!is.null(rel) && inherits(rel, "related") && length(rel$relatedIdentifier)) {
    rel$relatedIdentifier
  } else if (!is.null(x$relation) && length(x$relation)) {
    as.character(x$relation)
  } else {
    NA_character_
  }
}

# ---- main crosswalk --------------------------------------------------------

#' Crosswalk bibliographic metadata to schema.org / dataspice
#'
#' @param x A `dublincore` or `datacite` object produced by your helpers
#' @param target One of "schema.org" or "dataspice"
#' @return For "schema.org": a named list ready to be JSON‑LD.
#'         For "dataspice": a list with data frames: $biblio, $creators, $contributors.
crosswalk_bib <- function(x, target = c("schema.org", "dataspice")) {
  target <- match.arg(target)

  if (!(inherits(x, "dublincore") || inherits(x, "datacite"))) {
    stop("Unsupported metadata class")
  }

  # Unified view over both classes
  unified <- list(
    title = x$title,
    description = x$description %||% NA_character_,
    identifier = x$identifier %||% NA_character_,
    creator = if (is.null(x$author)) {
      list()
    } else {
      # ensure a list of person
      if (inherits(x$author, "person")) as.list(x$author) else x$author
    },
    contributor = attr(x, "contributor", exact = TRUE) %||% list(),
    publisher = x$publisher %||% NA_character_,
    date = x$date %||% x$year %||% NA_character_,
    language = x$language %||% NA_character_,
    keywords = .extract_subject_term(x),
    relation = .extract_related_identifier(x)
  )

  if (identical(target, "schema.org")) {
    sch <- list(
      "@context" = "https://schema.org",
      "@type" = "Dataset",
      name = unified$title,
      description = unified$description,
      identifier = unified$identifier,
      publisher = if (!is.na(unified$publisher) && nzchar(unified$publisher)) {
        list("@type" = "Organization", name = unified$publisher)
      } else {
        NULL
      },
      datePublished = unified$date,
      inLanguage = unified$language
    )

    # keywords (single string or NA -> omit)
    if (!is.na(unified$keywords) && nzchar(unified$keywords)) {
      sch$keywords <- unified$keywords
    }

    # isBasedOn (from relation)
    if (!is.na(unified$relation) && nzchar(unified$relation)) {
      sch$isBasedOn <- unified$relation
    }

    # creators
    if (length(unified$creator)) {
      sch$creator <- lapply(unified$creator, function(p) {
        list(
          "@type" = "Person",
          name    = clean_person_name(p),
          role    = map_role_to_schema(p$role[1] %||% "cre")
        )
      })
    }

    # contributors
    if (length(unified$contributor)) {
      # normalize single person to list
      contr_list <- if (inherits(unified$contributor, "person")) {
        as.list(unified$contributor)
      } else {
        unified$contributor
      }

      if (length(contr_list)) {
        sch$contributor <- lapply(contr_list, function(p) {
          list(
            "@type" = "Person",
            name    = clean_person_name(p),
            role    = map_role_to_schema(p$role[1] %||% "ctb")
          )
        })
      }
    }

    return(sch)
  }

  # dataspice: return tidy data frames
  # creators df (can be 0 rows)
  creators_df <- if (length(unified$creator)) {
    data.frame(
      name = vapply(unified$creator, clean_person_name, character(1)),
      role = vapply(unified$creator, function(p) map_role_to_schema(p$role[1] %||% "cre"), character(1)),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(name = character(0), role = character(0), stringsAsFactors = FALSE)
  }

  # contributors df (can be 0 rows)
  contr_vec <- unified$contributor
  if (inherits(contr_vec, "person")) contr_vec <- as.list(contr_vec)
  contributors_df <- if (length(contr_vec)) {
    data.frame(
      name = vapply(contr_vec, clean_person_name, character(1)),
      role = vapply(contr_vec, function(p) map_role_to_schema(p$role[1] %||% "ctb"), character(1)),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(name = character(0), role = character(0), stringsAsFactors = FALSE)
  }

  # biblio df (always 1 row; use NA when missing)
  biblio_df <- data.frame(
    title = unified$title %||% NA_character_,
    description = unified$description %||% NA_character_,
    id = unified$identifier %||% NA_character_,
    publisher = unified$publisher %||% NA_character_,
    date = unified$date %||% NA_character_,
    language = unified$language %||% NA_character_,
    keywords = unified$keywords %||% NA_character_,
    relation = unified$relation %||% NA_character_,
    stringsAsFactors = FALSE
  )

  list(
    biblio       = biblio_df,
    creators     = creators_df,
    contributors = contributors_df
  )
}
