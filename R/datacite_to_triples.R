#' @keywords internal
datacite_to_triples <- function(dc_list,
                                dataset_id = "http://example.com/dataset") {
  if (is.null(dc_list$title) || nchar(dc_list$title) == 0) {
    stop("datacite_to_triples(): title is required")
  }

  base <- "http://datacite.org/schema/kernel-4/"
  triples <- character()

  triples <- c(triples, n_triple(
    dataset_id,
    paste0(base, "title"), dc_list$title
  ))

  if (!is.null(dc_list$author)) {
    triples <- c(triples, n_triple(
      dataset_id,
      paste0(base, "creator"),
      dc_list$author
    ))
  }

  if (!is.null(dc_list$contributor)) {
    triples <- c(triples, n_triple(
      dataset_id,
      paste0(base, "contributor"),
      dc_list$contributor
    ))
  }

  if (!is.null(dc_list$identifier)) {
    triples <- c(triples, n_triple(
      dataset_id,
      paste0(base, "identifier"),
      dc_list$identifier
    ))
  }

  if (!is.null(dc_list$publisher)) {
    triples <- c(triples, n_triple(
      dataset_id,
      paste0(base, "publisher"),
      dc_list$publisher
    ))
  }

  if (!is.null(dc_list$publicationyear)) {
    triples <- c(triples, n_triple(
      dataset_id,
      paste0(base, "publicationYear"),
      dc_list$publicationyear
    ))
  }

  if (!is.null(dc_list$language)) {
    triples <- c(triples, n_triple(
      dataset_id,
      paste0(base, "language"),
      dc_list$language
    ))
  }

  if (!is.null(dc_list$rights)) {
    triples <- c(triples, n_triple(dataset_id, paste0(base, "rights"), dc_list$rights))
  }

  if (!is.null(dc_list$description)) {
    triples <- c(triples, n_triple(
      dataset_id,
      paste0(base, "descriptions"),
      dc_list$description
    ))
  }

  if (!is.null(dc_list$subject)) {
    subj_value <- dc_list$subject

    if (is.subject(subj_value)) {
      subj_value <- subj_value$term
    } else if (is.list(subj_value)) {
      subj_value <- vapply(subj_value, function(s) {
        if (is.subject(s)) {
          s$term
        } else {
          as.character(s)
        }
      }, character(1))
    } else {
      subj_value <- as.character(subj_value)
    }

    triples <- c(triples, n_triple(
      dataset_id,
      paste0(base, "subjects"),
      subj_value
    ))
  }


  if (!is.null(dc_list$format)) {
    triples <- c(triples, n_triple(
      dataset_id,
      paste0(base, "formats"),
      dc_list$format
    ))
  }

  if (!is.null(dc_list$version)) {
    triples <- c(triples, n_triple(
      dataset_id,
      paste0(base, "version"),
      dc_list$version
    ))
  }

  # --- Related identifiers ---
  rel_attr <- attr(dc_list, "relation", exact = TRUE)
  if (!is.null(rel_attr)) {
    # normalize: single object → list
    if (is.related(rel_attr)) rel_attr <- list(rel_attr)

    if (is.list(rel_attr) && all(vapply(rel_attr, is.related, logical(1)))) {
      for (ri in rel_attr) {
        triples <- c(
          triples,
          n_triple(
            dataset_id,
            paste0(base, "relatedIdentifier"),
            ri$relatedIdentifier
          ),
          n_triple(
            dataset_id,
            paste0(base, "relationType"),
            ri$relationType
          ),
          n_triple(
            dataset_id,
            paste0(base, "relatedIdentifierType"),
            ri$relatedIdentifierType
          )
        )
        if (!is.null(ri$resourceTypeGeneral) && nzchar(ri$resourceTypeGeneral)) {
          triples <- c(
            triples,
            n_triple(
              dataset_id,
              paste0(base, "resourceTypeGeneral"),
              ri$resourceTypeGeneral
            )
          )
        }
      }
    }
  } else if (!is.null(dc_list$relatedidentifier) &&
    dc_list$relatedidentifier != ":unas") {
    triples <- c(
      triples,
      n_triple(
        dataset_id,
        paste0(base, "relatedIdentifier"),
        dc_list$relatedidentifier
      )
    )
  }


  n_triples(triples)
}
