#' @title Add or retrieve related items (DataCite/Dublin Core)
#'
#' @description Manage related resources for a dataset using a unified accessor.
#' - For **DataCite 4.x**, this maps to `relatedIdentifier` (+ type & relation).
#' - For **Dublin Core**, this maps to `dct:relation` (string).
#'
#' @details To remain compatible with [utils::bibentry()], the bibentry stores
#' only the
#' **string identifier** (e.g., DOI/URL). The full structured object created by
#' [related_create()] is preserved in the `"relation"` attribute.
#'
#' A `"related"` object is a small S3 list with the following elements:
#' - `relatedIdentifier`: the related resource identifier (DOI, URL, etc.)
#' - `relationType`: the DataCite relation type (e.g., `"IsPartOf"`, `"References"`)
#' - `relatedIdentifierType`: the type of identifier (`"DOI"`, `"URL"`, etc.)
#' - `resourceTypeGeneral`: optional, the general type of the related resource (e.g., `"Text"`, `"Dataset"`)
#'
#' @param x A dataset object created with [dataset_df()] or [as_dataset_df()].
#' @param value A `related` object from [related_create()] or a character.
#'   Vectors of characters are also supported and will be converted to a list of
#'   `"related"` objects.
#' @param relatedIdentifier A string with the identifier of the related
#'   resource.
#' @param relationType A string naming the relation type (per DataCite
#'   vocabulary).
#' @param relatedIdentifierType A string naming the identifier type (`"DOI"`,
#'   `"URL"`, etc.).
#' @param resourceTypeGeneral Optional: a string naming the general type of the
#'   related resource.
#'
#' @return
#' * `relation(x)` returns:
#'   - a single structured `"related"` object (from [related_create()]) if only
#' one relation is present,
#'   - a list of `"related"` objects if multiple relations are present,
#'   - otherwise it falls back to the bibentry field (`relatedidentifier` for
#' DataCite or `relation` for Dublin Core).
#' * `relation(x) <- value` sets the `"relation"` attribute (structured object
#' or list of objects) and the bibentry string fields (`relatedidentifier` and
#' `relation`), and returns the dataset invisibly.
#' * `related_create()` constructs a structured `"related"` object.
#' * `is.related(x)` returns `TRUE` if `x` inherits from class `"related"`.
#'
#' @examples
#' df <- dataset_df(data.frame(x = 1))
#' relation(df) <- related_create(
#'   relatedIdentifier = "10.1234/example",
#'   relationType = "IsPartOf",
#'   relatedIdentifierType = "DOI"
#' )
#' relation(df) # structured object
#' get_bibentry(df)$relation # "10.1234/example"
#' get_bibentry(df)$relatedidentifier # "10.1234/example"
#'
#' # Character input is normalized to a DOI/URL with default types
#' relation(df) <- "https://doi.org/10.5678/xyz"
#' relation(df) # structured object (relationType/Type filled with defaults)
#'
#' # Create related object directly
#' rel <- related_create("https://doi.org/10.5678/xyz", "References", "DOI")
#' is.related(rel) # TRUE
#'
#' @family bibliographic helper functions
#' @export
relation <- function(x) {
  assertthat::assert_that(
    is.dataset_df(x),
    msg = "relation(x): x must be a dataset_df object."
  )

  rel_attr <- attr(x, "relation", exact = TRUE)
  if (!is.null(rel_attr)) {
    if (is.related(rel_attr)) {
      return(rel_attr)
    }
    if (is.list(rel_attr) && all(vapply(rel_attr, is.related, logical(1)))) {
      return(if (length(rel_attr) == 1) rel_attr[[1]] else rel_attr)
    }
    return(rel_attr)
  }

  be <- get_bibentry(x)
  if (!is.null(be$relatedidentifier)) {
    return(be$relatedidentifier)
  }
  if (!is.null(be$relation)) {
    return(be$relation)
  }

  message("No related item is recorded.")
  NULL
}

#' @rdname relation
#' @export
`relation<-` <- function(x, value) {
  assertthat::assert_that(
    is.dataset_df(x),
    msg = "relation<-(x, value): x must be a dataset_df object created with dataset_df() or as_dataset_df()."
  )

  be <- get_bibentry(x)

  # --- normalize input ---
  if (is.null(value)) {
    value <- list(related_create(":unas", "IsPartOf", "URL"))
  } else if (is.character(value)) {
    # allow vector of strings too
    value <- lapply(value, function(v) {
      related_create(
        relatedIdentifier = v,
        relationType = "IsPartOf",
        relatedIdentifierType = if (grepl("^https?://", v)) "URL" else "DOI"
      )
    })
  } else if (is.related(value)) {
    value <- list(value) # wrap single related in a list
  } else if (is.list(value) && all(vapply(value, is.related, logical(1)))) {
    # already a list of related objects <U+2192> nothing to do
  } else {
    stop("relation(x, value)<- : value must be created with `related_create()` or be a character string (or list thereof).")
  }

  # --- store in dataset_bibentry ---
  # keep only the identifiers (vector of strings)
  be$relatedidentifier <- vapply(value, function(v) v$relatedIdentifier, character(1))
  be$relation <- be$relatedidentifier
  attr(be, "relation") <- value # structured list of related items

  attr(x, "dataset_bibentry") <- be
  attr(x, "relation") <- value

  invisible(x)
}

#' @rdname relation
#' @export
related_create <- function(relatedIdentifier,
                           relationType,
                           relatedIdentifierType,
                           resourceTypeGeneral = NULL) {
  rel <- list(
    relatedIdentifier = as.character(relatedIdentifier),
    relationType = as.character(relationType),
    relatedIdentifierType = as.character(relatedIdentifierType),
    resourceTypeGeneral = if (is.null(resourceTypeGeneral)) NULL else as.character(resourceTypeGeneral)
  )
  class(rel) <- c("related", "list")
  rel
}

#' @rdname relation
#' @export
is.related <- function(x) inherits(x, "related")

#' @rdname relation
#' @export
related_item <- relation

#' @rdname relation
#' @export
`related_item<-` <- `relation<-`
