#' @title Get or Set the Identifier of a Dataset or Metadata Record
#'
#' @description Retrieve or assign the `identifier` attribute of a dataset or
#' bibliographic metadata object.
#'
#' @details An *identifier* provides an unambiguous reference to a resource.
#' Recommended practice is to supply a persistent identifier string, such as a
#' DOI, ISBN, or URN, that conforms to a recognized identification system.
#'
#' Both [Dublin
#' Core](https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#identifier)
#' and [DataCite
#' 4.4](https://support.datacite.org/docs/datacite-metadata-schema-v44-mandatory-properties#1-identifier)
#' define `identifier` as a core property. If the identifier is a DOI, it will
#' also be stored in the `doi` field of the metadata record.
#'
#' Although `identifier` is not part of the minimal Dublin Core term set, it is
#' always included in `dataset` metadata for compatibility with publishing and
#' indexing systems. You may omit it if working under a strict DC profile.
#'
#' For best practice in choosing identifier schemes, see the [IANA-registered
#' URI schemes](https://www.iana.org/assignments/uri-schemes/uri-schemes.xhtml).
#'
#' @param x A [dataset_df()] object or a [utils::bibentry()] object (including
#'   [dublincore()] or [datacite()] records).
#' @param value A character string giving the identifier. Can be named (e.g.,
#'   `c(doi = "...")`) or unnamed. Numeric values are coerced to character.
#' @param overwrite Logical. If `TRUE` (default), any existing identifier is
#'   replaced. If `FALSE`, an existing identifier is preserved unless it is
#'   `":unas"` or `":tba"`.
#'
#' @return For `identifier()`, the current identifier as a character string. For
#' `identifier<-()`, the updated object (invisible).
#'
#' @examples
#' orange_copy <- orange_df
#'
#' # Get the current identifier
#' identifier(orange_copy)
#'
#' # Set a new identifier (e.g., a DOI)
#' identifier(orange_copy) <- "https://doi.org/10.9999/example.doi"
#'
#' # Prevent accidental overwrite
#' identifier(orange_copy, overwrite = FALSE) <- "https://example.org/id"
#'
#' # Use numeric and NULL values
#' identifier(orange_copy) <- 12345
#' identifier(orange_copy) <- NULL # Sets ":unas"
#'
#' @family Reference metadata functions
#' @rdname identifier
#' @export

identifier <- function(x) {
  if (!(inherits(x, "bibentry") || inherits(x, "dataset_df"))) {
    stop("identifier(x): x must be a dataset_df or a bibentry object.")
  }

  if (is.dataset_df(x)) {
    ds_bibentry <- get_bibentry(x)
  } else {
    ds_bibentry <- x
  }

  ds_bibentry$identifier
}

#' @rdname identifier
#' @export
`identifier<-` <- function(x, overwrite = TRUE, value) {
  if (is.numeric(value)) value <- as.character(value)

  if (!(inherits(x, "bibentry") || inherits(x, "dataset_df"))) {
    stop("identifier(x): x must be a dataset_df or a bibentry object.")
  }

  if (!(is.null(value) || inherits(value, "character"))) {
    stop("identifier(x) <- value: value must be a named or not named character string of length 1.")
  }

  if (is.dataset_df(x)) {
    ds_bibentry <- get_bibentry(x)
  } else {
    ds_bibentry <- x
  }

  old_identifier <- ds_bibentry$identifier

  if (is.null(value)) {
    value <- ":unas"
  }

  is_doi <- function(i) {
    if (!is.null(names(i))) {
      if (tolower(names(i)) == "doi") {
        return(TRUE)
      }
    } else {
      ifelse(grepl("https://doi.org", i), TRUE, FALSE)
    }
  }

  if (overwrite || old_identifier %in% c(":unas", ":tba")) {
    ds_bibentry$identifier <- value
    if (is_doi(value)) {
      doi <- gsub("https://doi.org/", "", value)
      doi <- gsub("/$", "", doi)
      ds_bibentry$doi <- doi
    }
  } else {
    warning(
      "The dataset has already an identifier: ",
      old_identifier, ".\nYou can overwrite this message with identifier(x, overwrite = TRUE) <- value"
    )
  }

  if (inherits(x, "bibentry")) {
    ds_bibentry
  } else {
    if (is_doi(value)) attr(x, "doi") <- gsub("https://doi.org/", "", value)
    attr(x, "dataset_bibentry") <- ds_bibentry
    invisible(x)
  }
}
