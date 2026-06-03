#' Create a Modern Metadata Object Compatible with bibentry
#'
#' Constructs a [utils::bibentry()] object extended with Dublin Core and
#' DataCite-compatible fields. This unified structure supports use with
#' functions such as [dublincore()] and [datacite()], and is the internal
#' format for storing rich metadata with datasets.
#'
#' @param title A character string specifying the dataset title.
#' @param author A [utils::person()] or list/vector of person objects. Mapped to
#'   `creator` in DataCite and DCMI.
#' @param contributor Optional list or vector of [utils::person()] objects.
#'   Contributor roles are merged if duplicated.
#' @param publisher A character string or [utils::person()] representing the
#'   publishing entity.
#' @param year Publication year. Automatically derived from `date` if not
#'   provided explicitly.
#' @param date A [Date] object or character string in ISO format.
#' @param identifier A persistent identifier (e.g., DOI or URL).
#' @param subject Optional keyword, tag, or controlled vocabulary term.
#' @param ... Additional fields such as `language`, `format`, `rights`, or
#'   `description`.
#'
#' @return
#' An object of class `"bibrecord"` and `"bibentry"`, suitable for citation and
#' embedding in metadata-aware structures such as [dataset_df()].
#'
#' @examples
#' bibrecord(
#'   title = "Gross domestic product, volumes",
#'   author = person("Eurosat"),
#'   publisher = person("Eurostat"),
#'   identifier = "https://doi.org/10.2908/TEINA011",
#'   date = as.Date("2025-05-20")
#' )
#'
#' @seealso
#' Learn more in the vignette:
#' [bibrecord](https://dataset.dataobservatory.eu/articles/bibrecord.html)
#' @importFrom utils bibentry
#' @family bibrecord functions
#' @export
bibrecord <- function(
  title,
  author,
  contributor = NULL,
  publisher = NULL,
  year = NULL,
  date = Sys.Date(),
  identifier = NULL,
  subject = NULL,
  ...
) {
  stopifnot(inherits(author, "person") ||
    all(vapply(author, inherits, logical(1), "person")))

  # Normalize author roles and ensure vector of class "person"
  author <- normalize_roles(as.list(author))
  author <- do.call("c", author)

  # Normalize contributor if present
  if (!is.null(contributor)) {
    if (inherits(contributor, "person")) {
      contributor <- list(contributor)
    }
    contributor <- normalize_roles(as.list(contributor))
    contributor <- do.call("c", contributor)
  }

  # Infer year from date if missing
  year <- year %||% substr(as.character(date), 1, 4)

  meta <- bibentry(
    bibtype = "Misc",
    title = title,
    author = author,
    year = year,
    date = as.character(date),
    identifier = identifier,
    publisher = publisher,
    subject = subject,
    ...
  )

  if (!is.null(contributor)) {
    attr(meta, "contributor") <- contributor
  }

  class(meta) <- c("bibrecord", class(meta))
  meta
}

#' @keywords internal
`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

#' @export
print.bibrecord <- function(x, ...) {
  NextMethod("print")

  contributors <- attr(x, "contributor")
  if (!is.null(contributors)) {
    cat("\nContributors:\n")
    cat(fix_contributor(contributors), "\n")
  }

  invisible(x)
}
