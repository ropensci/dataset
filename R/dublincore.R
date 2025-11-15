#' Add or Retrieve Dublin Core Metadata
#'
#' Adds or retrieves metadata conforming to the
#' [Dublin Core Metadata Terms](https://www.dublincore.org/specifications/dublin-core/dcmi-terms/)
#' standard, enabling consistent and structured citation and retrieval of R
#' dataset objects.
#'
#' @details
#' The Dublin Core Metadata Element Set (DCMES) is a standardized vocabulary for
#' describing digital and physical resources. It includes 15 core fields and is
#' formally standardized as ISO 15836, IETF RFC 5013, and ANSI/NISO Z39.85.
#'
#' This function constructs a [utils::bibentry()] object extended with DCMI
#' terms and is compatible with [dataset_df()] objects. The resulting metadata
#' can be used for semantic documentation and machine-readable citation.
#'
#' For compatibility with [utils::bibentry()], the `dataset_date` parameter is
#' automatically used to derive both `publication_date` and `year` fields.
#'
#' @param x An object to annotate. Typically a `data.frame`, `tibble`,
#'   or named list; can be added with [`set_bibentry<-`] to a semantically
#'   rich `dataset_df`.
#' @param title A name given to the resource. See [dataset_title()].
#' @param creator One or more [utils::person()] objects representing the
#'   creator(s). See [creator()].
#' @param contributor Additional contributors ([utils::person()]) with optional
#'   roles. See [contributor()].
#' @param publisher A character or [utils::person()] indicating the publishing
#'   entity. See [publisher()].
#' @param dataset_date A publication or release date (`Date`, `POSIXct`, or
#'   character in `YYYY`, `YYYY-MM-DD`, or ISO format).
#' @param year An explicit publication year. If omitted, inferred from
#'   `dataset_date`.
#' @param identifier A unique persistent identifier (e.g., DOI). See [identifier()].
#' @param subject A keyword or controlled vocabulary term. See [subject()] and
#'   [subject_create()].
#' @param description A free-text summary of the dataset. See [description()].
#' @param language ISO 639-1 language code. See [language()].
#' @param rights A string describing intellectual property or usage rights.
#' Use a URI like `"https://creativecommons.org/public-domain/cc0/"`.
#' @param dataset_format The technical format of the dataset (e.g., MIME type).
#' See [dataset_format()].
#' @param relation A related resource (e.g., version, paper, or parent dataset).
#'   Currently only supports an URI, for example,
#'   `"https:://doi.org/10.32614/CRAN.package.dataset"`.
#' @param datasource A URL or label for the original source of the dataset.
#' @param coverage Geographic or temporal extent (spatial/temporal coverage).
#' @param type The resource type. For datasets, use `"Dataset"`. See
#'   [DCMI Type Vocabulary](https://www.dublincore.org/specifications/dublin-core/dcmi-type-vocabulary/).
#' @param ... Additional metadata fields.
#'
#' @return
#' A `bibentry` object extended with class `"bibrecord"`, storing structured
#' Dublin Core metadata. Use [as_dublincore()] to extract the metadata in list,
#' tabular, or RDF form.
#'
#' @source
#' - [DCMI Metadata Terms](https://www.dublincore.org/specifications/dublin-core/dcmi-terms/)
#'
#' @importFrom utils person bibentry
#'
#' @examples
#' orange_bibentry <- dublincore(
#'   title = "Growth of Orange Trees",
#'   creator = c(
#'     person(
#'       given = "N.R.",
#'       family = "Draper",
#'       role = "cre",
#'       comment = c(VIAF = "http://viaf.org/viaf/84585260")
#'     ),
#'     person(given = "H", family = "Smith", role = "cre")
#'   ),
#'   contributor = person(given = "Antal", family = "Daniel", role = "dtm"),
#'   publisher = "Wiley",
#'   datasource = "https://isbnsearch.org/isbn/9780471170822",
#'   dataset_date = 1998,
#'   identifier = "https://doi.org/10.5281/zenodo.14917851",
#'   language = "en",
#'   description = "The Orange data frame has 35 rows and 3 columns of records
#'                  of the growth of orange trees."
#' )
#'
#' # To inspect structured metadata from a dataset_df object:
#' as_dublincore(orange_df, type = "list")
#'
#' @export
#' @seealso
#' Learn more in the vignette:
#' [`bibrecord`](https://dataset.dataobservatory.eu/articles/bibrecord.html)
#' @family bibrecord functions

dublincore <- function(
    title,
    creator,
    contributor = NULL,
    year = NULL,
    publisher = NULL,
    identifier = NULL,
    subject = NULL,
    type = "DCMITYPE:Dataset",
    dataset_date = NULL,
    language = NULL,
    relation = NULL,
    dataset_format = "application/r-rds",
    rights = NULL,
    datasource = NULL,
    description = NULL,
    coverage = NULL) {
  if (missing(creator) || is.null(creator)) {
    stop("dublincore(): A valid `creator` (as person or list of person) is required.")
  }

  if (inherits(creator, "person")) {
    creator <- list(creator)
  }

  if (length(title) != 1) {
    stop("dublincore(): title must be a single character string.")
  }

  creators <- normalize_roles(creator, default_role = "cre")

  dataset_date <- ifelse(is.null(dataset_date), ":tba", as.character(dataset_date))
  identifier <- ifelse(is.null(identifier), ":tba", as.character(identifier))
  dataset_format <- ifelse(is.null(dataset_format), "application/r-rds", as.character(dataset_format))
  if (is.null(relation)) {
    relation <- ":unas"
  }
  rights <- ifelse(is.null(rights), ":tba", as.character(rights))
  coverage <- ifelse(is.null(coverage), ":unas", as.character(coverage))
  datasource <- ifelse(is.null(datasource), ":unas", as.character(datasource))
  publishers <- if (is.null(publisher)) ":unas" else publisher
  contributor <- if (is.null(contributor)) NULL else contributor
  creators <- if (is.null(creator)) creators <- ":tba" else creators <- creator
  year <- if (!is.null(dataset_date)) substr(as.character(dataset_date), 1, 4) else NULL

  publisher <- fix_publisher(publishers = publishers)

  new_dublincore(
    title = title,
    creator = creators,
    identifier = identifier,
    publisher = publisher,
    subject = subject,
    type = type,
    contributor = contributor,
    dataset_date = dataset_date,
    year = year,
    language = language,
    relation = relation,
    dataset_format = dataset_format,
    rights = rights,
    datasource = datasource,
    description = description,
    coverage = coverage
  )
}


#' @keywords internal
new_dublincore <- function(title,
                           creator,
                           identifier = NULL,
                           publisher = NULL,
                           subject = NULL,
                           type = "DCMITYPE:Dataset",
                           contributor = NULL,
                           dataset_date = NULL,
                           year = NULL,
                           language = NULL,
                           relation = NULL,
                           dataset_format = NULL,
                           rights = NULL,
                           datasource = NULL,
                           description = NULL,
                           coverage = NULL) {
  year <- if (!is.null(dataset_date)) {
    substr(as.character(dataset_date), 1, 4)
  } else {
    NULL
  }

  # flatten relation for bibrecord
  relation_flat <- if (is.related(relation)) {
    relation$relatedIdentifier
  } else {
    relation
  }

  dublincore_object <- bibrecord(
    title       = title,
    author      = creator,
    identifier  = identifier,
    publisher   = publisher,
    subject     = if (is.subject(subject)) subject$term else as.character(subject),
    type        = type,
    contributor = contributor,
    date        = dataset_date,
    year        = year,
    language    = language,
    relation    = relation_flat,
    format      = dataset_format,
    rights      = rights,
    datasource  = datasource,
    description = description,
    coverage    = coverage
  )

  # preserve structured subject
  if (!is.null(subject) && is.subject(subject)) {
    attr(dublincore_object, "subject") <- subject
  }

  # preserve structured relation
  if (!is.null(relation) && is.related(relation)) {
    attr(dublincore_object, "relation") <- relation
  }

  if (!is.null(contributor)) {
    attr(dublincore_object, "contributor") <- contributor
  }

  class(dublincore_object) <- c("dublincore", class(dublincore_object))
  dublincore_object
}


#' @rdname dublincore
#'
#' @description
#' `is.dublincore()` checks whether an object inherits from the `"dublincore"`
#' class.
#'
#' @param x An object to test.
#'
#' @return
#' A logical value: `TRUE` if `x` is a Dublin Core metadata record (i.e.,
#' inherits from `"dublincore"`), otherwise `FALSE`.
#'
#' @export
is.dublincore <- function(x) {
  inherits(x, "dublincore")
}

#' @rdname dublincore
#' @exportS3Method
print.dublincore <- function(x, ...) {
  cat("Dublin Core Metadata Record\n")
  cat("--------------------------\n")

  pr <- function(label, value) {
    if (!is.null(value) && length(value) > 0 && any(nzchar(value))) {
      cat(sprintf("%-12s %s\n", paste0(label, ":"), paste(value, collapse = "; ")))
    }
  }

  pr("Title", x$title)
  pr("Creator(s)", paste(format(x$author), collapse = "; "))

  contributor <- attr(x, "contributor")
  if (!is.null(contributor)) {
    pr("Contributor(s)", fix_contributor(contributor))
  }

  subj <- attr(x, "subject")
  if (!is.null(subj) && is.subject(subj)) {
    subj_val <- subj$term
    if (!is.null(subj$schemeURI) && nzchar(subj$schemeURI)) {
      subj_val <- paste0(subj_val, " [", subj$schemeURI, "]")
    }
    pr("Subject(s)", subj_val)
  } else if (!is.null(x$subject)) {
    pr("Subject(s)", x$subject)
  }

  pr("Publisher", x$publisher)
  pr("Year", x$year)
  pr("Language", x$language)
  pr("Description", x$description)

  invisible(x)
}
