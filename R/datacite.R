#' Create a Bibentry Object with DataCite Metadata Fields
#'
#' Constructs a bibliographic metadata record conforming to the
#' [DataCite Metadata Schema](https://schema.datacite.org/). The resulting
#' object is stored as a modified [utils::bibentry()] enriched with structured
#' Dublin Core and DataCite-compliant metadata.
#'
#' @details
#' DataCite is a leading non-profit organization that provides persistent
#' identifiers (DOIs) for research data and other research outputs. Members of
#' the research community use DataCite to register datasets with globally
#' resolvable metadata for citation and discovery.
#'
#' This function sets `"Dataset"` as the default resource type. The `Size`
#' attribute (e.g., bytes, pages, etc.) is automatically added if available.
#'
#' @param Title The name(s) by which the resource is known. Similar to
#'   [dct:title](https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/title/).
#' @param Creator One or more [utils::person()] objects describing the main
#'   authors or contributors responsible for creating the resource.
#' @param Identifier A persistent identifier (e.g., DOI or URI). May refer to a
#'   specific version or all versions of the resource.
#' @param Publisher The name of the organization that holds, publishes, or
#'   distributes the resource. Required by DataCite. See [publisher()].
#' @param PublicationYear The year of public availability (in `YYYY` format).
#'   See [publication_year()].
#' @param Subject A topic, keyword, or classification term. See [subject()] and
#'   [subject_create()] for structured vocabularies.
#' @param Contributor An individual or institution that contributed to the
#'   development, distribution, or curation of the resource.
#' @param Type The resource type. Defaults to `"Dataset"` for general use. See
#'   [dcm:type](https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/type/).
#' @param Language Language code as per IETF BCP 47 / ISO 639-1. See [language()].
#' @param AlternateIdentifier Optional local or secondary identifier. Defaults
#'   to `":unas"`.
#' @param RelatedIdentifier Related resources (e.g., prior versions, papers).
#'   Defaults to `":unas"`.
#' @param Format A technical format (e.g., `"application/pdf"`, `"text/csv"`).
#' @param Version A free-text version string (e.g., `"1.0.0"`). Defaults to
#'   `"0.1.0"`. See [version()].
#' @param Rights Licensing or usage restrictions for the resource. Defaults to
#'   `":tba"`. See [rights()].
#' @param Date A date in `"YYYY"`, `"YYYY-MM-DD"` or ISO datetime format.
#'   Can also be a [Date] or [POSIXct] object.
#' @param DateList A list of multiple dates. Currently not supported.
#' @param Description Free-text summary or additional information. Defaults to
#'   `":tba"`.
#' @param Geolocation Geographic location covered or referenced by the resource.
#'   See [geolocation()].
#' @param FundingReference Information about funding or financial support.
#'   Defaults to `":unas"`. Structured funding metadata not yet implemented.
#'
#' @return
#' A [utils::bibentry()] object with DataCite-compliant fields. Use
#' [as_datacite()] to extract the metadata as a list or bibentry object.
#'
#' @source
#' - [DataCite 4.3 Mandatory Properties](https://support.datacite.org/docs/schema-mandatory-properties-v43)
#' - [DataCite 4.3 Optional Properties](https://support.datacite.org/docs/schema-optional-properties-v43)
#'
#' @family bibrecord functions
#' @seealso
#' Learn more in the vignette:
#' [bibrecord](https://dataset.dataobservatory.eu/articles/bibrecord.html)
#' @importFrom utils person bibentry
#'
#' @examples
#' datacite(
#'   Title = "Growth of Orange Trees",
#'   Creator = c(
#'     person(
#'       given = "N.R.",
#'       family = "Draper",
#'       role = "cre",
#'       comment = c(VIAF = "http://viaf.org/viaf/84585260")
#'     ),
#'     person(
#'       given = "H",
#'       family = "Smith",
#'       role = "cre"
#'     )
#'   ),
#'   Publisher = "Wiley",
#'   Date = 1998,
#'   Language = "en"
#' )
#'
#' # Extract bibliographic metadata
#' as_datacite(orange_df)
#'
#' # As a list
#' as_datacite(orange_df, "list")
#'
#' @export

datacite <- function(Title,
                     Creator,
                     Identifier = NULL,
                     Publisher = NULL,
                     PublicationYear = NULL,
                     Subject = subject_create(
                       term = "data sets",
                       subjectScheme = "Library of Congress Subject Headings (LCSH)",
                       schemeURI = "https://id.loc.gov/authorities/subjects.html",
                       valueURI = "http://id.loc.gov/authorities/subjects/sh2018002256"
                     ),
                     Type = "Dataset",
                     Contributor = NULL,
                     Date = ":tba",
                     DateList = NULL,
                     Language = NULL,
                     AlternateIdentifier = ":unas",
                     RelatedIdentifier = ":unas",
                     Format = ":tba",
                     Version = "0.1.0",
                     Rights = ":tba",
                     Description = ":tba",
                     Geolocation = ":unas",
                     FundingReference = ":unas") {
  if (is.null(PublicationYear)) {
    if (!is.null(Date) && grepl("^\\d{4}", as.character(Date))) {
      PublicationYear <- substr(as.character(Date), 1, 4)
    } else {
      PublicationYear <- ":tba"
    }
  }

  Date <- ifelse(is.null(DateList), ":tba", as.character(Date))
  DateList <- ifelse(is.null(DateList), ":tba", as.character(DateList))
  Format <- ifelse(is.null(Format), ":tba", as.character(Format))
  AlternateIdentifier <- ifelse(is.null(AlternateIdentifier),
    ":unas", AlternateIdentifier
  )
  if (is.null(RelatedIdentifier)) {
    RelatedIdentifie <- ":unas"
  }
  Rights <- ifelse(is.null(Rights), ":tba", as.character(Rights))
  Geolocation <- ifelse(is.null(Geolocation), ":unas",
    as.character(Geolocation)
  )
  FundingReference <- ifelse(is.null(FundingReference),
    ":unas", as.character(FundingReference)
  )

  new_datacite(
    Title = Title,
    Creator = Creator,
    Identifier = Identifier,
    Publisher = Publisher,
    PublicationYear = PublicationYear,
    Subject = Subject,
    Type = "Dataset",
    Contributor = Contributor,
    Date = Date,
    DateList = DateList,
    Language = Language,
    AlternateIdentifier = AlternateIdentifier,
    RelatedIdentifier = RelatedIdentifier,
    Format = Format,
    Version = Version,
    Rights = Rights,
    Description = Description,
    Geolocation = Geolocation,
    FundingReference = FundingReference
  )
}

#' @keywords internal
new_datacite <- function(Title,
                         Creator,
                         Identifier,
                         Publisher,
                         PublicationYear,
                         Subject,
                         Type = "Dataset",
                         Contributor,
                         Date,
                         DateList,
                         Language,
                         AlternateIdentifier,
                         RelatedIdentifier,
                         Format,
                         Version,
                         Rights,
                         Description,
                         Geolocation,
                         FundingReference) {
  # bibentry: subject slot must be character
  datacite_object <- bibentry(
    bibtype = "Misc",
    title = Title,
    author = Creator,
    year = as.character(PublicationYear),
    identifier = Identifier,
    publisher = Publisher,
    date = Date,
    language = Language,
    subject = if (is.subject(Subject)) Subject$term else as.character(Subject),
    alternateidentifier = AlternateIdentifier,
    relatedidentifier = if (is.related(RelatedIdentifier)) {
      RelatedIdentifier$relatedIdentifier
    } else if (is.null(RelatedIdentifier)) {
      ":unas"
    } else {
      as.character(RelatedIdentifier)
    },
    format = Format,
    version = Version,
    rights = Rights,
    description = Description,
    geolocation = Geolocation,
    fundingreference = FundingReference
  )

  # keep the structured subject object as an attribute
  if (!is.null(Subject) && is.subject(Subject)) {
    attr(datacite_object, "subject") <- Subject
  }

  # keep the structured related item object as an attribute
  if (!is.null(RelatedIdentifier) && is.related(RelatedIdentifier)) {
    attr(datacite_object, "relatedIdentifier") <- RelatedIdentifier
  }

  # add structured contributor list
  if (!is.null(Contributor)) {
    attr(datacite_object, "contributor") <- Contributor
  }

  class(datacite_object) <- c("datacite", class(datacite_object))
  datacite_object
}

#' @rdname datacite
#' @return[is.datacite()] returns a logical values (if the object
#' `x` is of class `datacite`).
is.datacite <- function(x) {
  UseMethod("is.datacite", x)
}

#' @rdname datacite
#' @param x An object that is tested if it has a class "datacite".
#' @exportS3Method
is.datacite.datacite <- function(x) inherits(x, "datacite")

#' @rdname datacite
#' @exportS3Method
print.datacite <- function(x, ...) {
  cat("DataCite Metadata Record\n")
  cat("--------------------------\n")

  pr <- function(label, value) {
    if (!is.null(value) && length(value) > 0 && any(nzchar(value))) {
      # %-12s = left-justify label into 12 chars
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

  pr("Identifier", x$identifier)
  pr("Publisher", x$publisher)
  pr("Year", x$year)
  pr("Language", x$language)
  pr("Description", x$description)

  invisible(x)
}
