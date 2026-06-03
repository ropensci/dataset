#' @rdname datacite
#' @param x A dataset object created with [dataset_df()].
#' @param type A DataCite 4.4 metadata can be returned as:
#'   `"list"`, `"dataset_df"`, `"bibentry"` (default), or `"ntriples"`.
#' @param ... Optional parameters to add to a `datacite` object.
#'   For example, `author = person("Jane", "Doe")` adds an author if
#'   `type = "dataset_df"`.
#' @return `as_datacite(x, type)` returns the DataCite bibliographical metadata
#'   of `x` either as a list, a bibentry object, an N-Triples text serialisation
#'   or a dataset_df object.
#' @export
as_datacite <- function(x, type = "bibentry", ...) {
  citation_author <- person(NULL, NULL)

  is_person <- function(p) inherits(p, "person")

  arguments <- list(...)
  if (!is.null(arguments$author)) {
    if (is_person(arguments$author)) {
      citation_author <- arguments$author
    } else {
      stop("as_datacite(x, ..., author = ): author must be created with utils::person().")
    }
  }

  if (!type %in% c("bibentry", "list", "dataset_df", "ntriples")) {
    warning(paste0(
      "as_datacite(ds, type=...) type cannot be ",
      type, ". Reverting to 'bibentry'."
    ))
    type <- "bibentry"
  }

  ds_bibentry <- get_bibentry(x)
  Title <- ds_bibentry$title
  Creator <- ds_bibentry$author

  if (!is_person(Creator)) {
    stop('attr(x, "dataset_bibentry")$author is not a person object.')
  }

  # Date / Year fallback
  if (!is.null(ds_bibentry$year)) {
    if (is.null(ds_bibentry$dataset_date)) {
      Date <- as.character(ds_bibentry$year)
    } else {
      Date <- as.character(ds_bibentry$date)
    }
    PublicationYear <- as.character(ds_bibentry$year)
  } else if (!is.null(ds_bibentry$date)) {
    Date <- as.character(ds_bibentry$date)
    PublicationYear <- substr(Date, 1, 4)
  } else {
    Date <- ":tba"
    PublicationYear <- ":unas"
  }

  Publisher <- ifelse(is.null(ds_bibentry$publisher), "", as.character(ds_bibentry$publisher))
  Identifier <- ifelse(is.null(ds_bibentry$identifier), ":tba", as.character(ds_bibentry$identifier))
  Version <- ifelse(is.null(ds_bibentry$version), ":unas", as.character(ds_bibentry$version))
  Description <- ifelse(is.null(ds_bibentry$description), ":unas", as.character(ds_bibentry$description))
  Language <- ifelse(is.null(ds_bibentry$language), ":unas", as.character(ds_bibentry$language))
  Format <- ifelse(is.null(ds_bibentry$format), "application/r-rds", as.character(ds_bibentry$format))
  AlternateIdentifier <- ifelse(is.null(ds_bibentry$alternateidentifier), ":unas", ds_bibentry$alternateidentifier)
  RelatedIdentifier <- ifelse(is.null(ds_bibentry$relatedidentifier), ":unas", ds_bibentry$relatedidentifier)
  Rights <- ifelse(is.null(ds_bibentry$rights), ":tba", as.character(ds_bibentry$rights))
  Geolocation <- ifelse(is.null(ds_bibentry$geolocation), ":unas", as.character(ds_bibentry$geolocation))
  FundingReference <- ifelse(is.null(ds_bibentry$fundingreference), ":unas", as.character(ds_bibentry$fundingreference))

  # Contributors <U+2014> check attribute if available
  Contributor <- if (!is.null(attr(ds_bibentry, "contributor"))) {
    attr(ds_bibentry, "contributor")
  } else if (!is.null(ds_bibentry$contributor)) {
    ds_bibentry$contributor
  } else {
    ""
  }

  # Subject handling (always structured or NULL)
  if (is.null(ds_bibentry$subject)) {
    Subject <- NULL
  } else if (is.subject(ds_bibentry$subject)) {
    Subject <- ds_bibentry$subject
  } else if (is.character(ds_bibentry$subject)) {
    Subject <- new_Subject(term = ds_bibentry$subject)
  } else {
    stop("Unsupported subject type in dataset_bibentry$subject")
  }

  if (type == "bibentry") {
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
  } else if (type == "list") {
    if (identical(Contributor, "")) Contributor <- NULL
    if (identical(Subject, "")) Subject <- NULL

    list(
      Title = Title,
      Creator = Creator,
      Identifier = Identifier,
      Publisher = Publisher,
      PublicationYear = PublicationYear,
      Subject = Subject,
      Type = "Dataset",
      Contributor = Contributor,
      Date = Date,
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
  } else if (type == "dataset_df") {
    # Flatten multi-length values to single strings
    collapse_if_needed <- function(x) {
      if (length(x) > 1) paste(as.character(x), collapse = "; ") else x
    }

    flatten_subject <- function(s) {
      if (is.null(s)) {
        return("")
      }
      if (is.subject(s)) {
        # Use only the term for dataset_df export
        return(s$term)
      }
      as.character(s)
    }

    dataset_df(
      data.frame(
        Title = collapse_if_needed(Title),
        Creator = paste(
          vapply(Creator, function(p) p$family, character(1)),
          collapse = "; "
        ),
        Identifier = collapse_if_needed(Identifier),
        Publisher = collapse_if_needed(Publisher),
        PublicationYear = collapse_if_needed(PublicationYear),
        Subject = collapse_if_needed(flatten_subject(Subject)),
        Type = "Dataset",
        Contributor = if (is.null(Contributor)) {
          ":unas"
        } else {
          collapse_if_needed(as.character(Contributor))
        },
        Date = collapse_if_needed(Date),
        Language = collapse_if_needed(Language),
        AlternateIdentifier = collapse_if_needed(AlternateIdentifier),
        RelatedIdentifier = collapse_if_needed(RelatedIdentifier),
        Format = collapse_if_needed(Format),
        Version = collapse_if_needed(Version),
        Rights = collapse_if_needed(Rights),
        Description = collapse_if_needed(Description),
        Geolocation = collapse_if_needed(Geolocation),
        FundingReference = collapse_if_needed(FundingReference)
      ),
      dataset_bibentry = datacite(
        Title = paste0("The DataCite metadata of `", collapse_if_needed(Title), "`"),
        Creator = Creator,
        Identifier = collapse_if_needed(Identifier),
        Date = collapse_if_needed(Date)
      )
    )
  } else if (type == "ntriples") {
    dataset_id <- if (is.null(Identifier) || Identifier == ":tba") {
      "http://example.com/dataset_tba/"
    } else {
      Identifier
    }

    dc_list <- list(
      title = Title,
      creator = paste(vapply(Creator, function(p) p$family, character(1)), collapse = "; "),
      identifier = Identifier,
      publisher = Publisher,
      publicationyear = PublicationYear,
      subject = Subject,
      type = "http://purl.org/dc/dcmitype/Dataset",
      contributor = Contributor,
      date = Date,
      language = Language,
      alternateidentifier = AlternateIdentifier,
      relatedidentifier = RelatedIdentifier,
      format = Format,
      version = Version,
      rights = Rights,
      description = Description,
      geolocation = Geolocation,
      fundingreference = FundingReference
    )

    datacite_to_triples(dc_list, dataset_id = dataset_id)
  }
}
