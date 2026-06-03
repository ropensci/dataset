#' @rdname datacite
#' @param x A dataset object created with \code{dataset::\link{dataset}}.
#' @param type A DataCite 4.4  metadata can be returned as a \code{type="list"},
#' a `type="dataset"`, or a `type="bibentry"` (default).
#' @param ... Optional parameters to add to a `datacite` object.
#' \code{author=person("Jane", "Doe")} adds an author to the citation
#' object if `type="dataset"`.
#' @keywords internal
as_datacite <- function(x, type = "bibentry", ...) {
  citation_author <- person(NULL, NULL)

  is_person <- function(p) ifelse(inherits(p, "person"), TRUE, FALSE)

  arguments <- list(...)
  if (!is.null(arguments$author)) {
    if (is_person(arguments$author)) {
      citation_author <- arguments$author
    } else {
      stop("as_datacite(x, ..., author = ): author must be created with utils::person().")
    }
  }

  if (!type %in% c("bibentry", "list", "dataset")) {
    warning_message <- "as_datacite(ds, type=...) type cannot be "
    warning(warning_message, type, ". Reverting to 'bibentry'.")
    type <- "bibentry"
  }

  ds_bibentry <- get_bibentry(x)
  Title <- ds_bibentry$title
  Creator <- ds_bibentry$author
  Publisher <- ifelse(is.null(ds_bibentry$publisher), ":unas", as.character(ds_bibentry$publisher))
  Identifier <- ifelse(is.null(ds_bibentry$identifier), ":tba", as.character(ds_bibentry$identifier))
  Version <- ifelse(is.null(ds_bibentry$version), ":unas", as.character(ds_bibentry$version))
  Description <- ifelse(is.null(ds_bibentry$description), ":unas", as.character(ds_bibentry$description))
  Language <- ifelse(is.null(ds_bibentry$language), ":unas", as.character(ds_bibentry$language))
  DateList <- ifelse(is.null(ds_bibentry$DateList), ":tba", as.character(ds_bibentry$DateList))
  PublicationYear <- ifelse(is.null(ds_bibentry$year), ":unas", as.character(ds_bibentry$year))
  Format <- ifelse(is.null(ds_bibentry$format), ":tba", as.character(ds_bibentry$format))
  AlternateIdentifier <- ifelse(is.null(ds_bibentry$alternateidentifier), ":unas", ds_bibentry$alternateidentifier)
  RelatedIdentifier <- ifelse(is.null(ds_bibentry$relatedidentifier), ":unas", ds_bibentry$relatedidentifier)
  Rights <- ifelse(is.null(ds_bibentry$rights), ":tba", as.character(ds_bibentry$rights))
  Geolocation <- ifelse(is.null(ds_bibentry$geolocation), ":unas", as.character(ds_bibentry$geolocation))
  FundingReference <- ifelse(is.null(ds_bibentry$fundingreference), ":unas", as.character(ds_bibentry$fundingreference))
  Contributor <- ifelse(is.null(ds_bibentry$contributor), "", as.character(ds_bibentry$contributor))
  Subject <- ifelse(is.null(subject(x)), new_Subject(":tba"), subject(x))

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
  } else if (type == "list") {
    list(
      Title = Title,
      Creator = Creator,
      Identifier = Identifier,
      Publisher = Publisher,
      PublicationYear = PublicationYear,
      Subject = Subject,
      Type = "Dataset",
      Contributor = Contributor,
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
  } else if (type == "dataset") {
    dataset_df(
      data.frame(
        Title = Title,
        Creator = as.character(Creator),
        Identifier = Identifier,
        Publisher = Publisher,
        PublicationYear = PublicationYear,
        Subject = ifelse(is.null(Subject), "", as.character(Subject)),
        Type = "Dataset",
        Contributor = ifelse(is.null(Contributor), ":unas", as.character(Contributor)),
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
    )
  }
}
