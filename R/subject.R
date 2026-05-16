#' @title Create, add, or retrieve a subject
#'
#' @description
#' Manage the subject metadata of a dataset. The subject can be stored as a
#' simple character term or as a structured object with subproperties created by
#' [subject_create()].
#'
#' @details
#' The subject property records what the dataset is about.
#' The [DataCite subject property](https://schema.datacite.org/meta/kernel-4/)
#' allows multiple subproperties, but these cannot be stored directly in a
#' standard [`utils::bibentry`] object.
#' Therefore:
#'
#' * If you set a character string as the subject, it is stored in both the
#'   bibentry and the `"subject"` attribute.
#' * If you set a structured subject (via [subject_create()]), the `$term` value
#'   is stored in the bibentry, and the full object is stored in the `"subject"`
#'   attribute of the [`dataset_df`] object.
#'
#' @param x A dataset object created with [dataset_df()] or [as_dataset_df()].
#' @param term A subject term, for example `"Data sets"`.
#' @param schemeURI URI of the subject identifier scheme, for example
#'   `"http://id.loc.gov/authorities/subjects"`.
#' @param valueURI URI of the subject term, for example
#'   `"https://id.loc.gov/authorities/subjects/sh2018002256"`.
#' @param prefix Abbreviated prefix for a scheme URI, for example `"lcch:"`.
#'   Widely used namespaces (schemes) have conventional abbreviations.
#' @param subjectScheme Name of the subject scheme, classification code, or
#'   authority if one is used. This acts as a namespace.
#' @param classificationCode Classification code for schemes that do not have
#'   `valueURI` entries for each subject term (e.g., ANZSRC).
#' @param value A subject object created by [subject_create()] or a character
#'   string. Used by `subject<-` to replace the subject.
#'
#' @return
#' * `subject(x)` returns:
#'   - a single `"subject"` object if only one is present,
#'   - a list of `"subject"` objects if multiple are present,
#'   - otherwise falls back to the plain string from the bibentry.
#' * `subject(x) <- value` accepts a character vector, a `"subject"` object, or
#'   a list of `"subject"` objects, and updates both the bibentry slot and the
#'   `"subject"` attribute. Returns the dataset invisibly.
#' * `subject_create()` returns a structured `"subject"` object — or a list of
#'   them if multiple terms are provided.
#' * `is.subject(x)` returns `TRUE` if `x` inherits from class `"subject"`.
#'
#' @examples
#' # Set a structured subject
#' subject(orange_df) <- subject_create(
#'   term = "Oranges",
#'   schemeURI = "http://id.loc.gov/authorities/subjects",
#'   valueURI = "http://id.loc.gov/authorities/subjects/sh85095257",
#'   subjectScheme = "LCCH",
#'   prefix = "lcch:"
#' )
#'
#' # Retrieve subject with subproperties
#' subject(orange_df)
#'
#' @family bibliographic helper functions
#' @importFrom assertthat assert_that
#' @export
#' @export
subject <- function(x) {
  assertthat::assert_that(
    is.dataset_df(x),
    msg = "subject(x): x must be a dataset_df object created with dataset_df() or as_dataset_df()."
  )

  subj_attr <- attr(x, "subject", exact = TRUE)
  if (!is.null(subj_attr)) {
    if (is.subject(subj_attr)) {
      return(subj_attr)
    }
    if (is.list(subj_attr) && all(vapply(subj_attr, is.subject, logical(1)))) {
      return(if (length(subj_attr) == 1) subj_attr[[1]] else subj_attr)
    }
    return(subj_attr)
  }

  subj_bib <- get_bibentry(x)$subject
  if (!is.null(subj_bib)) {
    return(subj_bib)
  }

  message("No subject is recorded.")
  NULL
}


#' @rdname subject
#' @export
subject_create <- function(term,
                           schemeURI = NULL,
                           valueURI = NULL,
                           prefix = NULL,
                           subjectScheme = NULL,
                           classificationCode = NULL) {
  if (is.null(term)) term <- ":tba"

  if (length(term) > 1) {
    dataset_subject <- lapply(seq_along(term), function(i) {
      new_Subject(
        term[i],
        subjectScheme = subjectScheme[i],
        schemeURI = schemeURI[i],
        valueURI = if (!is.null(valueURI)) valueURI[i] else NULL,
        classificationCode = if (!is.null(classificationCode)) classificationCode[i] else NULL,
        prefix = prefix[i]
      )
    })
    # don’t force class on the whole list
    return(dataset_subject)
  } else {
    return(new_Subject(
      term = term,
      subjectScheme = subjectScheme,
      schemeURI = schemeURI,
      valueURI = valueURI,
      classificationCode = classificationCode,
      prefix = prefix
    ))
  }
}

#' @inheritParams Subject
#' @keywords internal
new_Subject <- function(term,
                        schemeURI = NULL,
                        valueURI = NULL,
                        prefix = NULL,
                        subjectScheme = NULL,
                        classificationCode = NULL) {
  if (is.null(subjectScheme)) subjectScheme <- ""
  if (is.null(schemeURI)) schemeURI <- ""
  if (is.null(valueURI)) valueURI <- ""
  if (is.null(prefix)) prefix <- ""

  if (!is.null(classificationCode)) {
    dataset_subject <- list(
      term = term,
      subjectScheme = subjectScheme,
      schemeURI = schemeURI,
      classificationCode = classificationCode,
      prefix = prefix
    )
  } else {
    dataset_subject <- list(
      term = term,
      subjectScheme = subjectScheme,
      schemeURI = schemeURI,
      valueURI = valueURI,
      classificationCode = classificationCode,
      prefix = prefix
    )
  }

  class(dataset_subject) <- c("subject", class(dataset_subject))

  dataset_subject
}

#' @rdname subject
#' @export
`subject<-` <- function(x, value) {
  assert_that(
    is.dataset_df(x),
    msg = "subject<-(x, value): x must be a dataset object created with dataset_df() or as_dataset_df()."
  )

  ds_bibentry <- get_bibentry(x)

  # normalize input
  if (is.null(value)) {
    value <- list(new_Subject(term = ":tba"))
  } else if (is.character(value)) {
    value <- lapply(value, new_Subject)
  } else if (is.subject(value)) {
    value <- list(value) # wrap single subject
  } else if (is.list(value) && all(vapply(value, is.subject, logical(1)))) {
    # already a list of subjects, ok
  } else {
    stop("subject(x, value)<- : value must be created with `subject_create()` or be a character string (or list thereof).")
  }

  # flatten terms into bibentry
  ds_bibentry$subject <- vapply(value, function(s) s$term, character(1))
  attr(x, "dataset_bibentry") <- ds_bibentry

  # keep full structured objects
  attr(x, "subject") <- value

  invisible(x)
}

#' @rdname subject
is.subject <- function(x) {
  ifelse(inherits(x, "subject"), TRUE, FALSE)
}

#' @keywords internal
default_subject <- subject_create(
  term = "Data sets",
  subjectScheme = "LCSH",
  schemeURI = "http://id.loc.gov/authorities/subjects",
  valueURI = "http://id.loc.gov/authorities/subjects/sh2018002256",
  prefix = "lcsh:"
)
